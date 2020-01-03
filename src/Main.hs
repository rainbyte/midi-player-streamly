{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent hiding (yield)
import           Control.Monad
import           Control.Monad.State

import qualified Data.ByteString.Lazy as BL
import           Data.Function (on)
import           Data.List (groupBy)
import           Data.Maybe (catMaybes)
import qualified Data.Text as T

import qualified Sound.MIDI.File as MidiFile
import qualified Sound.MIDI.File.Load as MidiFile.Load
import qualified Sound.MIDI.Parser.Report as MidiParser
import           Sound.MIDI.PortMidi (fromMidiFileRelative)
import qualified Sound.PortMidi as PM

import           Streamly
import qualified Streamly.Prelude as S

import           System.Directory
import           System.Exit


import           HtmlGUI (UICmd, htmlGUI)
import qualified HtmlGUI as UI


type MidiCmd = Either String PM.PMMsg
type MidiTimed = (Rational, [MidiCmd])

data PlayerInput = LoadMidi ![MidiTimed] | PlayPause | Stop | Tick | PProgress

data PlayerEvent = NoEvent | Clock | Midi !MidiTimed | Progress Rational String
  deriving Show

data MidiPlayerStatus
  = Playing ![MidiTimed] ![MidiTimed]
  | Paused  ![MidiTimed] ![MidiTimed]
  | Stopped ![MidiTimed]

midiFromPath :: String -> IO (Maybe MidiFile.T)
midiFromPath path = do
  putStrLn $ "Opening " ++ path
  exists <- doesFileExist path
  if exists then do
    file <- BL.readFile path
    let mMidifile = MidiFile.Load.maybeFromByteString file
    case MidiParser.result mMidifile of
      Left err -> do
        putStrLn $ "Error " ++ err
        putStrLn "Try again..."
        pure Nothing
      Right midifile -> do
        putStrLn $ "Loading " ++ path
        pure $ Just midifile
  else do
    putStrLn $ "File " ++ path ++ " does not exist"
    putStrLn "Try again..."
    pure Nothing

outputHandler
  :: MonadAsync m
  => MVar () -> MVar Int -> [PM.PMStream]
  -> PlayerEvent -> SerialT m ()
outputHandler progressSigVar streamIdxVar streams ev =
  liftIO $ case ev of
    Midi (_, cmds) -> do
      streamIdx <- readMVar streamIdxVar
      let stream = streams !! streamIdx
      mapM_ (handleMidiCmd stream) cmds
      putMVar progressSigVar ()
    _ -> pure ()
  where
    handleMidiCmd stream (Right msg) = do
      eErr <- PM.writeShort stream (PM.PMEvent (PM.encodeMsg msg) 0)
      case eErr of
        Right _  -> pure ()
        Left err -> putStrLn $ "Error: " ++ show err
    handleMidiCmd _ (Left str) = putStrLn ("Output: " ++ str)

progressHandler
  :: MonadAsync m
  => MVar UI.UIStatus -> PlayerEvent -> SerialT m ()
progressHandler uiStatusVar ev =
  liftIO $ case ev of
    Progress pos msg -> modifyMVar_ uiStatusVar (newUiStatus pos msg)
    _          -> pure ()
  where
    newUiStatus pos msg uiStatus = pure $ uiStatus
      { UI.progress = fromRational pos, UI.display = msg }

clockHandler
  :: MonadAsync m
  => MVar () -> PlayerEvent -> SerialT m PlayerEvent
clockHandler cmdVar ev = do
  liftIO $ case ev of
    Midi (t, _) -> do
      threadDelay (round $ t * (10^(6::Int)))
      putMVar cmdVar ()
    Clock -> putMVar cmdVar ()
    _ -> pure ()
  pure ev

midiPlayerStep :: (PlayerEvent, MidiPlayerStatus) -> PlayerInput -> (PlayerEvent, MidiPlayerStatus)
midiPlayerStep (_, status) cmd =
  case (status, cmd) of
    (_, LoadMidi l) -> (notesOff, Stopped l)
    (Stopped remain        , PlayPause) -> (Clock, Playing remain [])
    (Stopped _             , PProgress) -> (Progress 0 "stopped", status)
    (Paused  []      played, PlayPause) -> (notesOff, Stopped $ reverse played)
    (Paused  remain  played, PlayPause) -> (Clock, Playing remain played)
    (Paused  remain  played, Stop)      -> (notesOff, Stopped (combine remain played))
    (Paused  remain  played, PProgress) -> (Progress (progress remain played) "paused", status)
    (Playing remain  played, PlayPause) -> (notesOff, Paused remain played)
    (Playing remain  played, Stop)      -> (notesOff, Stopped (combine remain played))
    (Playing []      played, Tick)      -> (notesOff, Stopped $ reverse played)
    (Playing (x:xs)  played, Tick)      -> (Midi x, Playing xs (x:played))
    (Playing remain  played, PProgress) -> (Progress (progress remain played) "playing", status)
    _ -> (NoEvent, status)
  where
    calcTime = sum . fmap fst
    progress :: [MidiTimed] -> [MidiTimed] -> Rational
    progress rs ps =
      let ptime = calcTime ps
          rtime = calcTime rs
      in (100 * ptime) / (rtime + ptime)
    combine = foldl (flip (:))
    notesOff :: PlayerEvent
    notesOff = Midi (0, [Right $ PM.PMMsg (0xB0 + n) 0x7B 0 | n <- [0..15]])

midiPlayer
  :: (MonadAsync m)
  => SerialT m PlayerInput -> SerialT m PlayerEvent
midiPlayer = S.map fst . S.scanl' midiPlayerStep (NoEvent, Stopped [])

mpAction :: MonadAsync m => MVar UICmd -> MVar Int -> SerialT m PlayerInput
mpAction cmdVar streamIdxVar = S.repeatM $ liftIO getCmd
  where
  getCmd = do
    ev <- takeMVar cmdVar
    case ev of
      (UI.SelPort idx) -> do
        putStrLn $ "SelPort " ++ show idx
        _ <- swapMVar streamIdxVar idx
        getCmd
      (UI.LoadMidi path) -> openMidiFile path
      UI.PlayPause -> pure PlayPause
      UI.Stop -> pure Stop
  openMidiFile filename = do
    mMidifile <- midiFromPath $ T.unpack filename
    case mMidifile of
      Just midifile -> pure (LoadMidi $ preprocess midifile)
      Nothing -> getCmd
    where
      preprocess = fmap joinTimed . groupByTime . fromMidiFileRelative
      groupByTime :: Eq a => [(a, b)] -> [[(a, b)]]
      groupByTime = groupBy ((==) `on` fst)
      joinTimed :: [(a, b)] -> (a, [b])
      joinTimed []      = error "Should not happen"
      joinTimed l@(x:_) = (fst x, fmap snd l)

mpClock :: MonadAsync m => MVar () -> SerialT m PlayerInput
mpClock = S.repeatM . (*> pure Tick) . liftIO . takeMVar

mpProgress :: MonadAsync m => MVar () -> SerialT m PlayerInput
mpProgress = S.repeatM . (*> pure PProgress) . liftIO . takeMVar

main :: IO ()
main = do
  _ <- PM.initialize
  deviceCount <- PM.countDevices
  putStrLn "Output devices:"
  streams <- fmap catMaybes $ forM [0..deviceCount - 1] $ \deviceId -> do
    info <- PM.getDeviceInfo deviceId
    when (PM.output info) $
      putStrLn $ "  " ++ show deviceId ++ ". " ++ PM.name info
    eStream <- PM.openOutput deviceId 0
    pure $ either (const Nothing) (\stream -> pure (PM.name info, stream)) eStream
  case streams of
    _:_ -> do
      streamIdxVar <- newMVar 0
      cmdVar <- newEmptyMVar
      clkVar <- newEmptyMVar
      progressSigVar <- newEmptyMVar
      uiStatusVar <- newMVar UI.UIStatus { UI.progress = 0, UI.display = "stopped" }

      let outputHandler' :: MonadAsync m => PlayerEvent -> SerialT m ()
          outputHandler' = outputHandler progressSigVar streamIdxVar (fmap snd streams)
          progressHandler' :: MonadAsync m => PlayerEvent -> SerialT m ()
          progressHandler' = progressHandler uiStatusVar
          peHandler :: (MonadIO m, MonadAsync m) => PlayerEvent -> SerialT m ()
          peHandler ev = clockHandler clkVar ev >>= (\x -> outputHandler' x `parallel` progressHandler' x)
          mpActions = mpProgress progressSigVar `parallel` mpClock clkVar `parallel` mpAction cmdVar streamIdxVar
          runMidiPlayer :: IO ()
          runMidiPlayer = S.drain $ midiPlayer mpActions >>= peHandler
      _ <- forkIO runMidiPlayer

      htmlGUI uiStatusVar cmdVar (fmap fst streams)
      exitSuccess
    [] -> do
      _ <- error "Output device not available"
      exitFailure
