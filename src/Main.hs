{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Control.Concurrent hiding (yield)
import           Control.Monad
import           Control.Monad.State

import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import           Data.Time.Clock

import qualified Graphics.UI.Webviewhs as WHS
import           Language.Javascript.JMacro

import qualified Sound.MIDI.File as MidiFile
import qualified Sound.MIDI.File.Load as MidiFile.Load
import qualified Sound.MIDI.Parser.Report as MidiParser
import           Sound.MIDI.PortMidi (fromMidiFileRelative)
import qualified Sound.PortMidi as PM

import           Streamly
import qualified Streamly.Prelude as S

import           System.Directory
import           System.Exit


type MidiCmd = Either String PM.PMMsg
type MidiTimed = (Rational, MidiCmd)

data UICmd = SelPort Int | LoadMidi ![MidiTimed] | PlayPause | Stop | Tick
  deriving Show

data PlayerEvent = NoEvent | Clock | Midi ![MidiTimed]
  deriving Show

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
  
htmlGUI :: MVar UICmd -> [String] -> IO ()
htmlGUI cmdVar portNames = void $ do
  dir <- getCurrentDirectory
  eitherWindow <- WHS.createWindow (windowParams dir) windowCallback
  case eitherWindow of
    Left  _      -> pure ()
    Right window -> do
      windowSetup window
      windowLoop window
      windowCleanup window
      WHS.terminateWindowLoop window
      WHS.destroyWindow window
  where
  windowLoop :: WHS.Window a -> IO ()
  windowLoop window = do
    timeIni <- getCurrentTime
    shouldContinue  <- WHS.iterateWindowLoop window False
    timeEnd <- getCurrentTime
    let fps = 120
        elapsed = realToFrac (diffUTCTime timeEnd timeIni) :: Double
        toMicros = (* 1000000)
        nextFrame = floor $ toMicros (1/fps - elapsed)
    -- print nextFrame
    when shouldContinue $ do
      when (nextFrame > 0) (threadDelay nextFrame)
      windowLoop window
  windowParams dir = WHS.WindowParams
    { WHS.windowParamsTitle = "midi-player-hs"
    , WHS.windowParamsUri = T.pack $ "file://" ++ dir ++ "/src/Main.html"
    , WHS.windowParamsWidth = 600
    , WHS.windowParamsHeight = 340
    , WHS.windowParamsResizable = False
    , WHS.windowParamsDebuggable = True
    }
  windowSetup window = do
    putStrLn "Initialize GUI"
    -- Add output ports to combobox
    forM_ (indexed portNames) (uncurry addPort)
    where
    indexed = zip ([1..]::[Int])
    addPort idx name = WHS.runJavaScript window
      [jmacro| portAdd(`idx`, `name`); |]
  windowCleanup _ = pure ()
  windowCallback window msg = case msg of
    "load"      -> WHS.withWindowOpenDialog
      window "Open MIDI file" False openMidiFile
    "playpause" -> enqueueCmd PlayPause
    "stop"      -> enqueueCmd Stop
    (parsePort -> Just idx) -> do
      putStrLn $ "port" <> show idx
      enqueueCmd (SelPort idx)
    _ -> pure ()
    where
    parsePort = (textToInt =<<) . T.stripPrefix "port"
    textToInt = rightToMaybe . fmap fst . TR.decimal
    rightToMaybe = either (const Nothing) pure
  openMidiFile filename = do
    mMidifile <- midiFromPath $ T.unpack filename
    case mMidifile of
      Just midifile -> enqueueCmd (LoadMidi $ fromMidiFileRelative midifile)
      Nothing -> pure ()
  enqueueCmd = putMVar cmdVar

outputHandler
  :: MonadAsync m
  => MVar Int -> [PM.PMStream]
  -> PlayerEvent -> SerialT m ()
outputHandler streamIdxVar streams ev =
  liftIO $ case ev of
    Midi cmds -> do
      streamIdx <- readMVar streamIdxVar
      let stream = streams !! streamIdx
      mapM_ (handleMidiCmd stream) cmds
    _ -> pure ()
  where
    handleMidiCmd stream (_, Right msg) = do
      eErr <- PM.writeShort stream (PM.PMEvent (PM.encodeMsg msg) 0)
      case eErr of
        Right _  -> pure ()
        Left err -> putStrLn $ "Error: " ++ show err
    handleMidiCmd _ (_, Left str) = putStrLn ("Output: " ++ str)

clockHandler
  :: MonadAsync m
  => MVar UICmd -> PlayerEvent -> SerialT m PlayerEvent
clockHandler cmdVar ev = do
  liftIO $ case ev of
    Midi (x:_) -> do
      let t = fst x
      threadDelay (round $ t * (10^(6::Int)))
      putMVar cmdVar Tick
    Clock -> putMVar cmdVar Tick
    _ -> pure ()
  pure ev

data MidiPlayerStatus
  = Playing ![MidiTimed] ![MidiTimed]
  | Paused  ![MidiTimed] ![MidiTimed]
  | Stopped ![MidiTimed]

midiPlayerStep :: (PlayerEvent, MidiPlayerStatus) -> UICmd -> (PlayerEvent, MidiPlayerStatus)
midiPlayerStep (_, status) cmd =
  case (status, cmd) of
    (_, LoadMidi l) -> (notesOff, Stopped l)
    (Stopped remain        , PlayPause) -> (Clock, Playing remain [])
    (Paused  []      played, PlayPause) -> (NoEvent, Stopped played)
    (Paused  remain  played, PlayPause) -> (Clock, Playing remain played)
    (Paused  remain  played, Stop)      -> (NoEvent, Stopped (combine remain played))
    (Playing remain  played, PlayPause) -> (notesOff, Paused remain played)
    (Playing remain  played, Stop)      -> (notesOff, Stopped (combine remain played))
    (Playing []      played, Tick)      -> (notesOff, Stopped $ reverse played)
    (Playing l@(x:_) played, Tick)      ->
      let (ps, remain') = span ((== fst x) . fst) l
      in (Midi ps, Playing remain' (reverse ps <> played))
    _ -> (NoEvent, status)
  where
    combine = foldl (flip (:))
    notesOff :: PlayerEvent
    notesOff = Midi [(0, Right $ PM.PMMsg (0xB0 + n) 0x7B 0) | n <- [0..15]]
  
midiPlayer
  :: (MonadAsync m)
  => SerialT m UICmd -> SerialT m PlayerEvent
midiPlayer = S.map fst . S.scanl' midiPlayerStep (NoEvent, Stopped [])

mpAction :: MonadAsync m => MVar UICmd -> MVar Int -> SerialT m UICmd
mpAction cmdVar streamIdxVar = S.repeatM $ liftIO getCmd
  where
  getCmd = do
    ev <- takeMVar cmdVar
    case ev of
      (SelPort idx) -> do
        putStrLn $ "SelPort " ++ show idx
        _ <- swapMVar streamIdxVar idx
        getCmd
      _             -> pure ev

mpClock :: MonadAsync m => MVar UICmd -> SerialT m UICmd
mpClock = S.repeatM . liftIO . takeMVar

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

      let outputHandler' :: MonadAsync m => PlayerEvent -> SerialT m ()
          outputHandler' = outputHandler streamIdxVar (fmap snd streams)
          peHandler :: (MonadIO m, MonadAsync m) => PlayerEvent -> SerialT m ()
          peHandler ev = clockHandler clkVar ev >>= outputHandler'
          mpActions = mpClock clkVar `parallel` mpAction cmdVar streamIdxVar 
          runMidiPlayer :: IO ()
          runMidiPlayer = S.drain $ midiPlayer mpActions >>= peHandler
      _ <- forkIO runMidiPlayer

      htmlGUI cmdVar (fmap fst streams)
      exitSuccess
    [] -> do
      _ <- error "Output device not available"
      exitFailure
