{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Control.Concurrent hiding (yield)
import           Control.Monad

import qualified Data.ByteString.Lazy as BL
import           Data.Functor (($>))
import           Data.List (groupBy)
import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import           Data.Time.Clock

import qualified Graphics.UI.Webviewhs as WHS
import           Language.Javascript.JMacro

import           Pipes
import           Pipes.Concurrent

import qualified Sound.MIDI.File as MidiFile
import qualified Sound.MIDI.File.Load as MidiFile.Load
import qualified Sound.MIDI.Parser.Report as MidiParser
import           Sound.MIDI.PortMidi (fromMidiFileRelative)
import qualified Sound.PortMidi as PM

import           System.Directory
import           System.Exit


type MidiCmd = Either String PM.PMMsg
type MidiTimed = (Rational, MidiCmd)

data UICmd = SelPort Int | LoadMidi ![MidiTimed] | PlayPause | Stop | Tick
  deriving Show

newtype ClockCmd = ClockWait Rational
  deriving Show

data PlayerEvent = NoEvent | Clock !ClockCmd | Midi !MidiCmd
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

outputHandler :: MVar Int -> [PM.PMStream] -> Consumer PlayerEvent IO ()
outputHandler streamIdxVar streams = forever $ do
  ev <- await
  lift $ case ev of
    Midi (Right msg) -> do
      streamIdx <- readMVar streamIdxVar
      let stream = streams !! streamIdx
      eErr <- PM.writeShort stream (PM.PMEvent (PM.encodeMsg msg) 0)
      case eErr of
        Right _  -> pure ()
        Left err -> putStrLn $ "Error: " ++ show err
    Midi (Left str) -> putStrLn ("Output: " ++ str)
    _ -> pure ()

clockHandler :: Pipe PlayerEvent UICmd IO ()
clockHandler = forever $ do
  ev <- await
  case ev of
    Clock (ClockWait t) -> do
      lift $ threadDelay (round $ t * (10^(6::Int)))
      yield Tick
    _ -> pure ()

data PlayerStatus = PlayPauseStatus
  { midiPlayed :: ![MidiCmd]
  , midiRemain :: ![MidiCmd]
  }
data MidiPlayerStatus
  = Playing ![[MidiTimed]] ![[MidiTimed]]
  | Paused  ![[MidiTimed]] ![[MidiTimed]]
  | Stopped ![[MidiTimed]]

handleInput :: UICmd -> MidiPlayerStatus -> ([PlayerEvent], MidiPlayerStatus)
handleInput ev status =
  case (status, ev) of
    (_, LoadMidi l) -> (notesOff, Stopped (groupBy (\a b -> fst a == fst b) l))
    (Stopped l@((x:_):_)     , PlayPause) -> ([Clock $ ClockWait (fst x)], Playing l [])
    (Paused  []     played, PlayPause) -> ([NoEvent], Stopped played)
    (Paused  l@((x:_):_) played, PlayPause) -> ([Clock $ ClockWait (fst x)], Playing l played)
    (Paused  remain played, Stop)      -> ([NoEvent], Stopped (combine remain played))
    (Playing remain played, PlayPause) -> (notesOff, Paused remain played)
    (Playing remain played, Stop)      -> (notesOff, Stopped (combine remain played))
    (Playing []     played, Tick)      -> (notesOff, Stopped played)
    (Playing (xs:xss) played, Tick) ->
      let midiEvents = map (Midi . snd) xs
          played' = xs:played
          nextState l = case l of
            []        -> (                 midiEvents ++ notesOff, Stopped $ reverse played')
            ([]:yss)  -> nextState yss
            ((y:_):_) -> ( (Clock $ ClockWait (fst y)) : midiEvents, Playing xss played')
      in nextState xss
    _ -> ([NoEvent], status)
  where
    combine = foldl (flip (:))
    notesOff :: [PlayerEvent]
    notesOff = [Midi $ Right $ PM.PMMsg (0xB0 + n) 0x7B 0 | n <- [0..15]]

midiPlayer :: Monad m => Pipe UICmd PlayerEvent m ()
midiPlayer = loop (Stopped [])
  where
    loop status = do
      ev <- await
      let (foo, status') = handleInput ev status
      forM_ foo yield
      loop status'

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

      (outputPlayer, inputPlayer) <- spawn unbounded
      (outputClock, inputClock) <- spawn unbounded
      (outputMidi, inputMidi) <- spawn unbounded

      _ <- forkIO $ runEffect $
        fromInput inputPlayer >-> midiPlayer >-> toOutput (outputClock <> outputMidi)
      _ <- forkIO $ runEffect $
        fromInput inputClock >-> clockHandler >-> toOutput outputPlayer
      _ <- forkIO $ runEffect $
        fromInput inputMidi >-> outputHandler streamIdxVar (fmap snd streams)

      _ <- forkIO $ forever $ do
        ev <- takeMVar cmdVar
        case ev of
          (SelPort idx) -> do
            putStrLn $ "SelPort " ++ show idx
            swapMVar streamIdxVar idx $> ()
          _             -> atomically $ send outputPlayer ev $> ()

      htmlGUI cmdVar (fmap fst streams)
      exitSuccess
    [] -> do
      _ <- error "Output device not available"
      exitFailure
