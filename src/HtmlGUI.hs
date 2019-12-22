{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module HtmlGUI where

import           Control.Concurrent
import           Control.Monad

import qualified Data.Text as T
import qualified Data.Text.Read as TR
import           Data.Time.Clock

import qualified Graphics.UI.Webviewhs as WHS
import           Language.Javascript.JMacro

import           System.Directory


data UICmd = SelPort Int | LoadMidi T.Text | PlayPause | Stop
  deriving Show

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
    indexed = zip ([0..]::[Int])
    addPort idx name = WHS.runJavaScript window
      [jmacro| portAdd(`idx`, `name`); |]
  windowCleanup _ = pure ()
  windowCallback window msg = case msg of
    "load"      -> WHS.withWindowOpenDialog
      window "Open MIDI file" False (enqueueCmd . LoadMidi)
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
  enqueueCmd = void . forkIO . putMVar cmdVar
