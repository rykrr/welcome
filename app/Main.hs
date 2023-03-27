{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import UI.NCurses

import qualified Data.Text as T
import Data.Text (Text)

import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Process
import Debug.Trace

import SystemInfo

import GHC.Integer (divInteger)
import GHC.Num.Integer (integerFromInt)

import System.Posix.Signals



main :: IO ()
main = do
    _ <- installHandler sigINT (Catch $ return ()) Nothing
    runCurses $ do
        setEcho False
        setCursorMode CursorInvisible
        defaultWindow >>= welcomeScreen


welcomeScreen :: Window -> Curses ()
welcomeScreen window = loop 
  where
    loop = do
        renderWelcomeScreen window
        handler <- handleEvent window handlers
        handler

    handlers = [
        ((EventSpecialKey $ KeyFunction 2), \_ -> notImplemented window >> loop),
        ((EventSpecialKey $ KeyFunction 12), \_ -> restart >> loop),
        --((EventSpecialKey $ KeyFunction 10), \_ -> return ()),
        (EventResized, \_ -> loop)]

    restart :: Curses ()
    restart = liftIO $ readProcess "/sbin/reboot" [] [] >> return ()

    notImplemented :: Window -> Curses ()
    notImplemented window = do
        errorColour <- newColorID ColorBlack ColorRed 4
        updateWindow window $ do
            (height, width) <- windowSize
            let halfHeight = height // 2
                halfWidth = width // 2
                errorMsg = "Not Implemented"
            setColor errorColour
            fillBg width [halfHeight..height-1]
            moveCursor (halfHeight+1) bottomOffset
            drawString errorMsg
        render
        _ <- getEvent window Nothing
        return ()

    renderWelcomeScreen :: Window -> Curses ()
    renderWelcomeScreen window = do
        -- Collect Information
        uname <- liftIO getUname
        memGiB <- liftIO getMemoryGibibytes
        cpuNames <- liftIO getCpuInfo
        interfaces <- liftIO getInterfaces

        -- Format information into human readable text
        let addrText = enumerateAddresses $ filter (not . isLoopback) interfaces
            memText = T.pack $ show (round_ memGiB 3) ++ " GiB Memory"

        -- Format information into lines
        let infoLines = [uname] ++ [T.pack ""] ++ cpuNames ++ [memText]
            addrLines = map (\i -> T.pack (fst i ++ ": " ++ snd i)) addrText

        -- Background colours (ident numbers are arbitrary)
        accentColour <- newColorID ColorBlack ColorYellow 2
        normalColour <- newColorID ColorWhite ColorBlack 3

        updateWindow window $ do
            (height, width) <- windowSize
            let halfHeight = height // 2

            setColor normalColour
            fillBg width [0..halfHeight-1]
            printText baseWidth baseHeight infoLines

            setColor accentColour
            fillBg width [halfHeight..height-1]
            printText baseWidth (halfHeight+1) addrLines

            moveCursor (height-bottomOffset) bottomOffset
            drawString "<F2> Settings"

            let powerstr = "<F12> Restart"
            moveCursor (height-bottomOffset) (width - ((toInteger $ length powerstr) + 2))
            drawString powerstr

        render

    blank = (Glyph ' ' [])

    baseHeight, baseWidth, bottomOffset :: Integer
    baseHeight = 2
    baseWidth = 4
    bottomOffset = 2

    fillBg :: Integer -> [Integer] -> Update ()
    fillBg width range = mapM fillLine range >> return ()
        where fillLine y = moveCursor y 0 >> drawLineH (Just blank) width

    printText :: Integer -> Integer -> [Text] -> Update ()
    printText xOffset yOffset lines = do
        flip mapM (zip [yOffset..] lines) $ \(y, line) -> do
            moveCursor y xOffset
            drawText line
        return ()

handleEvent :: Window -> [(Event, Event -> Curses ())] -> Curses (Curses ())
handleEvent w handlers = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' ->
                case lookup ev' handlers of
                    Nothing -> loop
                    Just handler -> return $ handler ev'

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop


(//) = divInteger

round_ :: Double -> Int -> Double
round_ x n = (fromIntegral . floor $ x * f) / f
  where f = 10^n
