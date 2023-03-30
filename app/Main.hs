{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import UI.NCurses

import qualified Data.Text as T
import Data.Text (Text)

import Data.Maybe (fromMaybe)

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
        handler <- handleEvent window handlers 10
        handler

    handlers = [
         (Just $ EventSpecialKey $ KeyFunction 2, \_ -> notImplemented window >> loop)
       , (Just $ EventSpecialKey $ KeyFunction 12, \_ -> restart >> loop)
       , (Just $ EventSpecialKey $ KeyFunction 10, \_ -> return ())
       , (Just EventResized, \_ -> loop)
       , (Nothing, \_ -> loop)
       ]

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
        memGiB <- liftIO (decimalTrunc 2 . show <$> getMemoryGibibytes)
        cpuNames <- liftIO getCpuNames
        interfaces <- liftIO (filter (not.isLoopback) <$> getInterfaces)
        productName <- liftIO getProductName

        let productText = case productName of
                            Nothing -> [""]
                            Just name -> ["", name, ""]

        issue <- liftIO $ maybeReadText "/etc/welcome"
        let issueText = case issue of
                           Nothing -> []
                           Just text -> formatIssue text interfaces ++ [""]

        -- Format information into human readable text
        let addrText = enumerateAddresses interfaces
            memText = [T.pack $ memGiB ++ " GiB Memory"]

        -- Format information into lines
        let infoLines = [uname] ++ productText ++ cpuNames ++ memText
            addrLines = issueText ++ map (\i -> fst i <> ": " <> snd i) addrText

        -- Background colours (ident numbers are arbitrary)
        normalColour <- newColorID ColorWhite ColorBlack 2
        accentColour <- newColorID ColorBlack ColorYellow 3
        accentColour' <- newColorID ColorWhite ColorYellow 4

        (height, width) <- screenSize
        let halfHeight = height // 2

        topWindow <- newWindow halfHeight width 0 0
        botWindow <- newWindow (height - halfHeight) width halfHeight 0

        updateWindow topWindow $ do
            (height, width) <- windowSize

            setColor normalColour
            fillBg width [0..height-1]
            --drawBorder Nothing Nothing Nothing (Just blank) Nothing Nothing verti verti

            printText baseWidth baseHeight infoLines

        updateWindow botWindow $ do
            (height, width) <- windowSize

            setColor accentColour'
            fillBg width [0..height-1]

            setColor accentColour
            printText baseWidth 1 addrLines

            moveCursor (height-2) bottomOffset
            drawString "<F2> Settings"

            let powerstr = "<F12> Restart"
            moveCursor (height-2) (width - ((toInteger $ length powerstr) + bottomOffset))
            drawString powerstr

        updateWindow window $ do
            drawBox Nothing Nothing
            overlay botWindow OverlayReplace
            overlay topWindow OverlayReplace

        render

    drawGlyphAt y x glyph = moveCursor y x >> drawGlyph glyph

    blank = Glyph ' ' []
    verti = Just glyphLineV
    horiz = Just glyphLineH

    formatIssue :: Text -> [Interface] -> [Text]
    formatIssue text interfaces = do
        let address = fromMaybe "<No Address>" $ getFirstAddress interfaces
            address' = fst . T.break ('/'==) $ address
        T.lines $ T.replace "%a" address' text
      where
        getFirstAddress [] = Nothing
        getFirstAddress ((Interface _ []):next) = getFirstAddress next
        getFirstAddress ((Interface _ (addr:_)):_) = Just addr

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

drawSplitBox :: ColorID -> ColorID -> Update ()
drawSplitBox topColour bottomColour = do
    (height, width) <- windowSize
    let halfHeight = height // 2

    setColor bottomColour
    drawBox Nothing Nothing
    setColor topColour
    drawLineH_ 0 0 width
    drawLineV_ 0 1 (halfHeight-1)
    drawLineV_ (width-1) 1 (halfHeight-1)
    drawGlyphAt 0 0 glyphCornerUL
    drawGlyphAt 0 (width-1) glyphCornerUR
  where
    drawGlyphAt y x glyph = moveCursor y x >> drawGlyph glyph
    drawLineH_ y x x' = moveCursor y x >> drawLineH Nothing x'
    drawLineV_ x y y' = moveCursor y x >> drawLineV Nothing y'
    blank = Just $ Glyph ' ' []


handleEvent :: Window -> [(Maybe Event, Event -> Curses ())] -> Integer -> Curses (Curses ())
handleEvent window handlers timeout = loop where
    loop = do
        ev <- getEvent window (Just timeout)
        case lookup ev handlers of
            Nothing -> loop
            Just handler -> return $ handler $ fromMaybe (EventUnknown 0) ev

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop


(//) = divInteger

decimalTrunc :: Int -> String -> String
decimalTrunc digits = (\(x,y) -> x ++ take (digits+1) y) . break ('.'==)
