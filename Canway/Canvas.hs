module Canway.Canvas where

import Canway.Grid
import Control.Concurrent
import Graphics.Blank

type X = Float
type XOffset = Float
type Y = Float
type YOffset = Float
type Width = Float
type XPadding = Float
type Height = Float
type YPadding = Float
type FontSize = Int

drawButton :: X -> Y -> Width -> Height -> FontSize -> String -> Canvas ()
drawButton x y w h fs capt = do
    beginPath()
    fillStyle("#C6FF87")
    fillRect(x, y, w, h)
    fillStyle("black")
    font(show fs ++ "px Arial")
    fillText(capt, x + fromIntegral fs*1.2, y + fromIntegral fs*1.2)
    closePath()

drawLines :: Width -> XOffset -> Height -> YOffset -> Width -> XPadding -> Height -> YPadding -> Canvas ()
drawLines sw sx sh sy cw cwd ch chd  = do
    mapM_ (\ y -> drawLine sx y (sw+cwd) chd) [sy,(ch+sy)..(sh+sy)]
    mapM_ (\ x -> drawLine x sy cwd (sh+chd)) [sx,(cw+sx)..(sw+sx)]

drawLine :: X -> Y -> Width -> Height -> Canvas ()
drawLine x y w h = do
    beginPath()
    fillStyle("#808080")
    fillRect(x, y, w, h)
    fill()
    closePath()

clearCell :: X -> XOffset -> Y -> YOffset -> Width -> XPadding -> Height -> YPadding -> Cell -> Bool -> Canvas ()
clearCell x dx y dy cw cwd ch chd c heatSeeking = if (not heatSeeking || c == Alive)
                                                  then clearRect(dx + cwd + cw*x, dy + chd + ch*y, cw-cwd, ch-chd)
                                                  else return ()

drawCell :: X -> XOffset -> Y -> YOffset -> Width -> XPadding -> Height -> YPadding -> Cell -> Canvas ()
drawCell x dx y dy cw cwd ch chd c = if c == Alive then do
    beginPath()
    fillStyle("black")
    fillRect(dx + cwd + cw*x, dy + chd + ch*y, cw-cwd, ch-chd)
    fill()
    closePath()
                                     else return ()

clearGrid :: XOffset -> YOffset -> Width -> XPadding -> Height -> YPadding -> Bool -> Grid -> Canvas ()
clearGrid sx sy cw cwd ch chd heatSeeking g = imapGridM_ (\ x y c -> clearCell (fromIntegral x) sx (fromIntegral y) sy cw cwd ch chd c heatSeeking) g

drawGrid :: Width -> XOffset -> Height -> YOffset -> Width -> XPadding -> Height -> YPadding -> Grid -> Canvas ()
drawGrid sw sx sh sy cw cwd ch chd g = imapGridM_ (\ x y c -> drawCell (fromIntegral x) sx (fromIntegral y) sy cw cwd ch chd c) g

chooseGrid :: Int -> Int -> Context -> IO ()
chooseGrid x y context = do
    let grid = fillGrid x y Dead
    (w, h) <- send context size -- The canvas width/height
    let (sw, sh) = (w, h*0.9) -- The actual grid width/height. The height is reduced to make room for captions.
    let (sx, sy) = ((w - sw)/2, 0.0) -- The grid x-/y-offset. The grid is centered horizontally.
    let (gw, gh) = dimens grid -- The number of cells in a row/column
    let (cw, ch) = (sw/fromIntegral gw, sh/fromIntegral gh) -- The width/height of a single cell
    let (cwd, chd) = (cw*0.10, ch*0.10) -- The width/height of a grid line
    let (bx, by, bw, bh) = (w*0.44, h*0.92, 100, 50) -- Button size
    let fs = 16 -- Button font size
    let capt = "Animate" -- Button caption

    send context $ drawLines sw sx sh sy cw cwd ch chd
    send context $ drawButton bx by bw bh fs capt
    
    let getRectCoord :: Ord a => a -> a -> a -> Maybe a
        getRectCoord start end coord = if start <= coord && coord < end then Just coord else Nothing
    let pointToCorner :: (Int, Int) -> Maybe (Int, Int)
        pointToCorner (x, y) = do
        let nx = floor $ (fromIntegral x - sx) / cw
        let ny = floor $ (fromIntegral y - sy) / ch
        x' <- getRectCoord 0 gw nx
        y' <- getRectCoord 0 gh ny
        Just (x',y')
        
    let eventHandler :: Grid -> IO ()
        eventHandler g = do
        send context $ clearGrid sx sy cw cwd ch chd False g
        send context $ drawGrid sw sx sh sy cw cwd ch chd g
        
        event <- send context $ readEvent MouseDown
        case jsMouse event of
             Nothing -> eventHandler g
             Just (x', y') -> case pointToCorner (x', y') of
                                   Nothing -> let button = do
                                                      bx' <- getRectCoord bx (bx+bw) (fromIntegral x')
                                                      by' <- getRectCoord by (by+bh) (fromIntegral y')
                                                      Just (bx', by')
                                              in case button of
                                                      Nothing -> eventHandler g
                                                      Just _ -> do
                                                            send context $ clearRect(bx, by, bw, bh)
                                                            animateGrid g context
                                   Just (ex, ey) -> case updateToggle g ex ey of
                                                         Nothing -> eventHandler g
                                                         Just g' -> eventHandler g'
    
    eventHandler grid

animateGrid :: Grid -> Context -> IO ()
animateGrid grid context = do
    (w, h) <- send context size -- The canvas width/height
    let (sw, sh) = (w, h*0.9) -- The actual grid width/height. The height is reduced to make room for captions.
    let (sx, sy) = ((w - sw)/2, 0.0) -- The grid x-/y-offset. The grid is centered horizontally.
    let (gw, gh) = dimens grid -- The number of cells in a row/column
    let (cw, ch) = (sw/fromIntegral gw, sh/fromIntegral gh) -- The width/height of a single cell
    let (cwd, chd) = (cw*0.10, ch*0.10) -- The width/height of a grid line
    
    send context $ drawLines sw sx sh sy cw cwd ch chd
    
    let animateGridCells :: Grid -> IO ()
        animateGridCells g = do
        send context $ drawGrid sw sx sh sy cw cwd ch chd g
        threadDelay $ 1 * 1000 * 50
        send context $ clearGrid sx sy cw cwd ch chd True g
        animateGridCells $ tick g
    
    animateGridCells grid

main :: IO ()
--main = blankCanvas 3000 $ chooseGrid 15 15
main = blankCanvas 3000 $ animateGrid infinite2Big