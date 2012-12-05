module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import GHC.Float
import GOL

import Data.IORef
example=[[False,False, False],[True,True, True],[False,False, False]]
--need libghc-glib-dev
--export PATH=$PATH:$HOME/.cabal/bin
--dans bash.rc modifier PATH pour persistence
--cabal install gtk2hs-buildtools
--cabal install gtk

------------------------------------------------------------------------

sizeX, sizeY, radius, speed :: Int
sizeX  = 600
sizeY  = 600
radius = 10
speed = 500
------------------------------------------------------------------------

main :: IO ()
main =
  do initGUI

     -- create main window
     win <- windowNew
     windowSetTitle win "GOL"
     win `onDestroy` mainQuit
     
     -- create abstract widget
     grid <- newIORef example
     baseGrid <- newIORef example
     scale <- newIORef radius

     -- create canvas (drawing area)
     can <- drawingAreaNew
     can `onSizeRequest` return (Requisition sizeX sizeY)
     can `onExpose`      (\_ -> evolveEvent can grid return scale)
     
     -- create Reset button
     clr <- buttonNewWithLabel "Reset"
     clr `onClicked`  evolve can grid (\_ -> (do bs <- readIORef baseGrid;return bs)) scale

     -- create Zoom button
     zoom <- hScaleNewWithRange 10 200 (fromIntegral radius)
     adj <- rangeGetAdjustment zoom
     zoom `onRangeValueChanged`  (do newZoom <- adjustmentGetValue adj;updateScale scale (round newZoom))

     -- create Close button     
     cls <- buttonNewWithLabel "Close"
     cls `onClicked` widgetDestroy win
     
     -- create timer; this runs the animation
     timeoutAdd (evolveEvent can grid (\g -> return (nextStep g)) scale) speed
     
     -- describe layout of all widgets
     buts <- hBoxNew False 5
     containerAdd buts clr
     containerAdd buts zoom
     containerAdd buts cls
     
     lay <- vBoxNew False 5
     containerAdd lay can
     containerAdd lay buts
     
     containerAdd win lay
     widgetShowAll win
     mainGUI

------------------------------------------------------------------------

draw :: DrawingArea -> Grid -> IORef Int -> IO ()
draw can bs scale =
  do dw <- widgetGetDrawWindow can
     drawWindowClear dw
     gc <- gcNew dw
     scal <- readIORef scale
     gcSetValues gc newGCValues{ foreground = black }
     sequence_ [ drawSquare dw gc p scal
               | p <- (getLivingsCoord bs)
               ]
 where
  black = Color 0 65535 0 
  drawSquare dw gc (x,y) r =
    drawRectangle dw gc True (x*r) (y*r) r r

evolve :: DrawingArea -> IORef Grid -> (Grid -> IO Grid) -> IORef Int-> IO ()
evolve can grid f scale=
  do bs <- readIORef grid
     bs' <- f bs
     writeIORef grid bs'
     draw can bs' scale

evolveEvent :: DrawingArea -> IORef Grid -> (Grid -> IO Grid) -> IORef Int -> IO Bool
evolveEvent can grid f scale=
  do evolve can grid f scale
     return True

updateScale scale value =
	do writeIORef scale value
------------------------------------------------------------------------

