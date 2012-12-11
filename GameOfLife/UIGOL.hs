module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import GHC.Float
import GOL
import System.Environment

import Data.IORef
example2=[(2::Int,0::Int),(2::Int,1::Int),(2::Int,2::Int)]
example=[(1,0),(2,0),(3,0),(6,0),(7,0),(1,1),(2,1),(4,1),(6,1),(8,1),(9,1),(10,1),(1,2),(2,2),(3,2),(6,2),(7,2),(8,2),(9,2),(10,2),(0,3),(2,3),(4,3),(6,3),(8,3),(10,3),(0,4),(1,4),(2,4),(3,4),(5,4),(7,4),(9,4),(4,5),(5,5),(6,5),(1,6),(3,6),(5,6),(7,6),(8,6),(9,6),(10,6),(0,7),(2,7),(4,7),(6,7),(8,7),(10,7),(0,8),(1,8),(2,8),(3,8),(4,8),(7,8),(8,8),(9,8),(0,9),(1,9),(2,9),(4,9),(6,9),(8,9),(9,9),(3,10),(4,10),(7,10),(8,10),(9,10)]
--need libghc-glib-dev
--export PATH=$PATH:$HOME/.cabal/bin
--dans bash.rc modifier PATH pour persistence
--cabal install gtk2hs-buildtools
--cabal install gtk

------------------------------------------------------------------------

sizeX, sizeY, radius, speed :: Int
sizeX  = 600
sizeY  = 600
radius = 1
speed = 10
------------------------------------------------------------------------

main :: IO ()
main =
  do initGUI

     --get the args
     args <- getArgs

     gridRLE <- case length args of
        0 -> return example
        1 -> readGrid (head args)
        _ -> error "Bad number of arguments !"

     -- create main window
     win <- windowNew
     windowSetTitle win "GOL"
     win `onDestroy` mainQuit

     -- create abstract widget

     cells <- newIORef gridRLE
     baseCells <- newIORef gridRLE
     scale <- newIORef radius

     -- create canvas (drawing area)
     can <- drawingAreaNew
     can `onSizeRequest` return (Requisition sizeX sizeY)
     can `onExpose`      (\_ -> evolveEvent can cells return scale)
     
     -- create Reset button
     clr <- buttonNewWithLabel "Reset"
     clr `onClicked`  evolve can cells (\_ -> readIORef baseCells) scale

     -- create Zoom button
     zoom <- hScaleNewWithRange (fromIntegral radius) 30 (fromIntegral radius)
     adj <- rangeGetAdjustment zoom
     zoom `onRangeValueChanged`  (do newZoom <- adjustmentGetValue adj;updateScale scale (round newZoom))

     -- create Close button     
     cls <- buttonNewWithLabel "Close"
     cls `onClicked` widgetDestroy win
     
     -- create timer; this runs the animation
     timeoutAdd (evolveEvent can cells (return . nextStep) scale) speed
     
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

draw :: DrawingArea -> Cells -> IORef Int -> IO ()
draw can bs scale =
  do dw <- widgetGetDrawWindow can
     drawWindowClear dw
     gc <- gcNew dw
     scal <- readIORef scale
     gcSetValues gc newGCValues{ foreground = black }
     sequence_ [ drawSquare dw gc p scal
               | p <- bs
               ]
 where
  black = Color 0 65535 0 
  drawSquare dw gc (x,y) r =
    drawRectangle dw gc True (x*r) (y*r) r r

evolve :: DrawingArea -> IORef Cells -> (Cells -> IO Cells) -> IORef Int-> IO ()
evolve can cells f scale=
  do bs <- readIORef cells
     bs' <- f bs
     writeIORef cells bs'
     draw can bs' scale

evolveEvent :: DrawingArea -> IORef Cells -> (Cells -> IO Cells) -> IORef Int -> IO Bool
evolveEvent can cells f scale=
  do evolve can cells f scale
     return True

updateScale = writeIORef
--updateScale scale value =
--	writeIORef scale value
------------------------------------------------------------------------

