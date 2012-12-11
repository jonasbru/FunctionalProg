module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import GHC.Float
import GOL
import System.Environment

import Data.IORef
example=[(2::Int,0::Int),(2::Int,1::Int),(2::Int,2::Int)]
--need libghc-glib-dev
--export PATH=$PATH:$HOME/.cabal/bin
--dans bash.rc modifier PATH pour persistence
--cabal install gtk2hs-buildtools
--cabal install gtk

------------------------------------------------------------------------

sizeX, sizeY, radius, frameSpeed, maxframeSpeed :: Int
sizeX  = 600
sizeY  = 600
radius = 1
frameSpeed = 300
maxframeSpeed = 1000
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

     -- create Ref

     cells <- newIORef gridRLE
     baseCells <- newIORef gridRLE
     scale <- newIORef radius

     -- create canvas (drawing area)
     can <- drawingAreaNew
     can `onSizeRequest` return (Requisition sizeX sizeY)
     can `onExpose`      (\_ -> evolveEvent can cells return scale)
     
     -- main function
     f <-return (evolveEvent can cells (return . nextStep) scale)

     -- create timer; this runs the animation
     _timer <- timeoutAdd (evolveEvent can cells (return . nextStep) scale) frameSpeed
     timer <-  newIORef (_timer)

     -- create Reset button
     clr <- buttonNewWithLabel "Reset"
     clr `onClicked`  evolve can cells (\_ -> readIORef baseCells) scale

     -- create speed button
     stp <- hScaleNewWithRange (fromIntegral 5) (fromIntegral maxframeSpeed) (100)
     adjstp <- rangeGetAdjustment stp
     adjustmentSetValue adjstp (fromIntegral frameSpeed)
     stp `onRangeValueChanged`  (do newSpeed <- adjustmentGetValue adjstp;changeTimer timer (round newSpeed) f)


     -- create Zoom button
     zoom <- hScaleNewWithRange (fromIntegral radius) 30 (fromIntegral radius)
     adj <- rangeGetAdjustment zoom
     zoom `onRangeValueChanged`  (do newZoom <- adjustmentGetValue adj;writeIORef scale (round newZoom))

     -- create Close button     
     cls <- buttonNewWithLabel "Close"
     cls `onClicked` widgetDestroy win
     
     

     -- describe layout of all widgets
     buts <- hBoxNew False 5
     containerAdd buts clr
     containerAdd buts zoom
     containerAdd buts cls
     containerAdd buts stp
     
     lay <- vBoxNew False 5
     containerAdd lay can
     containerAdd lay buts
     
     containerAdd win lay
     widgetShowAll win
     mainGUI

------------------------------------------------------------------------

-- Draw square for living cells
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
    drawRectangle dw gc True (x*r + r `div` 5) (y*r + r `div` 5) (r - r `div` 5 ) (r - r `div` 5)

evolve :: DrawingArea -> IORef Cells -> (Cells -> IO Cells) -> IORef Int-> IO ()
evolve can cells f scale =
  do bs <- readIORef cells
     bs' <- f bs
     writeIORef cells bs'
     draw can bs' scale

evolveEvent :: DrawingArea -> IORef Cells -> (Cells -> IO Cells) -> IORef Int -> IO Bool
evolveEvent can cells f scale = do
     evolve can cells f scale
     return True

changeTimer timer frameSpeed f= do
     htimer <-(readIORef timer)
     timeoutRemove (htimer)
     _timer <- timeoutAdd f frameSpeed
     writeIORef timer _timer

------------------------------------------------------------------------

