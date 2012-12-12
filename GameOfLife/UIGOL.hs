{-
    ********************************************************
    *      HASKELL IMPLEMENTATION OF THE GAME OF LIFE      *
    ********************************************************

    Chalmers -- Functional Programming -- Lab Assignment 4
    Michael Fagno && Jonas Bru
    
    The file was run through hlint without warnings 

    You will need the package libghc-glib-dev to run this file
    And then, run in a terminal :
    $> export PATH=$PATH:$HOME/.cabal/bin
    $> cabal install gtk2hs-buildtools
    $> cabal install gtk
    It can be a goo idea to add $HOME/.cabal/bin into bash.rc

    To compile and run :
    $> ghc --make UIGOL.hs
    $> ./UIGOL [File]
    Where file is the path of a RLE Game of Life file
    If you don't provide any file, a test pattern will be loaded
-}

module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import GHC.Float
import GOL
import System.Environment

import Data.IORef
example=[(2::Int,0::Int),(2::Int,1::Int),(2::Int,2::Int)]
example2=[(0,-1),(1,-1),(2,-1)]

------------------------------------------------------------------------

sizeX, sizeY, radius, frameDefaultSpeed, maxframeSpeed :: Int
-- Screen resolution
sizeX  = 600 
sizeY  = 600
-- Square default size
radius = 5
-- Default move distance on the grid
move = 50
-- Default animation speed
frameDefaultSpeed = 300
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
     
     -- evolve function
     let f = (evolveEvent can cells (return . nextStep) scale)

     -- create timer
     _timer <- timeoutAdd (evolveEvent can cells (return . nextStep) scale) frameDefaultSpeed
     timer <-  newIORef _timer

     -- create Reset button
     clr <- buttonNewWithLabel "Reset"
     clr `onClicked`  evolve can cells (\_ -> readIORef baseCells) scale

     -- create speed button
     stp <- hScaleNewWithRange (fromIntegral 5) (fromIntegral maxframeSpeed) 100
     adjstp <- rangeGetAdjustment stp
     adjustmentSetValue adjstp (fromIntegral frameDefaultSpeed)
     stp `onRangeValueChanged`  (do newSpeed <- adjustmentGetValue adjstp;changeTimer timer (round newSpeed) f)


     -- create Zoom button
     zoom <- hScaleNewWithRange 1 30 1
     adj <- rangeGetAdjustment zoom
     adjustmentSetValue adjstp (fromIntegral radius)
     zoom `onRangeValueChanged`  (do newZoom <- adjustmentGetValue adj;writeIORef scale (round newZoom))

     -- create Close button     
     cls <- buttonNewWithLabel "Close"
     cls `onClicked` widgetDestroy win
     
     -- create moove button
     mvUp <- buttonNewWithLabel "Up"
     mvDown <- buttonNewWithLabel "Down"
     mvLeft <- buttonNewWithLabel "Left"
     mvRight <- buttonNewWithLabel "Right"
     mvUp `onClicked` moveUp cells scale
     mvDown `onClicked` moveDown cells scale
     mvLeft `onClicked` moveLeft cells scale
     mvRight `onClicked` moveRight cells scale

     -- describe layout of all widgets
     buts <- hBoxNew False 5
     containerAdd buts clr
     containerAdd buts zoom
     containerAdd buts cls
     containerAdd buts stp
     
     butsmv <- hBoxNew False 5
     containerAdd butsmv mvUp
     containerAdd butsmv mvDown
     containerAdd butsmv mvLeft
     containerAdd butsmv mvRight

     lay <- vBoxNew False 5
     containerAdd lay can
     containerAdd lay buts
     containerAdd lay butsmv
     
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

-- evolve and draw the new living cells
evolve :: DrawingArea -> IORef Cells -> (Cells -> IO Cells) -> IORef Int-> IO ()
evolve can cells f scale =
  do bs <- readIORef cells
     bs' <- f bs
     writeIORef cells bs'
     draw can bs' scale

-- evolve and draw the new living cells
evolveEvent :: DrawingArea -> IORef Cells -> (Cells -> IO Cells) -> IORef Int -> IO Bool
evolveEvent can cells f scale = do
     evolve can cells f scale
     return True

-- change the simulation speed
changeTimer timer frameSpeed f= do
     htimer <-(readIORef timer)
     timeoutRemove htimer
     _timer <- timeoutAdd f frameSpeed
     writeIORef timer _timer

-- Move functions
moveLeft :: IORef Cells -> IORef Int -> IO ()
moveLeft cells scale = do
    scal <- readIORef scale
    (changeIORef cells (translate (move `div` scal)	 0))

moveRight :: IORef Cells -> IORef Int -> IO ()
moveRight cells scale = do
    scal <- readIORef scale
    (changeIORef cells (translate (-(move `div` scal)) 0))

moveUp :: IORef Cells -> IORef Int -> IO ()
moveUp cells scale = do
    scal <- readIORef scale
    (changeIORef cells (translate 0 (move `div` scal)))

moveDown :: IORef Cells -> IORef Int -> IO ()
moveDown cells scale = do
    scal <- readIORef scale
    (changeIORef cells (translate 0 (-(move `div` scal))))

-- Translate all the cells
translate ::Int -> Int -> Cells -> IO Cells
translate dx dy cells = return ( map (\(x,y) -> (x + dx, y + dy)) cells)

-- Transform the value of an IORef
changeIORef cells f = do
     bs <- readIORef cells
     bs' <- f bs
     writeIORef cells bs'
------------------------------------------------------------------------

