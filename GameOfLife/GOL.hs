module GOL where

import Data.List
import Test.QuickCheck
import Debug.Trace

type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Bool


