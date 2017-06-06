{-# LANGUAGE FlexibleContexts #-}
import Graphics.Implicit
import Data.Monoid

rect' xyz = rect3R 0 (0,0,0) xyz

base = (8,8,9.6)

dip = cylinder (5/2-0.01) 1.7
hole = cylinder (5/2) 2

block n m =
  let dips = do
        x <- [0..(n-1)]
        y <- [0..(m-1)]
        return $ translate (8*x,8*y,0) $ dip
      holes = do
        x <- [0..(n-1)]
        y <- [0..(m-1)]
        return $ translate (8*x,8*y,0) $ hole
      board = rect' (8*n,8*m,9.6)
  in difference [
       union [
          translate (-4,-4,0) $ board,
          translate (0,0,9.6) $ union dips
          ],
       translate (0,0,-0.01) $ union holes
       ]

main = do
  writeBinSTL 1 "block-nxm.stl" $ block 4 4
