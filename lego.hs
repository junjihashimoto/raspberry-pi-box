{-# LANGUAGE FlexibleContexts #-}
import Graphics.Implicit
import Graphics.Implicit.Definitions
import Data.Monoid
import Control.Monad

rect' xyz = rect3R 0 (0,0,0) xyz

base = (8,8,9.6)

dip = cylinder (5/2-0.01) 1.7
hole = cylinder (5/2+0.5) 2
holeR = translate (-3,-3,0) $ rect' (6,6,2)

block n m =
  let dips = do
        x <- [0..(n-1)]
        y <- [0..(m-1)]
        return $ translate (8*x,8*y,0) $ dip
      holes = do
        x <- [0..(n-1)]
        y <- [0..(m-1)]
        return $ translate (8*x,8*y,0) $ holeR
      board = rect' (8*n,8*m,9.6)
  in difference [
       union [
          translate (-4,-4,0) $ board,
          translate (0,0,9.6) $ union dips
          ],
       translate (0,0,-0.01) $ union holes
       ]
{-
moter =
  let shaft = cylinder 1 38
      body = extrude 25 
-}

module' = 1

tooth = 8

pitchCircleDiameter = module' * 8

baseCircleDiameter = pitchCircleDiameter * cos alpha

pitchCircle = pi * pitchCircleDiameter

baseCircle = pi * baseCircleDiameter

alpha = 2 * pi * 20 / 360



involuteCircle r theta = (r*(cos theta + theta * sin theta), r*(sin theta - theta * cos theta))

rotate' :: ℝ2 -> ℝ -> ℝ2
rotate' (x,y) theta = (cos theta * x - sin theta * y, cos theta * y +  sin theta * x)
rotate'' :: [ℝ2] -> ℝ -> [ℝ2] 
rotate'' xys theta = map (\i -> rotate' i theta) xys

yreverse (x,y) = (x,-y)

polyInvoluteCircle r =
  let n = 32
      m = n -- * 4 / 5
      th i = ( pi / tooth) * i / n
      pos t = involuteCircle r t --  (1.15*pi/tooth)
--      rpos t = yreverser (involuteCircle r t) --  (1.15*pi/tooth)
      rpos t = rotate' (yreverse (involuteCircle r t)) (1.15*pi/tooth)
      curve = map (pos.th) [0..m]
      rcurve = map (rpos.th) [0..m]
--      curve' = map (\(x,y) -> (x,-y)) $ reverse $ map (pos.th) [0..m]
  in  [(10,10),(0,0)] <> curve <> (reverse rcurve) -- rotate'' curve' (1.15*pi/tooth) <> curve'


gtooth :: [ℝ2]
gtooth =
  let t = polyInvoluteCircle (baseCircleDiameter / 2)
  in concat $ map (\i -> rotate'' t (2*pi*i/tooth)) [0..1]
      
gear = union [
--  cylinder (baseCircleDiameter / 2) 1,
--  cylinder (pitchCircleDiameter / 2) 1,
  extrudeR 0 (polygonR 0 gtooth) 1
  ]
  --map (\i -> rotate3 (0,0,2*pi*i/tooth) gtooth) [0..(tooth-1)]

main = do
  forM_ [0..100] $ \i -> do
    let (x,y) = involuteCircle (baseCircleDiameter/2) 
    print $ show x ++ " " ++ show y
--  writeBinSTL 1 "block-nxm.stl" $ block 4 4
--  writeSCAD3 1 "block-nxm.scad" $ block 4 4
--  writeBinSTL 0.3 "tooth.stl" $ gear
--  writeSCAD3 0 "tooth.scad" $ gear
