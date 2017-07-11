{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
import System.IO
import Control.Monad
import Data.Complex
import Data.List
import Data.ByteString (ByteString)

data Obj2D = Obj2D [String]
data Obj3D = Obj3D [String]

class Render2D a where
  render2d :: a -> [Complex Double]

class Render3D a where
  render3d :: a -> Obj3D

data Gear = Gear {
  gAlpha :: Double
, gTooth :: Double
, gModule :: Double
, gWidth :: Double
} deriving Show

data Bar = Bar {
  bWidth :: Double
} deriving Show

involute :: Double -> Double -> Complex Double
involute r theta = (r*(cos theta + theta * sin theta)) :+ (r*(sin theta - theta * cos theta))

yrev :: Complex Double -> Complex Double
yrev (a:+b) = (a:+(-b))

alpha :: Gear -> Double
alpha Gear{..} = 2 * pi * gAlpha / 360

radius :: Gear -> Double
radius g@Gear{..} = (cos (alpha g)) * radius' g

radius' :: Gear -> Double
radius' Gear{..} = gModule * gTooth / 2

instance Render2D Gear where
  render2d g@Gear{..} =
    let dat :: [Complex Double]
        dat = ((radius g) - 0.5) :+ 0 : map (\i-> involute (radius g) (i*2.0*pi/100.0))  [0..10]
        dat''' :: [Complex Double]
        dat''' = map (\i-> mkPolar (radius g) (i*2.0*pi/100.0))  [0..100]
        gear''  =  concat $
                   flip map [0..(gTooth-1)] $
                   \t -> map (\i -> i * (cis (t*2*pi/gTooth) )) $
                   dat ++
                   map (\t -> t * cis (0.9*pi/gTooth))
                   (reverse (map yrev dat))
    in gear''


instance Render3D Gear where
  render3d g@Gear{..} =
    let gear2d = render2d g
        gear2d' = intercalate "," $ map (\(r:+i) -> "[" ++ show r ++ "," ++ show i ++ "]") gear2d
    in Obj3D ["linear_extrude(height = " ++ show gWidth ++ ") { polygon( points= [",
              gear2d',
              "] );}"]

writePlot :: String -> [Complex Double] -> IO ()
writePlot file dat = do
  withFile file WriteMode $ \h -> do
    forM_ dat $ \(r:+i) -> do
      hPutStrLn h $ show r ++ " " ++ show i

write3DPlot :: String -> Obj3D -> IO ()
write3DPlot file (Obj3D dat) = do
  withFile file WriteMode $ \h -> do
    forM_ dat $ \str -> do
      hPutStr h $ str

main = do
  writePlot "involute.dat" $ render2d $ Gear 20 8 1 6
  write3DPlot "involute.scad" $ render3d $ Gear 20 8 1 6
