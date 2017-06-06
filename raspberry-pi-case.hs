{-# LANGUAGE FlexibleContexts #-}
import Graphics.Implicit
import Data.Monoid

-- 65 x 56 x 17 mm

rect' xyz = rect3R 0 (0,0,0) xyz

s = (sum [469/65,402/56])/2

-- 469 x 402
board = translate (0,0,0.1) $ rect' (469/s,402/s,2.2)

-- 469 x 402
cover' = translate (-2,-2,0.2) $ rect' (469/s+4,402/s+4,10)
cover'' = difference [
  translate (-2,-2,0.1) (rect' (469/s+4,402/s+4,4)) ,
  translate (-1,-1,0) (rect' (469/s+2,402/s+2,2)) ,
  rect' (469/s,402/s,6)
  ]
cover  = union [
  difference [cover', modelAPlus] ,
  translate (0,0,-3) cover''
  ]

futa = translate (-1,-1,0) $ rect' (469/s+2,402/s+2,2)

-- 167 x 135 - 94 x 91
cpu = translate ((469-167-94)/s,135/s,2) $ rect' (94/s,91/s,1)

-- 384 x 125 - 106 x 97 -> (469-384-106) x 125 - 106 x 96
usb = translate ((469-384-106)/s,125/s,2) $ rect' (106/s,97/s,8)

-- 171 x 323 - 116 x 93
hdmi = translate ((469-171-116)/s,323/s,2) $ rect' (116/s,93/s,7)

-- 50 x 1 - 373 x 45
dip =
  translate ((469-50-373)/s,0,2) $ rect' (373/s,45/s,9)
  --translate ((469-50-373)/s,0,2) $ rect' (373/s,45/s,20)

-- 361 x 307 - 49 x 111
audio = translate ((469-361-49)/s,307/s,2) $ rect' (49/s,111/s,6)

-- 46 x 363 - 58 x 48
miniusb = translate ((469-46-58)/s,363/s,2) $ rect' (58/s,48/s,3)

-- 1 x 160 - 20 x 79
sd = translate ((469-1)/s-15,160/s,-1) $ rect' (18,49/s,1)

-- 17 x 17 -  21 x 21
hole = union $ [
  translate (17/s,17/s,-1) $ cylinder (19/(2*s)) 4,
  translate (469/s-17/s,17/s,-1) $ cylinder (19/(2*s)) 4,
  translate (17/s,402/s-17/s,-1) $ cylinder (19/(2*s)) 4,
  translate (469/s-17/s,402/s-17/s,-1) $ cylinder (19/(2*s)) 4
  ]

peripheral = 
  union  [usb,
          hdmi,
          dip,
          audio,
          miniusb,
          sd,
          cpu
         ]

modelAPlus = difference [
  union  [board,
          peripheral
         ],
  hole]

case' = translate (-1,-1,-3) $ rect' (469/s+2,402/s+2,20)


extrude3d vec num obj = union $ loop (0,0,0) 0 []
  where
    loop vec' n objs | n >= num = objs
                     | otherwise = loop (vec'+vec) (succ n) $ translate vec' obj : objs

  

main = do
  writeBinSTL 1 "raspberry-pi-board.stl" $ modelAPlus
  writeBinSTL 1 "raspberry-pi-cover1.stl" $ cover
  writeBinSTL 1 "raspberry-pi-cover2.stl" $ 
    union [
       difference [
          extrude3d (0,0,-0.5) 10 peripheral,
          peripheral,
          extrude3d (0,0,-0.5) 10 (translate (0,0,-2) board)
          ],
       futa
       ]
