-----------------------------------------------------------------------------
--
-- Module      :  Data.Vec.LinAlg.OpenGL
-- Copyright   :  Vo Minh Thu
-- License     :  BSD3
--
-- Maintainer  :  Vo Minh Thu
-- Stability   :
-- Portability :
--
-- |
-- This module is similar to Data.Vec.LinAlg.Transform3D but provides
-- transforms that match those used in OpenGL.

-- TODO: move the showMatXX functions elsewhere.

module Data.Vec.LinAlg.OpenGL where

import Data.Vec
import Data.Vec.LinAlg.Transform3D (rotationVec)

showMat33 :: Show a => Mat33 a -> String
showMat33 ((a:.b:.c:.()):.
           (e:.f:.g:.()):.
           (i:.j:.k:.()):.()) = unlines
  [ "matFromList"
  , concat ["  [ ", show a, ", ", show b, ", ", show c, ","]
  , concat ["    ", show e, ", ", show f, ", ", show g, ","]
  , concat ["    ", show i, ", ", show j, ", ", show k, "]"]
  ]

showMat44 :: Show a => Mat44 a -> String
showMat44 ((a:.b:.c:.d:.()):.
           (e:.f:.g:.h:.()):.
           (i:.j:.k:.l:.()):.
           (m:.n:.o:.p:.()):.()) = unlines
  [ "matFromList"
  , concat ["  [ ", show a, ", ", show b, ", ", show c, ", ", show d, ","]
  , concat ["    ", show e, ", ", show f, ", ", show g, ", ", show h, ","]
  , concat ["    ", show i, ", ", show j, ", ", show k, ", ", show l, ","]
  , concat ["    ", show m, ", ", show n, ", ", show o, ", ", show p, "]"]
  ]

xAxis :: Mat44 a -> Vec3 a
xAxis
  ((a:._:._:._:.()):.
   (e:._:._:._:.()):.
   (i:._:._:._:.()):.
   (_:._:._:._:.()):.()) = a:.e:.i:.()

yAxis :: Mat44 a -> Vec3 a
yAxis
  ((_:.b:._:._:.()):.
   (_:.f:._:._:.()):.
   (_:.j:._:._:.()):.
   (_:._:._:._:.()):.()) = b:.f:.j:.()

zAxis :: Mat44 a -> Vec3 a
zAxis
  ((_:._:.c:._:.()):.
   (_:._:.g:._:.()):.
   (_:._:.k:._:.()):.
   (_:._:._:._:.()):.()) = c:.g:.k:.()

origin :: Mat44 a -> Vec3 a
origin
  ((_:._:._:.d:.()):.
   (_:._:._:.h:.()):.
   (_:._:._:.l:.()):.
   (_:._:._:._:.()):.()) = d:.h:.l:.()

verticalScreenExtent :: Floating a => a -> a -> a -> (a,a,a,a)
verticalScreenExtent fovy aspect hither = (xmin,xmax,ymin,ymax)
  where
  ymax = hither * tan (fovy * pi / 360)
  ymin = -ymax
  xmax = ymax * aspect
  xmin = -xmax;

-- | Construct a perspective projection matrix
-- from a window (left, right, bottom, top) and
-- near and far plane.

-- frustumm xmin xmax ymin ymax hither yon
-- frustumm left right bottom top near far
frustum ::Fractional a => a -> a -> a -> a -> a -> a -> Mat44 a
frustum l r b t n f = matFromList 
  [ (2*n)/(r-l), 0,          (r+l)/(r-l),  0,
    0,          (2*n)/(t-b), (t+b)/(t-b),  0,
    0,          0,           -(f+n)/(f-n), (-2*f*n)/(f-n),
    0,          0,           -1,           0]


-- | Constructs a view matrix from coordinate
-- vectors (x,y,z) and origin suitable to be
-- loaded in the model-view matrix to place the
-- camera.
viewing' :: Num a => Vec3 a -> Vec3 a -> Vec3 a -> Vec3 a -> Mat44 a
viewing' x@(xx:.xy:.xz:.()) y@(yx:.yy:.yz:.()) z@(zx:.zy:.zz:.()) origin =
  matFromList
    [ xx, xy, xz, negate $ dot x origin,
      yx, yy, yz, negate $ dot y origin,
      zx, zy, zz, negate $ dot z origin,
      0,  0,  0,  1]

viewing :: Num a => Mat44 a -> Mat44 a
viewing ((xx:.yx:.zx:.ox:.()):.
         (xy:.yy:.zy:.oy:.()):.
         (xz:.yz:.zz:.oz:.()):.
         (_:._:._:._:.()):.()) =
  matFromList
    [ xx, xy, xz, negate $ dot x o,
      yx, yy, yz, negate $ dot y o,
      zx, zy, zz, negate $ dot z o,
      0,  0,  0,  1]
  where x = xx:.xy:.xz:.()
        y = yx:.yy:.yz:.()
        z = zx:.zy:.zz:.()
        o = ox:.oy:.oz:.()

-- | Construct an orthographic projection matrix.
ortho :: Fractional a => a -> a -> a -> a -> a -> a -> Mat44 a
ortho l r b t n f = matFromList
  [ 2/(r-l), 0,       0,        -(r+l)/(r-l),
    0,       2/(t-b), 0,        -(t+b)/(t-b),
    0,       0,       -2/(f-n), -(f+n)/(f-n),
    0,       0,       0,        1]

-- | Return the transpose of the inverse of the upper leftmost 3x3
-- matrix of the given 4x4 matrix. This is the so-called normal matrix.

-- TODO normal :: Mat44 a -> Maybe (Mat33 a)
normal ((a:.b:.c:._:.()):.
        (e:.f:.g:._:.()):.
        (i:.j:.k:._:.()):.
        _:.()) = fmap transpose . invert $ matFromList [a,b,c,e,f,g,i,j,k]

translateAlongLocalX :: Num a => a -> Mat44 a -> Mat44 a
translateAlongLocalX x
  ((a:.b:.c:.d:.()):.
   (e:.f:.g:.h:.()):.
   (i:.j:.k:.l:.()):.
   (m:.n:.o:.p:.()):.()) =
  (a:.b:.c:.(a * x + d):.()):.
  (e:.f:.g:.(e * x + h):.()):.
  (i:.j:.k:.(i * x + l):.()):.
  (m:.n:.o:.p:.()):.()

translateAlongLocalY :: Num a => a -> Mat44 a -> Mat44 a
translateAlongLocalY x
  ((a:.b:.c:.d:.()):.
   (e:.f:.g:.h:.()):.
   (i:.j:.k:.l:.()):.
   (m:.n:.o:.p:.()):.()) =
  (a:.b:.c:.(b * x + d):.()):.
  (e:.f:.g:.(f * x + h):.()):.
  (i:.j:.k:.(j * x + l):.()):.
  (m:.n:.o:.p:.()):.()

translateAlongLocalZ :: Num a => a -> Mat44 a -> Mat44 a
translateAlongLocalZ x
  ((a:.b:.c:.d:.()):.
   (e:.f:.g:.h:.()):.
   (i:.j:.k:.l:.()):.
   (m:.n:.o:.p:.()):.()) =
  (a:.b:.c:.(c * x + d):.()):.
  (e:.f:.g:.(g * x + h):.()):.
  (i:.j:.k:.(k * x + l):.()):.
  (m:.n:.o:.p:.()):.()

-- the multtvm can be factored inside a multaxis function.
rotateAroundLocalX :: Floating a => a -> Mat44 a -> Mat44 a
rotateAroundLocalX x t@
  ((a:.b:.c:.d:.()):.
   (e:.f:.g:.h:.()):.
   (i:.j:.k:.l:.()):.
   (m:.n:.o:.p:.()):.()) =
  ((a':.b':.c':.d:.()):.
   (e':.f':.g':.h:.()):.
   (i':.j':.k':.l:.()):.
   (m:.n:.o:.p:.()):.())
  where
  rot = rotationVec (xAxis t) x
  (a':.e':.i':._:.()) = multvm (a:.e:.i:.0:.()) rot
  (b':.f':.j':._:.()) = multvm (b:.f:.j:.0:.()) rot
  (c':.g':.k':._:.()) = multvm (c:.g:.k:.0:.()) rot

rotateAroundLocalY :: Floating a => a -> Mat44 a -> Mat44 a
rotateAroundLocalY x t@
  ((a:.b:.c:.d:.()):.
   (e:.f:.g:.h:.()):.
   (i:.j:.k:.l:.()):.
   (m:.n:.o:.p:.()):.()) =
  ((a':.b':.c':.d:.()):.
   (e':.f':.g':.h:.()):.
   (i':.j':.k':.l:.()):.
   (m:.n:.o:.p:.()):.())
  where
  rot = rotationVec (yAxis t) x
  (a':.e':.i':._:.()) = multvm (a:.e:.i:.0:.()) rot
  (b':.f':.j':._:.()) = multvm (b:.f:.j:.0:.()) rot
  (c':.g':.k':._:.()) = multvm (c:.g:.k:.0:.()) rot

rotateAroundGlobalY :: Floating a => a -> Mat44 a -> Mat44 a
rotateAroundGlobalY x
  ((a:.b:.c:.d:.()):.
   (e:.f:.g:.h:.()):.
   (i:.j:.k:.l:.()):.
   (m:.n:.o:.p:.()):.()) =
  ((a':.b':.c':.d:.()):.
   (e':.f':.g':.h:.()):.
   (i':.j':.k':.l:.()):.
   (m:.n:.o:.p:.()):.())
  where
  rot = rotationVec (0:.1:.0:.()) x
  (a':.e':.i':._:.()) = multvm (a:.e:.i:.0:.()) rot
  (b':.f':.j':._:.()) = multvm (b:.f:.j:.0:.()) rot
  (c':.g':.k':._:.()) = multvm (c:.g:.k:.0:.()) rot


-- | Given a location (sx,sy) in window coordinates ((0,0) is the lower-left
-- corner) and the window dimensions (in pixels), rayFromPixel returns the
-- origin and direction (in world coordinates) from the ray passing through
-- the eye and the given location (on the near plane).
rayFromPixel :: Floating a => Mat44 a -> (a,a,a,a) -> Int -> Int -> a
  -> Int -> Int -> (Vec3 a, Vec3 a)
rayFromPixel mat (l,r,b,t) ww wh hither sx sy = (o + dir,normalize dir)
  where
  o = origin mat
  x = xAxis mat
  y = yAxis mat
  z = zAxis mat
  -- horizontal delta on the near plane
  h = l + (r - l) / fromIntegral ww * (fromIntegral sx + 0.5)
  -- vertical delta on the near plane
  v = b + (t - b) / fromIntegral wh * (fromIntegral sy + 0.5)
  -- the vector from origin to the point on the near plane
  dir = x * vec h + y * vec v - z * vec hither

