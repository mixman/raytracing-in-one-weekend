{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
module Main where

import Prelude hiding (div, pi)
import Control.Monad.Primitive (PrimState)
import Control.DeepSeq
import Control.Monad
import Control.Monad.ST (RealWorld)
import Control.Monad.IO.Class
import Data.Maybe (catMaybes)
import Data.Traversable (for)
import Data.Foldable (foldl')
import GHC.Generics (Generic)
import System.Random (randomRIO)
import System.Random.Stateful (uniformDouble01M, uniformR, applyAtomicGen, globalStdGen)
--import System.Random.MWC (create)
import System.Random.PCG.Class
import System.Random.PCG.Fast.Pure (Gen, create, uniform)

--- Image
--aspect_ratio = 16.0/9.0 :: Double
aspect_ratio = 3/2 :: Double
image_width = 1200 :: Int
image_height = floor $ fromIntegral image_width/aspect_ratio :: Int
samples_per_pixel = 100 :: Int
max_depth = 40

--- Camera
focal_length = 1.0

--

pi = 3.1415926535897932385
infinity = read "Infinity" :: Double

degreesToRadians :: Double -> Double
degreesToRadians degrees = degrees * pi / 180.0

-- Returns a random real in [0,1]
random_double :: System.Random.PCG.Class.Generator g m => g -> m Double
random_double gen = uniform gen

random_double' :: System.Random.PCG.Class.Generator g m => g -> Double -> Double -> m Double
random_double' gen min max = do
    rnd <- random_double gen
    return $ min + (max-min) * rnd

-- Main

main :: IO ()
main = do
    let r = cos(pi/4)
    {-
    let world = HittableList [ Sphere (point3 0 (-100.5) (-1)) 100 (Lambertian $ color 0.8 0.8 0.0)
                             , Sphere (point3 0 0 (-1)) 0.5 (Lambertian $ color 0.1 0.2 0.5)
                             --, Sphere (point3 0 0 (-1)) 0.5 (Dielectric 1.5)
                             --, Sphere (point3 0 0 (-1)) 0.5 (Lambertian $ color 0.7 0.3 0.3)
                             --, Sphere (point3 (-1) 0 (-1)) 0.5 (Metal (color 0.8 0.8 0.8) 0.3)
                             , Sphere (point3 (-1) 0 (-1)) 0.5 (Dielectric 1.5)
                             , Sphere (point3 (-1) 0 (-1)) (-0.45) (Dielectric 1.5)
                             , Sphere (point3   1  0 (-1)) 0.5 (Metal (color 0.8 0.6 0.2) 0)
                             ]
    -}
    let worldImage17 = HittableList [ Sphere (point3 (-r) 0 (-1))   r  (Lambertian $ color 0 0 1)
                             , Sphere (point3   r  0 (-1)) (-r) (Lambertian $ color 1 0 0)
                             ]
    --let cam' = mkCam (point3 (-2) 2 1) (point3 0 0 (-1)) (Vec3 0 1 0) 90 aspect_ratio
    --let lookfrom = point3 3 3 2
    --let lookat = point3 0 0 (-1)
    --let dist_to_focus = lengthV (lookfrom-lookat)
    --let aperture = 2.0
    let lookfrom = point3 13 2 3
    let lookat = point3 0 0 0
    let dist_to_focus = 10.0
    let aperture = 0.1

    let cam = mkCam lookfrom lookat (Vec3 0 1 0) 20 aspect_ratio aperture dist_to_focus
    let ys = [image_height-1, (image_height-2) .. 0] :: [Int]
    let xs = [0 .. (image_width-1)] :: [Int]
    gen <- create

    finalScene <- randomScene gen
    let world = HittableList finalScene

    let range = [0 .. samples_per_pixel] -- TODO: useless list creation, just because for
    let loopBody x y = do
            -- gen 50 samples for (x,y) ray
            vecs <- for range $ \_ -> do
                        rndA <- random_double gen
                        rndB <- random_double gen
                        let u = (fromIntegral x + rndA) / fromIntegral (image_width-1)
                        let v = (fromIntegral y + rndB) / fromIntegral (image_height-1)
                        rx <- cameraRay u v cam gen
                        pure $ rayColor gen rx world max_depth
            vecs' <- sequence vecs
            let pixel_color = foldl' (+) (color 0 0 0) vecs'
            pure $ writeColor pixel_color samples_per_pixel
    let scanLinesRemainingDebug y = "\rScanlines remaining: " ++ show y ++ "\n"
    -- list is 90k items (width*height)
    body' <- sequence $ concatMap (\y -> map (`loopBody` y) xs) ys
    let body = concat body'
    putStrLn $ "P3\n" ++ show image_width ++ " " ++ show image_height ++ "\n255\n" ++ body

-- Material

data Material = Lambertian { albedo :: !Vec3 }
              | Metal { albedo :: !Vec3, fuzz :: !Double }
              | Dielectric { indexOfRefraction :: !Double }
              deriving (Show, Generic)
instance NFData Material

data MaterialInteraction = MaterialInteraction
    { materialScattered   :: !Ray
    , materialAttenuation :: !Vec3
    } deriving (Generic)
instance NFData MaterialInteraction

class Scatter s where
    scatter :: s -> Gen RealWorld -> Ray -> HitRecord -> IO (Maybe MaterialInteraction)

instance Scatter Material where
    scatter (Lambertian a) gen rIn hr = do
        randU <- randomUnitVector gen
        let scatter_direction = getNormal hr + randU
        let scatter_direction' = if nearZero scatter_direction then getNormal hr else scatter_direction
        return $ Just MaterialInteraction { materialScattered=Ray (getP hr) scatter_direction'
                                          , materialAttenuation=albedo (getMaterial hr)
                                          }
    scatter (Metal a fuzz) gen rIn hr = do
        let reflected = reflect (unitVector (getDirection rIn)) (getNormal hr)
        randIH <- randomInUnitSphere gen
        let scattered = Ray (getP hr) (reflected + mul randIH fuzz)
        if dot (getDirection scattered) (getNormal hr) > 0
            then return $ Just MaterialInteraction { materialScattered=scattered
                                                   , materialAttenuation=albedo (getMaterial hr)
                                                   }
            else return Nothing

    scatter (Dielectric ior) gen (Ray _ dir) (HitRecord p n _ ff m) = do
        let attenuation = color 1 1 1
        let refraction_ratio = if ff then 1.0/ior else ior
        let unit_direction = unitVector dir
        let cosTheta = min (dot (-unit_direction) n) 1.0
        let sinTheta = sqrt (1.0 - cosTheta * cosTheta)
        let cannotRefract = refraction_ratio * sinTheta > 1.0
        rnd <- random_double gen
        let direction = if cannotRefract || reflectance cosTheta refraction_ratio > rnd
                            then reflect unit_direction n
                            else refract unit_direction n refraction_ratio
        return $ Just MaterialInteraction { materialScattered=Ray p direction
                                          , materialAttenuation=attenuation
                                          }

-- Camera

data Camera = Camera
    { cameraOrigin          :: !Vec3
    , cameraLowerLeftCorner :: !Vec3
    , cameraHorizontal      :: !Vec3
    , cameraVertical        :: !Vec3
    , cameraU               :: !Vec3
    , cameraV               :: !Vec3
    , cameraW               :: !Vec3
    , cameraLensRadius      :: !Double
    }

mkCam :: Vec3 -> Vec3 -> Vec3 -> Double -> Double -> Double -> Double -> Camera
mkCam lookfrom lookat vup vfov aspect_ratio aperture focus_dist =
    let
        theta = degreesToRadians vfov
        h = tan(theta/2)
        viewport_height = 2.0*h
        viewport_width = aspect_ratio * viewport_height

        w = unitVector $ lookfrom - lookat
        u = unitVector $ cross vup w
        v = cross w u

        -- TODO: multiplying Vec*scalar without mul
        origin = lookfrom
        horizontal = mul (mul u viewport_width) focus_dist
        vertical = mul (mul v viewport_height) focus_dist
        lower_left_corner = origin - div horizontal 2 - div vertical 2 - mul w focus_dist
        lens_radius = aperture/2
        in Camera origin lower_left_corner horizontal vertical u v w lens_radius

-- get_ray
--cameraRay :: Double -> Double -> Camera -> Gen RealWorld -> m Ray
cameraRay s t cam gen = do
    rnd <- randomInUnitDisk gen
    let rd = mul rnd (cameraLensRadius cam)
    let offset = mul (cameraU cam) (x rd) + mul (cameraV cam) (y rd)
    return $ Ray (cameraOrigin cam + offset)
                 (cameraLowerLeftCorner cam + mul (cameraHorizontal cam) s + mul (cameraVertical cam) t - cameraOrigin cam - offset)

-- Hittable
data HitRecord = HitRecord
    { getP         :: !Vec3 -- Location
    , getNormal    :: !Vec3
    , getT         :: !Double -- Time
    , getFrontFace :: !Bool
    , getMaterial  :: !Material
    } deriving (Show)

data Hittable = Sphere { center :: !Vec3, radius :: !Double, material :: !Material}
    | HittableList [Hittable]
    deriving (Show)

newtype HitListItem = HitListItem { getHitRecord :: HitRecord }

-- NOTE: Bool is now Maybe HitRecord as modifying the passed in HitRecord
-- HitRecord was modified on success in C++. Was passed as reference in C++ as used outside of function scope
-- Need Hittable, as using its properties within function
hit :: Ray -> Double -> Double -> Hittable -> Maybe HitRecord
hit r@(Ray o d) t_min t_max (Sphere center radius material) =
    let
        oc = getOrigin r - center
        a = lengthSquared $ getDirection r
        half_b = dot oc (getDirection r)
        c = lengthSquared oc - radius*radius
        discriminant = half_b*half_b - a*c
        sqrtd = sqrt discriminant
        -- Find the nearest root that lies in the acceptable range.
        root = ((-half_b) - sqrtd) / a
        root' = ((-half_b) + sqrtd) / a
        go | discriminant < 0 = Nothing
           | (root < t_min || t_max < root) && (root' < t_min || t_max < root') = Nothing
           | otherwise =
                let
                    p = at r root
                    outward_normal = (p - center) `div` radius
                    -- set_face_normal applied here
                    front_face = dot (getDirection r) outward_normal < 0
                    normal = if front_face then outward_normal else (-outward_normal)
                    in Just HitRecord { getT=root, getP=p, getNormal=normal, getFrontFace=front_face, getMaterial=material }
        in go
-- TODO: how to do this more eloquently?
-- - mconcat discards Nothing, thus keeping the state we want
hit r t_min t_max hlist@(HittableList objs) =
    let
        go (x:xs) Nothing = go xs (hit r t_min t_max x)
        go (x:xs) prev =
            let
                hres = hit r t_min (getT $ stateT prev Nothing) x
                in go xs (Just $ stateT prev hres)
        go [] prev = prev
        stateT prev now = case (prev, now) of
                -- TODO: pass Nothing instead of dummy Lambertian, or else?
                (Nothing, Nothing) -> HitRecord (Vec3 0 0 0) (Vec3 0 0 0) t_max False (Lambertian $ Vec3 255 0 0)
                (Nothing, Just hr) -> hr
                (Just hr, Nothing) -> hr
                (Just hr, Just hr') -> hr'
        in go objs Nothing

-- Ray

data Ray = Ray
    { getOrigin    :: !Vec3
    , getDirection :: !Vec3
    } deriving (Eq, Show, Generic)
instance NFData Ray

at :: Ray -> Double -> Vec3
at (Ray o d) t = o + mul d t

hitSphere :: Vec3 -> Double -> Ray -> Double
hitSphere center radius r =
    let
        oc = getOrigin r - center
        a = lengthSquared $ getDirection r
        half_b = dot oc (getDirection r)
        c = lengthSquared oc - radius*radius
        discriminant = half_b*half_b - a*c
        go = if discriminant < 0
                then -1.0
                else (-half_b - sqrt discriminant) / a
        in go

--rayColor :: Gen RealWorld -> Ray -> Hittable -> Int -> m Vec3
rayColor gen r world depth = do
    let unitDirection = unitVector $ getDirection r
    let t = 0.5*(y unitDirection + 1.0)
    if depth <= 0
        then pure $ color 0 0 0 -- TODO: this becomes final value instead of sampling.. why?
        else case hit r 0.001 infinity world of
            Just hr -> do
                scatter' <- scatter (getMaterial hr) gen r hr
                case scatter' of
                    Just mi -> do
                        rc <- rayColor gen (materialScattered mi) world (depth-1)
                        pure $ materialAttenuation mi * rc
                    Nothing -> pure $ color 0 0 0
            Nothing -> pure $ mul (color 1.0 1.0 1.0) (1.0-t) + mul (color 0.5 0.7 1.0) t

-- Vectors

data Vec3 = Vec3
    { x :: !Double
    , y :: !Double
    , z :: !Double
    } deriving (Eq, Show, Generic)
instance NFData Vec3

point3 = Vec3 -- 3D point
color = Vec3 -- RGB color

instance Num Vec3 where
    (+) = liftV2 (+)
    (-) = liftV2 (-)
    (*) = liftV2 (*)
    fromInteger = vec . fromInteger

liftV2 :: (Double -> Double -> Double) -> Vec3 -> Vec3 -> Vec3
liftV2 f (Vec3 a b c) (Vec3 x' y' z') = Vec3 (f a x') (f b y') (f c z')

vec3_random gen = Vec3 <$> random_double gen <*> random_double gen <*> random_double gen
vec3_random' gen min max = Vec3 <$> random_double' gen min max <*> random_double' gen min max <*> random_double' gen min max

nearZero :: Vec3 -> Bool
nearZero v = abs (x v) < s && abs (y v) < s && abs (z v) < s
    where
        s = 1e-8

reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = v - mul n (dot v n) * 2

refract :: Vec3 -> Vec3 -> Double -> Vec3
refract uv n etaiOverEtat =
    let
        cosTheta = min (dot (-uv) n) 1.0
        rOutPerpendicular = mul (uv + mul n cosTheta) etaiOverEtat
        rOutParallel = mul n (-sqrt(abs(1.0 - lengthSquared rOutPerpendicular)))
    in rOutPerpendicular + rOutParallel

reflectance :: Double -> Double -> Double
reflectance cosine ref_idx = let r0 = (1-ref_idx) / (1+ref_idx)
                                 r0' = r0*r0
                                 in r0' + (1-r0') * ((1-cosine) ** 5)

randomInUnitSphere gen = do
    p <- vec3_random' gen (-1) 1
    if lengthSquared p >= 1 then randomInUnitSphere gen else return p

randomUnitVector gen = unitVector <$> (randomInUnitSphere gen)

randomInHemisphere gen normal = do
    inUnitSphere <- randomInUnitSphere gen
    if dot inUnitSphere normal > 1.0 then return (-inUnitSphere) else return inUnitSphere

randomInUnitDisk :: Gen RealWorld -> IO Vec3
randomInUnitDisk gen = do
    p <- vec3 <$> random_double' gen (-1) 1 <*> random_double' gen (-1) 1 <*> pure 0
    if lengthSquared p >= 1
        then randomInUnitDisk gen
        else return p

vec :: Double -> Vec3
vec a = Vec3 a a a

vec3 :: Double -> Double -> Double -> Vec3
vec3 = Vec3

dot :: Vec3 -> Vec3 -> Double
dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
    Vec3 (y1*z2 - z1*y2)
         (z1*x2 - x1*z2)
         (x1*y2 - y1*x2)

unitVector :: Vec3 -> Vec3
unitVector v = v `div` lengthV v

-- multiply == scaling
mul :: Vec3 -> Double -> Vec3
mul (Vec3 x1 y1 z1) t = Vec3 (x1*t) (y1*t) (z1*t)

-- division == shrinking
div :: Vec3 -> Double -> Vec3
div _ 0 = error "divide by 0 in 'div'"
div v t = mul v (1.0/t)

lengthV :: Vec3 -> Double
lengthV v = sqrt $ lengthSquared v

lengthSquared :: Vec3 -> Double
lengthSquared v = dot v v

-- Color/Utility

clamp :: Double -> Double -> Double -> Double
clamp x min max
    | x < min = min
    | x > max = max
    | otherwise = x

writeColor :: Vec3 -> Int -> String
writeColor pixel_color@(Vec3 x' y' z') samplesPerPixel =
    let
        scale = 1.0 / fromIntegral samplesPerPixel
        toString x = show $ floor $ 256 * clamp (sqrt(scale*x)) 0.0 0.999
        in toString x' ++ " " ++ toString y' ++ " " ++ toString z' ++ "\n"


--genWorld :: Gen RealWorld -> m [Maybe Hittable]
genWorld gen = fmap (world <>) (randomSpheres gen)
    where
        ground_material = Lambertian $ color 0.5 0.5 0.5
        material1 = Dielectric 1.5
        material2 = Lambertian $ Vec3 0.4 0.2 0.1
        material3 = Metal (Vec3 0.7 0.6 0.5) 0.0
        world = [ Sphere (point3 0 (-1000) 0) 1000 ground_material
                , Sphere (point3 0 1 0) 1.0 material1
                , Sphere (point3 (-4) 1 0) 1.0 material2
                , Sphere (point3 4 1 0) 1.0 material3
                ]
        diffuse gen = do
            r0 <- vec3_random gen
            r1 <- vec3_random gen
            let albedo = r0 * r1
            pure $ Lambertian albedo

        metal gen = do
            albedo <- vec3_random' gen 0.5 1
            fuzz <- random_double' gen 0 0.5
            pure $ Metal albedo fuzz

        glass gen = pure $ Dielectric 1.5

        validCenter center = lengthV (center - point3 4 0.2 0) > 0.9

        --randomCenter :: Gen RealWorld -> Double -> Double -> m Vec3
        randomCenter gen a b = do
                rndA <- random_double gen
                rndB <- random_double gen
                pure $ point3 (rndA*0.9 + a) 0.2 (rndB*0.9 +b)

        matChoice gen x
              | x < 0.8 = diffuse gen
              | x < 0.95 = metal gen
              | otherwise = glass gen

        randomMat gen = do
            matSeed <- random_double gen
            rm <- matChoice gen matSeed
            pure $ rm

        mkSphere gen center = do
            m <- randomMat gen
            pure $ Sphere center 0.2 m

        centers gen = [randomCenter gen a b | a <- [-11 .. 10], b <- [-11 .. 10]]

        --randomSpheres :: Gen RealWorld -> [Vec3]
        randomSpheres gen = do
            c <- sequence (centers gen)
            let validC = filter validCenter c
            ps <- traverse (\c -> mkSphere gen c) validC
            pure $ ps
            --pure (mconcat ps)

        outer gen = for [ -11 .. 10 ] $ \a ->
                        for [ -11 .. 10 ] $ \b -> do
                            center <- randomCenter gen a b
                            m <- randomMat gen
                            return $ Sphere center 0.2 m

--randomScene :: Gen RealWorld -> m HittableList
randomScene gen = do
    w <- genWorld gen
    pure $ w

