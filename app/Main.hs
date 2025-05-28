{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate
import System.Random (randomRIO)
import Text.Printf (printf)
import Data.IORef
import Control.Monad (when)

-- for json loading
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import Data.Aeson.Types (Parser)

-- for trace
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

-- Add this type to hold the trace
type Trace = Seq.Seq (Float, Float)

-- Configuration
data PendulumConfig = PendulumConfig
  { m1 :: Double, m2 :: Double
  , l1 :: Double, l2 :: Double
  , g  :: Double
  , maxTraceLength :: Int
  , colorHead :: Color
  , colorTrace :: Color
  }

-- Helper to parse [r,g,b] arrays and convert to Color
parseColor :: Maybe Value -> Color -> Parser Color
parseColor (Just v) _ = withArray "RGB" (\arr ->
  if length arr == 3
    then do
      let [r, g, b] = toList arr
      r' <- parseJSON r
      g' <- parseJSON g
      b' <- parseJSON b
      return $ makeColorI r' g' b' 255
    else fail "RGB array must have exactly 3 elements") v
parseColor Nothing def = pure def

instance FromJSON PendulumConfig where
  parseJSON = withObject "PendulumConfig" $ \v -> do
    m1 <- v .:? "mass_1"    .!= 1.0
    m2 <- v .:? "mass_2"    .!= 1.0
    l1 <- v .:? "length_1"  .!= 1.0
    l2 <- v .:? "length_2"  .!= 1.0
    g  <- v .:? "g"         .!= 9.8
    maxTraceLength <- v .:? "max_trace_length" .!= 500
    rawColorHead <- (v .:? "color_head")
    rawColorTrace <- (v .:? "color_trace")
    colorHead <- parseColor rawColorHead (makeColorI 255 0 0 255)
    colorTrace <- parseColor rawColorTrace (makeColorI 0 255 0 128)
    return PendulumConfig{..}

-- State
data PendulumState = PendulumState
  { theta1 :: Double, theta2 :: Double
  , omega1 :: Double, omega2 :: Double }

instance FromJSON PendulumState where
  parseJSON = withObject "PendulumState" $ \v -> do
    theta1Deg <- v .:? "theta_1" .!= 0
    theta2Deg <- v .:? "theta_2" .!= 0
    omega1 <- v .:? "omega_1" .!= 0
    omega2 <- v .:? "omega_2" .!= 0
    let degToRad x = x * pi / 180
        theta1 = degToRad theta1Deg
        theta2 = degToRad theta2Deg
    return PendulumState{..}

-- Simple Euler integration
eulerStep :: PendulumConfig -> Double -> PendulumState -> PendulumState
eulerStep cfg dt state@PendulumState{..} =
  let (a1, a2) = accelerations cfg state
      newOmega1 = omega1 + a1 * dt
      newOmega2 = omega2 + a2 * dt
      newTheta1 = theta1 + omega1 * dt
      newTheta2 = theta2 + omega2 * dt
  in PendulumState newTheta1 newTheta2 newOmega1 newOmega2

-- Calculate angular accelerations
accelerations :: PendulumConfig -> PendulumState -> (Double, Double)
accelerations PendulumConfig{..} PendulumState{..} =
  let denom = l1*(2*m1 + m2 - m2*cos(2*θ1 - 2*θ2))
      a1 = (-g*(2*m1 + m2)*sin θ1 - m2*g*sin(θ1 - 2*θ2) - 2*sin(θ1-θ2)*m2*(omega2^2*l2 + omega1^2*l1*cos(θ1-θ2))) / denom
      a2 = (2*sin(θ1-θ2)*(omega1^2*l1*(m1+m2) + g*(m1+m2)*cos θ1 + omega2^2*l2*m2*cos(θ1-θ2))) / denom
  in (a1, a2)
  where
    θ1 = theta1
    θ2 = theta2


-- Rendering
renderPendulum :: PendulumConfig -> PendulumState -> Picture
renderPendulum PendulumConfig{..} PendulumState{..} =
  let x1 = realToFrac (l1 * sin theta1) :: Float
      y1 = realToFrac (-l1 * cos theta1) :: Float
      x2 = realToFrac (l1 * sin theta1 + l2 * sin theta2) :: Float
      y2 = realToFrac (-l1 * cos theta1 - l2 * cos theta2) :: Float
      scaleFactor = 100  -- pixels per meter
      bobRadius = 10
  in pictures
      [ color blue $ line [(0, 0), (x1*scaleFactor, y1*scaleFactor)]
      , color colorHead $ translate (x1*scaleFactor) (y1*scaleFactor) $ circleSolid bobRadius
      , color blue $ translate (x1*scaleFactor) (y1*scaleFactor) $ line [(0, 0), ((x2-x1)*scaleFactor, (y2-y1)*scaleFactor)]
      , color colorHead $ translate (x2*scaleFactor) (y2*scaleFactor) $ circleSolid bobRadius
      , color white $ translate (-300) (-250) $ scale 0.1 0.1 $ text $ 
          printf "θ₁=%.2f θ₂=%.2f" (realToFrac theta1 :: Float) (realToFrac theta2 :: Float)
      ]

renderPendulumWithTrace :: PendulumConfig -> PendulumState -> [(Float, Float)] -> Picture
renderPendulumWithTrace cfg st trace =
  pictures
    [ color (colorTrace cfg) $ line trace
    , renderPendulum cfg st
    ]

-- Default config and state generator
defaultConfigAndState :: IO (PendulumConfig, PendulumState)
defaultConfigAndState = do
  θ1 <- randomRIO (-pi/2, pi/2)
  θ2 <- randomRIO (-pi, pi)
  let c = PendulumConfig 1.0 1.0 1.0 1.0 9.8 500 (makeColorI 255 0 0 255) (makeColorI 0 255 0 128)
      s = PendulumState θ1 θ2 0 0
  return (c, s)


-- Load config and state from file, fallback to defaults/random
loadConfigAndState :: FilePath -> IO (PendulumConfig, PendulumState)
loadConfigAndState path = do
  content <- B.readFile path
  let cfg = decode content :: Maybe PendulumConfig
      st  = decode content :: Maybe PendulumState
  case (cfg, st) of
    (Just c, Just s) -> return (c, s)
    _ -> defaultConfigAndState

-- Animation
main :: IO ()
main = do
  args <- getArgs
  -- Load first pendulum config/state
  (cfg1, st1) <-
    case args of
      (configPath1:configPath2:_) -> loadConfigAndState configPath1
      (configPath1:_) -> loadConfigAndState configPath1
      _ -> defaultConfigAndState
  -- Load second pendulum config/state
  (cfg2, st2) <-
    case args of
      (_:configPath2:_) -> loadConfigAndState configPath2
      _ -> defaultConfigAndState
  let window = InWindow "Double Pendulum" (800, 600) (10, 10)

  -- Create a mutable reference to hold the state
  traceRef1 <- newIORef Seq.empty
  traceRef2 <- newIORef Seq.empty
  stateRef1 <- newIORef st1
  stateRef2 <- newIORef st2
  -- Use animateIO instead of animate to maintain state between frames
  animateIO window black (frameFunc2 cfg1 stateRef1 traceRef1 cfg2 stateRef2 traceRef2) (const (return ()))


-- Helper to update pendulum state and trace
updatePendulum
  :: PendulumConfig
  -> IORef PendulumState
  -> IORef Trace
  -> Float -- scaleFactor
  -> IO (PendulumState, [(Float, Float)])
updatePendulum cfg stateRef traceRef scaleFactor = do
  st <- readIORef stateRef
  let dt = 0.016
      newSt = eulerStep cfg dt st
      x2 = realToFrac (l1 cfg * sin (theta1 newSt) + l2 cfg * sin (theta2 newSt)) :: Float
      y2 = realToFrac (-l1 cfg * cos (theta1 newSt) - l2 cfg * cos (theta2 newSt)) :: Float
      headPos = (x2 * scaleFactor, y2 * scaleFactor)
  modifyIORef' traceRef $ \trace ->
    let trace' = trace Seq.|> headPos
    in if Seq.length trace' > maxTraceLength cfg then Seq.drop 1 trace' else trace'
  writeIORef stateRef newSt
  traceList <- fmap toList (readIORef traceRef)
  return (newSt, traceList)

-- Frame function for two pendulums
frameFunc2
  :: PendulumConfig -> IORef PendulumState -> IORef Trace
  -> PendulumConfig -> IORef PendulumState -> IORef Trace
  -> Float -> IO Picture
frameFunc2 cfg1 stateRef1 traceRef1 cfg2 stateRef2 traceRef2 _time = do
  let scaleFactor = 100
  (newSt1, trace1) <- updatePendulum cfg1 stateRef1 traceRef1 scaleFactor
  (newSt2, trace2) <- updatePendulum cfg2 stateRef2 traceRef2 scaleFactor

  -- Compose both pendulums
  let pend1 = renderPendulumWithTrace cfg1 newSt1 trace1
      pend2 = renderPendulumWithTrace cfg2 newSt2 trace2
  return $ pictures [pend1, pend2]

