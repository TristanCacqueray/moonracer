{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Data.Bool
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

import Debug.Trace

type World = ((Float, Float), (Float, Float), (Float, Float), (Float, Float), [(Float, Float)])
initialWorld :: World
initialWorld = (origin, origin, origin, origin, initialStars)

origin :: (Float, Float)
origin = (0, 0)

initialStars :: [(Float, Float)]
initialStars =
    [ ( fromIntegral (x `mod` 70 - 35)
      , fromIntegral (5 + x `mod` 50)
      )
    | x <- [0 :: Int, 88 ..]
    ]

type State = (String, World)

drawCircle :: (Float, Float) -> Picture
drawCircle (x, y) = translate (x * 10) (y * 10) (Circle 3)

draw :: State -> Picture
draw (status, (_, p, _, _, g : _)) = translate 0 (-297) (scale 0.1 0.1 (Text status) <> drawCircle p <> drawCircle g)
-- draw (status, w@(_, p, v, _, g : _)) = translate 0 (-297) (scale 0.1 0.1 (Text (show (status, p, v))) <> drawCircle p <> drawCircle g)

eventKey :: Event -> (SpecialKey, Float)
eventKey (EventKey (SpecialKey key) d _ _) = (key, bool 0 1 $ d == Down)
eventKey _ = (KeyF1, 0)

event :: Event -> State -> State
event ev (status, (score, pos, vel, (x, y), stars)) =
    let (k, d) = eventKey ev
        ax
            | k == KeyRight = d
            | k == KeyLeft = -1 * d
            | otherwise = x
        ay
            | k == KeyUp = d
            | k == KeyDown = -1 * d
            | otherwise = y
     in (status, (score, pos, vel, (ax, ay), stars))

step :: Float -> State -> State
step ts (status, ((score, time), (px, py), (vx, vy), a@(ax, ay), (sx, sy) : r)) =
    let hit m n = abs (m - n) < 1
        (newScore, rest) = if hit px sx && hit py sy then (score + 1, r) else (score, (sx, sy) : r)
        newPx = px + (vx + ax) * ts * 10
        newPy = max 0 (py + (vy + ay) * ts * 10)
        newVx = (ax + vx) * 0.8
        newVy = (ay + vy) * 0.8 + (-0.5)
        (newStatus, newWorld)
            | score > 9 = ("Best: " <> show time, initialWorld)
            | otherwise = (status, ((newScore, time + ts), (newPx, newPy), (newVx, newVy), a, rest))
     in (newStatus, newWorld)

win :: Display
win = InWindow "MoonRacer" (800, 600) (20, 20)

main :: IO ()
main = play win white 30 ("", initialWorld) draw event step
