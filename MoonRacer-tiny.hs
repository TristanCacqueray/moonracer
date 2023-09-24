{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Data.Bool
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

{-
import Debug.Trace
debugWorld :: Float -> World -> a -> a
debugWorld ts (_, pos, vel, acc, _) = traceShow ("ts", ts, "pos", pos, "vel", vel, "acc", acc)
debugStep :: Float -> State -> State
debugStep ts s@(_, w) = debugWorld ts w (step ts s)
-}

type World = ((Float, Float), (Float, Float), (Float, Float), [(Float, Float)])
initialWorld :: World
initialWorld = (origin, origin, origin, initialStars)

origin :: (Float, Float)
origin = (0, 0)

initialStars :: [(Float, Float)]
initialStars =
    [ ( fromIntegral (x `mod` 70 - 35)
      , fromIntegral (5 + x `mod` 50)
      )
    | x <- [0 :: Int, 88 ..]
    ]

type State = (String, (Float, Float), World)

translatePoint :: (Float, Float) -> Picture -> Picture
translatePoint (x, y) = translate (x * 10) (y * 10)
scaleUniform :: Float -> Picture -> Picture
scaleUniform factor = scale factor factor

drawCircle :: (Float, Float) -> Picture
drawCircle pos = translatePoint pos (Circle 3)

drawVelocy :: (Float, Float) -> (Float, Float) -> Picture
drawVelocy pos v = translatePoint pos (scaleUniform 10 $ line [origin, v])

draw :: State -> Picture
draw (status, _, (_, p, _, g : _)) =
    translatePoint
        (0, -29.7)
        ( scaleUniform 0.1 (Text status)
            <> drawCircle p
            <> drawCircle g
        )

eventKey :: Event -> (SpecialKey, Float)
eventKey (EventKey (SpecialKey key) d _ _) = (key, bool 1 0 $ d == Up)
eventKey _ = (KeyF1, 0)

event :: Event -> State -> State
event ev (status, (x, y), w) =
    let (k, isPressed) = eventKey ev
        ax
            | k == KeyRight = isPressed
            | k == KeyLeft = negate isPressed
            | otherwise = x
        ay
            | k == KeyUp = isPressed
            | k == KeyDown = negate isPressed
            | otherwise = y
     in (status, (ax, ay), w)

damp = (*) 0.8
gravity = 0.3

step :: Float -> State -> State
step ts (status, accel@(ax, ay), ((score, time), (px, py), (vx, vy), stars@((sx, sy) : r))) =
    let hit m n = abs (m - n) < 1
        (newScore, rest)
            | hit px sx && hit py sy = (score + 1, r)
            | otherwise = (score, stars)
        newPos = (px + (vx + ax) * gravity, py + (vy + ay) * gravity)
        newVel = (damp (ax + vx), damp (ay + vy - gravity))
        nextWorld = ((newScore, time + ts), newPos, newVel, rest)
        newState
            | score > 9 = (show time, origin, initialWorld)
            | otherwise = (status, accel, nextWorld)
     in newState

win :: Display
win = InWindow "MoonRacer" (800, 600) (5, 5)

main :: IO ()
main = play win white 30 ("", origin, initialWorld) draw event {-debug-} step
