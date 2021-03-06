module Fractales where

import Control.Parallel.Strategies
import Control.Monad
import GHC.Float
import System.Environment(getArgs)   

type ℝ = Double
data ℂ = Punto (ℝ,ℝ) deriving Show

instance Num ℂ where
  (+) (Punto (a,b)) (Punto (c,d)) = Punto (a+c, b+d)
  (*) (Punto (a,b)) (Punto (c,d)) = Punto ((a*c - b*d), (a*d + b*c))
  abs = error "Not necesary"
  signum = error "Not necesary"
  fromInteger = error "Not necesary"
  negate = error "Not necesary"
   
norma :: ℂ -> ℝ
norma (Punto (x,y)) = x*x + y*y

puntos_y :: ℝ -> Int -> [ℂ]
puntos_y delta n = [Punto ((-1), (1 - (fromIntegral(i) * delta))) | i <- [0..(n-1)]]

puntos_x :: ℂ -> ℝ -> Int-> [ℂ]
puntos_x (Punto (x,y)) delta n = [Punto (x + (fromIntegral(i) * delta), y) | i <- [0..(n-1)]]

plano :: ℝ -> [ℂ]
plano n = let delta = 2 / (n-1)
              ys = puntos_y delta (floor n) in
            concat(liftM3 puntos_x ys [delta] [floor n])

mandelbrot_set :: [ℂ]->String
mandelbrot_set [x] = if s_c x  then "1" else "0"
mandelbrot_set (x:xs) = if s_c x
                        then "1 "++(mandelbrot_set xs)
                        else "0 "++(mandelbrot_set xs)

s_c :: ℂ -> Bool
s_c x = let sc = [f x i | i <- [1..200]] in
          aux sc
          where
            f x 1 = x
            f x n = let z_n1 = f x (n-1) in
                      z_n1*z_n1 + x
            aux [x] = norma x <= 4
            aux (x:xs) = if norma x > 4
                         then False
                         else aux xs

mandelbrot_set_parallel :: [ℂ]->String
mandelbrot_set_parallel [x] = if s_c x then return '1'  else return '0'
mandelbrot_set_parallel (x:xs) =
  do
    head <- runEval (rpar [(s_c x)]);
    if head
      then return '1' ++ [' '] ++ (mandelbrot_set_parallel xs);
      else return '0' ++ [' '] ++ (mandelbrot_set_parallel xs);
      
