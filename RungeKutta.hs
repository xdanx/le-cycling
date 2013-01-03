{- RungeKutta.hs
Numerically solves the differential equation from the second paper.
To compute the speed of a cyclist given his pped.
-}
{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}

module RungeKutta(updateCyclistSpeed) where

import GHC.Exts

import Cyclist(Cyclist(Cyclist), speed)

rk4 :: Double# -> Double# -> Double#
rk4 !s !pped = rk4' pped (-60.0##) h 0.0## s y0
    where !h = 0.01##
          !y0 = 0.5## *## s *## s

rk4' :: Double# -> Double# -> Double# -> Double# -> Double# -> Double# -> Double#
rk4' !pped !t !h !e !xn !yn = if cont
                                then rk4' pped (t +## h) h e xnp1 ynp1
                                else xnp1
                                where cont = if h <## 0.0##
                                                then t >## e
                                                else t <## e
                                      h2   = h /## 2.0##
                                      h6   = h /## 6.0##
                                      k11  = f pped t xn yn
                                      k12  = g pped t xn yn
                                      k21  = f pped (t +## h2) (xn +## h2 *## k11) (yn +## h2 *## k12)
                                      k22  = g pped (t +## h2) (xn +## h2 *## k11) (yn +## h2 *## k12)
                                      k31  = f pped (t +## h2) (xn +## h2 *## k21) (yn +## h2 *## k22)
                                      k32  = g pped (t +## h2) (xn +## h2 *## k21) (yn +## h2 *## k22)
                                      k41  = f pped (t +## h)  (xn +## h  *## k31) (yn +## h  *## k32)
                                      k42  = g pped (t +## h)  (xn +## h  *## k31) (yn +## h  *## k32)
                                      xnp1 = xn +## h6 *## (k11 +## 2.0## *## k21 +## 2.0## *## k31 +## k41)
                                      ynp1 = yn +## h6 *## (k12 +## 2.0## *## k22 +## 2.0## *## k32 +## k42)


updateCyclistSpeed :: Cyclist -> Double -> Cyclist
updateCyclistSpeed !c !(D# pped) = c{speed = D# (rk4 s pped)}
                   where !(D# s) = speed c

f :: Double# -> Double# -> Double# -> Double# -> Double#
f !pped !t !y_1 !y_2 = (pped -## 15.1533## *## y_1 **## 3.0##) /## (2968.8050577## *## (sqrtDouble# (2.0## *## y_2)))

g :: Double# -> Double# -> Double# -> Double# -> Double#
g !pped !t !y_1 !y_2 = (pped -## 15.1533## *## y_1 **## 3.0##) /## 2968.8050577##
