module RungeKutta where

rk4 :: Floating a => (a -> a) -> a -> a -> a
rk4 f h x = x + (1/6) * (k1 + 2*k2 + 2*k3 + k4)
            where k1 = h * f (x)
                  k2 = h * f (x + 0.5*k1)
                  k3 = h * f (x + 0.5*k2)
                  k4 = h * f (x + k3)
