> module Fountain
>
> where 
>
> import Particle
> import Vector
> import System.Random
> import Data.List (unfoldr)

> type NumParticles = Int
>
> fountain :: Duration -> NumParticles -> [Float] -> [World]
> fountain dur cnt rnds = 
>     drop 1 . map snd . scanl next ((rnds, 0), []) $ repeat ()
>   where
>     next ((rs, elapsed), w) _ = 
>         let vis = filter visible w
>             rate = 0.2
>             new = if elapsed > rate || null vis
>                    then take 2 . map newParticle $ unfoldr (takes 3) rs
>                    else []
>             elapsed' = if elapsed > rate || null vis
>                         then 0
>                         else elapsed + dur
>             takes n rs = Just (take n rs, drop n rs)
>             w' = simulate dur (take cnt $ vis ++ new)
>         in ((drop (length new * 3) rs, elapsed'), w')
>     visible (Particle pos _ _) = vecY pos > -1
>     newParticle [r1,r2,r3] = Particle (vector2 0 0) 
>                                 (vector2 (1 * r1) 
>                                          (10 + 2 * r2))
>                                 (0.8 + 0.1 * r3)
>                                 

> run = do
>   r <- getStdGen
>   return $ fountain dur 100 (randomRs (-1, 1) r)

> dur :: Duration
> dur = 1 / 30


