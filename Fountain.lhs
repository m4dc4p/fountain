> module Fountain
>
> where 
>
> import Particle
> import Vector
> import System.Random

> type NumParticles = Int
>
> fountain :: Duration -> NumParticles -> [Float] -> [World]
> fountain dur cnt rnds = 
>     drop 1 . map snd . scanl next (rnds, []) $ repeat ()
>   where
>     next (rs, w) _ = 
>         let vis = filter visible w
>             new = take (cnt - length vis) . map newParticles $ rs
>             w' = simulate dur (vis ++ new)
>         in (drop (length new) rs, w')
>     visible (Particle pos _ _) = vecY pos > -1
>     newParticles rnd = Particle (vector2 0 0) 
>                                 (vector2 (1 * rnd) 
>                                          (10 + 2 * rnd))
>                                 (0.8 + 0.1 * rnd)
>                                 

> run = do
>   r <- getStdGen
>   return $ fountain dur 10 (randomRs (-1, 1) r)

> dur :: Duration
> dur = 1 / 30


