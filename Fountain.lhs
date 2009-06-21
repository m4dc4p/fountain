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
>     map snd $ scanl next (rnds, []) (repeat ())
>   where
>     next (rs, w) _ = 
>         let vis = filter visible w
>             new = take (cnt - length vis) . map newParticles $ rs
>             w' = simulate dur (vis ++ new)
>         in (drop (length new) rs, w')
>     visible (Particle pos _ _) = vecY pos > (-10)
>     newParticles rnd = Particle (vector2 0 0) 
>                                 (vector2 (1 * rnd) 
>                                          (10 + 10 * rnd))
>                                 (0.8 + 0.1 * rnd)
>                                 

> run = do
>   r <- getStdGen
>   let pos (Particle p _ _) = p
>   mapM_ (putStrLn . show) . map (map pos) $ fountain dur 30 (randomRs (-1, 1) r)

> dur :: Duration
> dur = 1 / 30


