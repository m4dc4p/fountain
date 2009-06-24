> module Fountain
>
> where 
>
> import Particle
> import Vector
> import System.Random
> import Data.List (unfoldr)

> data FP = FP Position Velocity Damping
>              deriving Eq
>              
> instance Show FP where
>     show = showParticle

> showParticle (FP pos vel damp) = 
>     "(" ++ show (vecX pos) ++ ", " ++ show (vecY pos) ++ ") [" ++
>     show (vecX vel) ++ ", " ++ show (vecY vel) ++ 
>     "] {" ++ show damp ++ "}"


> instance Particle FP where
>     parPosition (FP pos _ _ ) = pos
>     parVelocity (FP _ vel _ ) = vel
>     parDamping (FP _ _ d) = d
>     setPosition pos (FP _ v d) = FP pos v d
>     setVelocity vel (FP p _ d) = FP p vel d
>     setDamping damp (FP p v _) = FP p v damp

> type NumParticles = Int
> type FountainState s = (s, World FP)
> type Elapsed = Float

> fountainState :: FountainState ([Float], Elapsed, NumParticles, Duration) -> FountainState ([Float], Elapsed, NumParticles, Duration)
> fountainState ((rs, elapsed, cnt, dur), w) =
>         let vis = filter visible w
>             rate = 0.2
>             new = if elapsed > rate || null vis
>                    then take 2 . map newParticle $ unfoldr (takes 3) rs
>                    else []
>             elapsed' = if elapsed > rate || null vis
>                         then 0
>                         else elapsed + dur
>             takes n rs = Just (take n rs, drop n rs)
>             w' = take cnt $ vis ++ new
>             visible (FP pos _ _) = vecY pos > -1
>             newParticle [r1,r2,r3] = FP (vector2 0 0) 
>                                         (vector2 (1 * r1) 
>                                                  (10 + 2 * r2))
>                                         (0.8 + 0.1 * r3)
>         in ((drop (length new * 3) rs, elapsed', cnt, dur), w')
> 

> fountain :: (FountainState s -> FountainState s) -> s -> [World FP]
> fountain f i = drop 1 . map snd . 
>                scanl (\s _ -> f s) (i, []) $ repeat ()
>     

> run = do
>   r <- getStdGen
>   return $ fountain fountainState ((randomRs (-1, 1) r), 0, 100, 1 / 30)



