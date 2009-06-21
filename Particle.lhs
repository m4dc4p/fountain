> module Particle
>
> where
>
> import Vector

A particle is only acted on by gravity, though it does have a damping
factor applied which acts like drag. We only need to track the current
position, velocity, and damping factor:

> data Particle = Particle Position Velocity Damping
>               deriving Eq
>
> instance Show Particle where
>     show = showParticle
> 
> showParticle (Particle pos vel damp) = 
>     "(" ++ show (vecX pos) ++ ", " ++ show (vecY pos) ++ ") [" ++
>         show (vecX vel) ++ ", " ++ show (vecY vel) ++ 
>         "] {" ++ show damp ++ "}"

> type Position = Vector2 
> type Velocity = Vector2 
> type Damping = Float

The world we simulate just consists of the particles in the simulation:

> type World = [Particle]

Our simulation transforms the world from one state to the next, based on
how much time has passed:

> type Duration = Float
> simulate :: Duration -> World -> World
> simulate dur world = map (step dur) world

Our @step@ function just updates one particle base on the force of
gravity for the duration given:

> step :: Duration -> Particle -> Particle
> step dur p = update p gravity dur

Gravity pulls downwards at 10 $\frac{m}{s^2}$:

> type Acceleration = Vector2
> gravity :: Acceleration
> gravity = vector2 0 (-10)

We update position based on the constant velocity over the duration:

\[ p' = p + (dur vel)\]

And velocity based on constant acceleration. However, we also apply the
damping factor to cover up numeric inaccuracies:

\[ v' = v + (dur g) * {damping}^{dur} \].

> update :: Particle -> Acceleration -> Duration -> Particle
> update (Particle pos vel damp) acc dur = Particle p' v' damp
>   where
>       p' = pos `vecAdd` dur `scale` vel
>       v' = vel `vecAdd` (damp ** dur) `scale` (dur `scale` acc)

> p1 :: Particle
> p1 = Particle (vector2 0 0) (vector2 0 20) 0.995