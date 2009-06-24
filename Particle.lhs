> module Particle
>
> where
>
> import Vector

A particle is only acted on by gravity, though it does have a damping
factor applied which acts like drag. We only need to track the current
position, velocity, and damping factor:

> class Particle a where
>     parPosition :: a -> Position
>     parVelocity :: a -> Velocity 
>     parDamping :: a -> Damping
>     setPosition :: Position -> a -> a
>     setVelocity :: Velocity -> a -> a
>     setDamping :: Damping -> a -> a

> type Position = Vector2 
> type Velocity = Vector2 
> type Damping = Float

The world we simulate just consists of the particles in the simulation:

> type World a = [a]

Our simulation transforms the world from one state to the next, based on
how much time has passed:

> type Duration = Float
> simulate :: Particle a => Duration -> World a -> World a
> simulate dur world = map (step dur) world

Our @step@ function just updates one particle base on the force of
gravity for the duration given:

> step :: Particle a => Duration -> a -> a
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

> update :: Particle a => a -> Acceleration -> Duration -> a
> update p acc dur = setPosition p' . setVelocity v' $ p
>     where
>       pos = parPosition p
>       vel = parVelocity p
>       damp = parDamping p
>       p' = pos `vecAdd` dur `scale` vel
>       v' = vel `vecAdd` (damp ** dur) `scale` (dur `scale` acc)

