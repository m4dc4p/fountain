module Vector (Vector2, vecAdd, scale, vector2, vecX, vecY)

where

newtype Vector2 = V2 (Float, Float)
    deriving (Eq)

instance Show Vector2 where
    show = showVector

showVector (V2 (x, y)) = "(" ++ show x ++ ", " ++ show y ++ ")"

infixl 6 `vecAdd`
infixl 7 `scale`

vecAdd :: Vector2 -> Vector2 -> Vector2
vecAdd (V2 (x1, y1)) (V2 (x2, y2)) = V2 (x1 + x2, y1 + y2)

scale :: Float -> Vector2 -> Vector2
scale s (V2 (x1, y1)) = V2 (x1 * s, y1 * s)

vector2 :: Float -> Float -> Vector2
vector2 x y = V2 (x, y)

vecX (V2 (x, y)) = x

vecY (V2 (x, y)) = y

