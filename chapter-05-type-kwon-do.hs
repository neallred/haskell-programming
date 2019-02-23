data Woot

data Blah

f :: Woot -> Blah
f = undefined

g :: (Blah, Woot) -> (Blah, Blah)
g (b, w) = (b, b)

num1f :: Int -> String
num1f = undefined

num1g :: String -> Char
num1g = undefined

num1h :: Int -> Char
num1h x = g (f x)

data A

data B

data C

num2q :: A -> B
num2q = undefined

num2w :: B -> C
num2w = undefined

num2e :: A -> C
num2e a = num2w (num2q a)

data X

data Y

data Z

num3xz :: X -> Z
num3xz = undefined

num3yz :: Y -> Z
num3yz = undefined

num3xform :: (X, Y) -> (Z, Z)
num3xform (x, y) = (num3xz x, num3yz y)

num4unge :: (x -> y) -> (y -> (w, z)) -> x -> w
num4unge xToY yToWZ x = fst (yToWZ (xToY x))
