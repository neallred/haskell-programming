-- learn.hs

module Learn where

x = 10 * 5 + y

myResult = x * 5

y = 10



-- good
-- let
--   x = 3
--   y = 4

-- good
-- let x = 3
--     y = 4

-- bad
-- let
--   x = 3
--     y = 4

-- good
foo x =
  let y = x * 2
      z = x ^ 2
  in 2 * y * z


-- bad, next line needs at least one space of indent
-- variableHere = 10
-- * 5 + y

-- div  divide    integral division, round down
-- mod  modulo    like ‘rem’, but after modular division
-- quot quotient  integral division, round towards zero
-- rem  remainder remainder after division

-- rem is the complement to quot
-- mod is the complement to div
