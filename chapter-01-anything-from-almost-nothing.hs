-- Things of note.
------------------
-- Lambda calculus has three basic components:
-- expressions - superset of all: a variable name, an abstraction, or combination of both
-- abstraction - function, lambda term that had head (lambda) and a body and is applied to an argument
-- variable

-- Nested lambda with free variables
-- λxy.xy
-- (λxy.xy)(λz.a)1
-- (λy.(λz.a)y)1
-- (λz.a)1

-- Different bindings and "shadowing" of variables
-- (λxy.xxy)(λx.xy)(λx.xz)
-- (λy.(λx.xy)(λx.xy)y)(λx.xz)
-- (λx.xy)(λx.xy)(λx.xz)
-- (λx.xy)y(λx.xz)
-- yy(λx.xz)

-- (λxyz.xz(yz))(λmn.m)(λp.p)
-- (λx.λy.λz.xz(yz))(λm.λn.m)(λp.p)
-- λz.(λm.λn.m)z((λp.p)z)
-- λz.(λn.z)((λp.p)z)
-- λz.z
--

-- Intermission: Equivalence Exercises
--------------------------------------
-- 1.
-- b) λmn.mz
-- Variable names do not matter for lambda calculus except for whether they match eachother.
--
-- 2.
-- c) λa.(λ.b.aab)
--
-- 3.
-- b) λtos.st

-- Chapter Exercises
--------------------

-- Combinators
--------------
-- 1. λx.xxx IS a combinator: all terms in the body appear in the head
-- 2. λxy.zx IS NOT a combinator: z appears in the body but not the head
-- 3. λxyz.xy(zx) IS a combinator: all terms in the body appear in the head
-- 4. λxyz.xy(zxy) IS a combinator: all terms in the body appear in the head
-- 5. λxy.xy(zxy) IS NOT a combinator: z appears in the body but not the head

-- Normal form or diverge?
--------------------------
-- 1. λx.xxx Can be reduced to normal form
-- 2. (λz.zz)(λy.yy) Diverges
-- 2. (λx.xxx)z Can be reduced to normal form

-- Combinators
--------------
-- 1. (λabc.cba)zz(λwv.w)
-- (λbc.cbz)z(λwv.w)
-- (λc.czz)(λwv.w)
-- (λwv.w)zz
-- (λv.z)z
-- z
--
-- 2. (λx.λy.xyy)(λa.a)b
-- (λy.(λa.a)yy)b
-- (λa.a)bb
-- bb
--
-- 3. (λy.y)(λx.xx)(λz.zq)
-- (λx.xx)(λz.zq)
-- (λz.zq)(λz.zq)
-- (λz.zq)q
-- qq
--
-- 4. (λz.z)(λz.zz)(λz.zy)
-- yy (because it is alpha equivalent to 3.)
--
-- 5. (λx.λy.xyy)(λy.y)y
-- (λy.(λy.y)yy)y
-- (λy.y)yy
-- yy
--
-- 6. (λa.aa)(λb.ba)c
-- (λb.ba)(λb.ba)c
-- (λb.ba)ac
-- aac
--
-- 7. (λxyz.xz(yz))(λx.z)(λx.a)
-- (λyz.(λx.z)z(yz))(λx.a)
-- λz.(λx.z)z(λx.a)z
-- λz.za
