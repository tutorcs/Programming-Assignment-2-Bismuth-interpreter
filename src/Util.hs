https://tutorcs.com
WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
module Util (todo, liftA4) where

import Control.Applicative

-- | This is a placeholder value to represent the parts of the program you
-- should implement.
todo :: a
todo = undefined

liftA4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = pure f <*> a <*> b <*> c <*> d
