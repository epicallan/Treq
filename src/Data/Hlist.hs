-- | This module provides a Heterogeneous list used to represent HTTP request inputs and outputs
-- for some API type definitions
module Data.Hlist where

import Data.Kind (Type)

infixr 7 :.

-- * Hlist
data Hlist (a :: [Type]) where
  Nil :: Hlist '[]
  (:.) :: x -> Hlist xs -> Hlist (x ': xs)

singleton :: a -> Hlist '[ a ]
singleton = (:. Nil)

instance Show (Hlist '[]) where
  show _ = "Nil"

instance (Show (Hlist ts), Show a) => Show (Hlist (a ': ts)) where
  show (x :. xs) =  show x ++  " :. " ++ show xs
