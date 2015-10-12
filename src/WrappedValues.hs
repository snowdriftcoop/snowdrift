-- | A module to encapsulate this WrappedValues idea. It provides an
-- overloaded method for getting values out of Entity:s and Value:s and all
-- tuples, lists, and Maybes thereof.
--
-- I am not especially fond of this idea, and I think it indicates we need
-- better model abstractions.
module WrappedValues (WrappedValues(..)) where

import Prelude
import Database.Persist
import Database.Esqueleto (Value(..))

class WrappedValues a where
    type Unwrapped a
    unwrapValues :: a -> Unwrapped a

instance WrappedValues (Entity a) where
    type Unwrapped (Entity a) = (Entity a)
    unwrapValues = id

instance WrappedValues (Value a) where
    type Unwrapped (Value a) = a
    unwrapValues (Value v) = v

instance (WrappedValues a, WrappedValues b) => WrappedValues (a, b) where
    type Unwrapped (a, b) = (Unwrapped a, Unwrapped b)
    unwrapValues (a, b) = (unwrapValues a, unwrapValues b)

instance (WrappedValues a, WrappedValues b, WrappedValues c) => WrappedValues (a, b, c) where
    type Unwrapped (a, b, c) = (Unwrapped a, Unwrapped b, Unwrapped c)
    unwrapValues (a, b, c) = (unwrapValues a, unwrapValues b, unwrapValues c)

instance (WrappedValues a, WrappedValues b, WrappedValues c, WrappedValues d) => WrappedValues (a, b, c, d) where
    type Unwrapped (a, b, c, d) = (Unwrapped a, Unwrapped b, Unwrapped c, Unwrapped d)
    unwrapValues (a, b, c, d) = (unwrapValues a, unwrapValues b, unwrapValues c, unwrapValues d)

instance ( WrappedValues a
         , WrappedValues b
         , WrappedValues c
         , WrappedValues d
         , WrappedValues e
         ) => WrappedValues (a, b, c, d, e) where
    type Unwrapped (a, b, c, d, e) =
        ( Unwrapped a
        , Unwrapped b
        , Unwrapped c
        , Unwrapped d
        , Unwrapped e
        )
    unwrapValues (a, b, c, d, e) =
        ( unwrapValues a
        , unwrapValues b
        , unwrapValues c
        , unwrapValues d
        , unwrapValues e
        )

instance ( WrappedValues a
         , WrappedValues b
         , WrappedValues c
         , WrappedValues d
         , WrappedValues e
         , WrappedValues f
         ) => WrappedValues (a, b, c, d, e, f) where
    type Unwrapped (a, b, c, d, e, f) =
        ( Unwrapped a
        , Unwrapped b
        , Unwrapped c
        , Unwrapped d
        , Unwrapped e
        , Unwrapped f
        )
    unwrapValues (a, b, c, d, e, f) =
        ( unwrapValues a
        , unwrapValues b
        , unwrapValues c
        , unwrapValues d
        , unwrapValues e
        , unwrapValues f
        )

instance WrappedValues a => WrappedValues [a] where
    type Unwrapped [a] = [Unwrapped a]
    unwrapValues = map unwrapValues

instance WrappedValues a => WrappedValues (Maybe a) where
    type Unwrapped (Maybe a) = Maybe (Unwrapped a)
    unwrapValues = fmap unwrapValues
