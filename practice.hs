import Data.Maybe
import Data.Semigroup
import Data.Functor
-- Write the function reverseMaybe :: Maybe String -> Maybe String that
-- reverses a Maybe String and returns it as a Maybe String

reverseMaybe :: Maybe String -> Maybe String
reverseMaybe Nothing = Nothing
reverseMaybe (Just string) = Just (reverse string)

-- Making Maybe an instance of Functor
instance Functor Maybe where
    fmap func (Just n) = Just (func n)
    fmap func Nothing = Nothing 