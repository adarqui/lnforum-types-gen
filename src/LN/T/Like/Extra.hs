module LN.T.Like.Extra (
  (+->)
) where



import           Data.Text (Text)
import qualified Data.Text as T (toLower)
import           Prelude



infixr 9 +->

(+->) :: a -> [a] -> [a]
(+->) a as = a : as
