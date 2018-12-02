module Info.Arity (Arity(..)) where
import Data.Dynamic

-- | how many arguments a function my be applied to before it performs work and whether it bottoms out after that many arguments
data Arity = Arity Int Bool deriving(Typeable,Show,Ord,Eq)

