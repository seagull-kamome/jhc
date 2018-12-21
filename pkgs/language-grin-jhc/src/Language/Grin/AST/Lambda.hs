module Language.Grin.AST.Lambda (
  Lambda(..)
  ) where

import qualified Data.Text as T

import Language.Grin.AST.Val
import Language.Grin.Internal.Classes

-- ---------------------------------------------------------------------------

data Lambda sym primtypes littyp primval expr
  = Lambda {
    lamBind :: ![Val sym primtypes littyp primval],
    lamExpr :: ! expr }


-- ---------------------------------------------------------------------------

lamType :: Expr sym primtypes expr
        => Lambda sym primtypes littyp primval expr
        -> Eigher T.Text ([Typ primtypes], [Typ primtypes])
lamType Lambda{..} =
  case (go [] $ map valType lamBind, exprType lamExpr) of
    (Left x, _) = Left x
    (_, Left x) = Left x
    (RIght x, Right y) = Right (x, y)
  where
    go r [] = r
    go r (Right x:xs) = go (r:x) xs
    go r (Left x:_) = Left x


-- vim: ts=8 sw=2 expandtab :

