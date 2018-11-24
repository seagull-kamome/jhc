module Support.Unparse(Unparse(), Unparsable(..), unparse, unparse', Side(..), atom, atomize, bop, pop, fixitize) where

import Text.PrettyPrint.ANSI.Leijen

data Unparse a = Atom a | Pre a (Unparse a) | Fix (Unparse a) a (Unparse a) !Side !Int | Atomized (Unparse a) | Fixitized  !Side !Int (Unparse a)
data Side = R | L | N deriving(Eq)


atom :: a -> Unparse a
atom = Atom

atomize :: Unparse a -> Unparse a
atomize x@(Atomized _) = x
atomize x@(Atom _) = x
atomize x = Atomized x

fixitize :: (Side,Int) -> Unparse a -> Unparse a
fixitize (s,i) = Fixitized s i

pop :: a -> Unparse a -> Unparse a
pop = Pre

bop :: (Side,Int) -> a -> Unparse a -> Unparse a -> Unparse a
bop (s,i) op a b = Fix a op b s i

data Unparsable a = Unparsable {
    unparseGroup :: a -> a,
    unparseCat :: a -> a -> a
    }

data Fix = FAtom | FPre | FFix !Side !Int

--unparse :: DocLike a => Unparse a -> a
unparse :: Unparse Doc -> Doc
unparse = unparse' Unparsable { unparseGroup = parens, unparseCat = (<>) }

unparse' :: Unparsable a -> Unparse a -> a
unparse' Unparsable { unparseGroup = upg, unparseCat = (<>) } up = fst $ f up where
    f (Atom a) = atom a
    f (Atomized a) = (fst $ f a, FAtom)
    f (Fixitized s i a) = (fst $ f a, FFix s i)
    f (Pre a up) = pop a (f up)
    f (Fix a op b s i) = bop (s,i) op (f a) (f b)

    bop (f1,f2) s (a,FAtom) (b,FAtom)  = (sop s a b, FFix f1 f2)
    bop f@(f1,f2) s (a,af) (b,bf) | lts L f af  && lts R f bf  = (sop s a b, FFix f1 f2)
    bop f s (a,af) b | not (lts L f af) = bop f s (mkatom (a,af)) b
    bop f s a (b,bf) | not (lts R f bf)  = bop f s a (mkatom (b,bf))
    bop _ _ _ _ = error "bop"

    pop s (x, FAtom) = ( s <> x, FPre)
    pop s x = pop s $ mkatom x

    atom a = (a,FAtom)
    mkatom (a,FAtom) = (a,FAtom)
    mkatom (a,_) = ( upg a , FAtom)

    sop op a b = a <> (op <> b)

    lts :: Side -> (Side,Int) -> Fix -> Bool
    lts _ _ FAtom = True
    lts _ _ FPre = True
    lts _ (_,n') (FFix  _ n ) | n' /= n = n' < n
    lts R (R,_) (FFix  R _ ) = True
    lts L (L,_) (FFix  L _ ) = True
    lts _ _ _ = False


