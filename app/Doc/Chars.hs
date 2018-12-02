-- | A variety of useful constant documents representing many unicode characters.
module Doc.Chars where

import Data.Char(chr)

ulCorner, llCorner, urCorner, lrCorner, rTee, lTee, bTee, tTee, hLine,
 vLine, plus, s1, s9, diamond, ckBoard, degree, plMinus, bullet, lArrow,
 rArrow, dArrow, uArrow, board, lantern, block, s3, s7, lEqual, gEqual,
 pi, nEqual, sterling, coloncolon, alpha, beta, lambda, forall, exists,
 box, bot, bottom, top, pI, lAmbda, star, elem, notElem, and, or, sqoparen, sqcparen  :: Char

ulCorner  = chr 0x250C
llCorner = chr 0x2514
urCorner = chr 0x2510
lrCorner = chr 0x2518
rTee     = chr 0x2524
lTee     = chr 0x251C
bTee     = chr 0x2534
tTee     = chr 0x252C
hLine    = chr 0x2500
vLine    = chr 0x2502
plus     = chr 0x253C
s1       = chr 0x23BA -- was: 0xF800
s9       = chr 0x23BD -- was: 0xF804
diamond  = chr 0x25C6
ckBoard  = chr 0x2592
degree   = chr 0x00B0
plMinus  = chr 0x00B1
bullet   = chr 0x00B7
lArrow   = chr 0x2190
rArrow   = chr 0x2192
dArrow   = chr 0x2193
uArrow   = chr 0x2191
board    = chr 0x2591
lantern  = chr 0x256C
block    = chr 0x2588
s3       = chr 0x23BB -- was: 0xF801
s7       = chr 0x23BC -- was: 0xF803
lEqual   = chr 0x2264
gEqual   = chr 0x2265
pi       = chr 0x03C0
nEqual   = chr 0x2260
sterling = chr 0x00A3

coloncolon = chr 0x2237  -- ∷

alpha    = chr 0x03b1  -- α
beta     = chr 0x03b2  -- β


lambda   = chr 0x03bb  -- λ
forall   = chr 0x2200  -- ∀
exists   = chr 0x2203  -- ∃
box      = chr 0x25a1  -- □

bot      = chr 0x22a5  -- ⊥
bottom   = chr 0x22a5  -- ⊥
top      = chr 0x22a4  -- T
pI       = chr 0x03a0
lAmbda   = chr 0x039b  -- Λ  (capital λ)
and      = chr 0x2227  -- ∧
or       = chr 0x2228  -- ∨
star     = chr 0x22c6
elem     = chr 0x2208  -- ∈
notElem  = chr 0x2209

sqoparen = chr 0x3014  -- 〔
sqcparen = chr 0x3015  --  〕

