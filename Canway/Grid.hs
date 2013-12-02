module Canway.Grid where

import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Vector as V
import Data.Vector ((!?), (!), (//))

data Cell = Dead | Alive deriving (Eq)
-- Use sequences for constant-time random access
data Grid = Grid (V.Vector (V.Vector Cell)) deriving (Eq)

instance Show Cell where
	show c = show $ toChar c

instance Show Grid where
	show (Grid v) = intercalate "\n" $ V.toList $ V.map (V.toList . (V.map toChar)) v

toChar :: Cell -> Char
toChar Dead = ' '
toChar Alive = 'X'

toggle :: Cell -> Cell
toggle Alive = Dead
toggle Dead = Alive

height :: Grid -> Int
height (Grid v) = V.length v

width :: Grid -> Int
width (Grid v) = V.length $ v ! 0

dimens :: Grid -> (Int, Int)
dimens g = (width g, height g)

index :: Grid -> Int -> Int -> Maybe Cell
index (Grid v) x y = (v !? y) >>= \ row -> row !? x

updateToggle :: Grid -> Int -> Int -> Maybe Grid
updateToggle g x y = index g x y >>= \ c -> update g x y (toggle c)

update :: Grid -> Int -> Int -> Cell -> Maybe Grid
update (Grid v) x y c = do
	row <- (v !? y)
	row !? x
	Just $ Grid $ v // [(y, row // [(x, c)])]

fillGrid :: Int -> Int -> Cell -> Grid
fillGrid x y = Grid . V.replicate y . V.replicate x

-- When reading from the list, any whitespace character is interpreted as "dead,"
-- and all other characters are interpreted as "alive".
fromList :: [[Char]] -> Grid
fromList [] = error "Grid must be non-empty"
fromList cs = Grid $ V.fromList $ map (V.fromList . map spaceDead) cs
	where spaceDead c = if isSpace c then Dead else Alive

-- The number of living neighbors
neighbors :: Grid -> Int -> Int -> Int
neighbors g x y = length $ filter (== Alive) [fromJust nc | dy <- [-1,0,1], dx <- [-1,0,1], dx /= 0 || dy /= 0, let nc = index g (x+dx) (y+dy), isJust nc]

rules :: Cell -> Grid -> Int -> Int -> Cell
rules c g x y = case c of Alive -> if n < 2 then Dead			-- Rule 1
				   else if n == 2 || n == 3 then Alive	-- Rule 2
				   else Dead				-- Rule 3
			  Dead -> if n == 3 then Alive else Dead	-- Rule 4
	where n = neighbors g x y

tick :: Grid -> Grid
tick g@(Grid v) = Grid $ V.imap (\ y row -> V.imap (\ x c -> rules c g x y) row) v

gen :: Int -> Grid -> Grid
gen n g
	| n <= 0 = g
	| otherwise = (gen $! n-1) $! tick g

mapGridM_ :: Monad m => (Cell -> m a) -> Grid -> m ()
mapGridM_ f (Grid v) = V.sequence_ $ V.map (V.mapM f) v

imapGridM_ :: Monad m => (Int -> Int -> Cell -> m a) -> Grid -> m ()
imapGridM_ f (Grid v) = V.sequence_ $ V.imap (\ y -> V.sequence_ . V.imap (\ x -> f x y)) v

period :: Grid -> Int
period g = periodAux g g 1 where
	periodAux og ng t = let ng' = tick ng in
			 if og == ng' then t else periodAux og ng' t+1

blinker :: Grid
blinker = fromList [	"     ",
			"  X  ",
			"  X  ",
			"  X  ",
			"     "]

toad :: Grid
toad = fromList [	"      ",
			"      ",
			"  XXX ",
			" XXX  ",
			"      ",
			"      "]

beacon :: Grid
beacon = fromList [	"      ",
			" XX   ",
			" X    ",
			"    X ",
			"   XX ",
			"      "]
	
pulsar :: Grid
pulsar = fromList [	"                 ",
			"                 ",
			"    XXX   XXX    ",
			"                 ",
			"  X    X X    X  ",
			"  X    X X    X  ",
			"  X    X X    X  ",
			"    XXX   XXX    ",
			"                 ",
			"    XXX   XXX    ",
			"  X    X X    X  ",
			"  X    X X    X  ",
			"  X    X X    X  ",
			"                 ",
			"    XXX   XXX    ",
			"                 ",
			"                 "]

gosperGliderGun :: Grid
gosperGliderGun = fromList [	"                                      ",
				"                         X            ",
				"                       X X            ",
				"             XX      XX            XX ",
				"            X   X    XX            XX ",
				" XX        X     X   XX               ",
				" XX        X   X XX    X X            ",
				"           X     X       X            ",
				"            X   X                     ",
				"             XX                       ",
				"                                      "]