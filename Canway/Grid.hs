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
rules c g x y = case c of Alive -> if n < 2 then Dead                           -- Rule 1
                                            else if n == 2 || n == 3 then Alive -- Rule 2
                                                                     else Dead  -- Rule 3
                          Dead -> if n == 3 then Alive else Dead	        -- Rule 4
    where n :: Int
          n = neighbors g x y

tick :: Grid -> Grid
tick g@(Grid v) = Grid $ V.imap (\ y row -> V.imap (\ x c -> rules c g x y) row) v

gen :: Int -> Grid -> Grid
gen n g
    | n <= 0 = g
    | otherwise = (gen $! n-1) $! tick g

mapGrid :: (Cell -> Cell) -> Grid -> Grid
mapGrid f (Grid v) = Grid $ V.map (V.map f) v

imapGrid :: (Int -> Int -> Cell -> Cell) -> Grid -> Grid
imapGrid f (Grid v) = Grid $ V.imap (\ y -> V.imap (\ x -> f x y)) v

mapGridM_ :: Monad m => (Cell -> m a) -> Grid -> m ()
mapGridM_ f (Grid v) = V.sequence_ $ V.map (V.mapM f) v

imapGridM_ :: Monad m => (Int -> Int -> Cell -> m a) -> Grid -> m ()
imapGridM_ f (Grid v) = V.sequence_ $ V.imap (\ y -> V.sequence_ . V.imap (\ x -> f x y)) v

period :: Grid -> Int
period g = periodAux g g 1 where
    periodAux og ng t = let ng' = tick ng
                        in if og == ng'
                              then t
                              else periodAux og ng' t+1

intersectGrids :: Grid -> Grid -> Grid
intersectGrids a b = let blank = fillGrid (max (width a) (width b)) (max (height a) (height b)) Dead
                     in imapGrid (\ x y _ -> intersectCells x y) blank
    where intersectCells :: Int -> Int -> Cell
          intersectCells x y = let maybeC1 = index a x y
                                   maybeC2 = index b x y
                                   maybeAnd = maybeC1 >>= \ c1 -> maybeC2 >>= \ c2 -> if (c1 == Alive) && (c2 == Alive) then return Alive else return Dead
                               in if (isJust maybeAnd) then (fromJust maybeAnd) else Dead

unionGrids :: Grid -> Grid -> Grid
unionGrids a b = let blank = fillGrid (max (width a) (width b)) (max (height a) (height b)) Dead
                 in imapGrid (\ x y _ -> unionCells x y) blank
    where unionCells :: Int -> Int -> Cell
          unionCells x y = let maybeC1 = index a x y
                               maybeC2 = index b x y
                               maybeOr = maybeC1 >>= \ c1 -> maybeC2 >>= \ c2 -> if (c1 == Alive) || (c2 == Alive) then return Alive else return Dead
                           in if (isJust maybeOr) then (fromJust maybeOr) else Dead

shiftGrid :: Int -> Int -> Grid -> Grid
shiftGrid x y g = let blank = fillGrid (width g + x) (height g + y) Dead
                  in imapGrid (\ x' y' _ -> getCell g (width g - x') (height g - y')) blank
    where getCell :: Grid -> Int -> Int -> Cell
          getCell h x'' y'' = case index h x'' y'' of
                                   Just c  -> c
                                   Nothing -> Dead

embiggen :: Grid -> Grid
embiggen g = shiftGrid 25 25 $ unionGrids g $ fillGrid 25 25 Dead

blinker :: Grid
blinker = fromList [    "     ",
                        "  X  ",
                        "  X  ",
                        "  X  ",
                        "     "]

toad :: Grid
toad = fromList [       "      ",
                        "      ",
                        "  XXX ",
                        " XXX  ",
                        "      ",
                        "      "]

beacon :: Grid
beacon = fromList [     "      ",
                        " XX   ",
                        " X    ",
                        "    X ",
                        "   XX ",
                        "      "]

pulsar :: Grid
pulsar = fromList [     "                 ",
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

clock :: Grid
clock = fromList [ "              ",
                   "       XX     ",
                   "       XX     ",
                   "              ",
                   "     XXXX     ",
                   " XX X  X X    ",
                   " XX X X  X    ",
                   "    X X  X XX ",
                   "    X    X XX ",
                   "     XXXX     ",
                   "              ",
                   "     XX       ",
                   "     XX       ",
                   "              "]

gosperGliderGun :: Grid
gosperGliderGun = fromList [    "                                      ",
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
                                
gosperGliderGunBig :: Grid
gosperGliderGunBig = unionGrids gosperGliderGun (fillGrid 50 50 Dead)

infinite1 :: Grid
infinite1 = fromList [  "          ",
                        "       X  ",
                        "     X XX ",
                        "     X X  ",
                        "     X    ",
                        "   X      ",
                        " X X      ",
                        "          "]

infinite1Big :: Grid
infinite1Big = embiggen infinite1

infinite2 :: Grid
infinite2 = fromList [  "       ",
                        " XXX X ",
                        " X     ",
                        "    XX ",
                        "  XX X ",
                        " X X X ",
                        "       "]

infinite2Big :: Grid
infinite2Big = embiggen infinite2

methusela :: Grid
methusela = fromList [  "          ",
                        " XX   X X ",
                        " XX    X  ",
                        "       X  ",
                        "          "]

methuselaBig :: Grid
methuselaBig = embiggen methusela

maxFill :: Grid
maxFill = fromList [    "                             ",
                        "                   X         ",
                        "                  XXX        ",
                        "             XXX    XX       ",
                        "            X  XXX  X XX     ",
                        "           X   X X  X X      ",
                        "           X    X X X X XX   ",
                        "             X    X X   XX   ",
                        " XXXX     X X    X   X XXX   ",
                        " X   XX X XXX XX         XX  ",
                        " X     XX     X              ",
                        "  X  XX X  X  X XX           ",
                        "        X X X X X X     XXXX ",
                        "  X  XX X  X  X  XX X XX   X ",
                        " X     XX   X X X   XX     X ",
                        " X   XX X XX  X  X  X XX  X  ",
                        " XXXX     X X X X X X        ",
                        "           XX X  X  X XX  X  ",
                        "              X     XX     X ",
                        "  XX         XX XXX X XX   X ",
                        "   XXX X   X    X X     XXXX ",
                        "   XX   X X    X             ",
                        "   XX X X X X    X           ",
                        "      X X  X X   X           ",
                        "     XX X  XXX  X            ",
                        "       XX    XXX             ",
                        "        XXX                  ",
                        "         X                   ",
                        "                             "]

maxFillBig :: Grid
maxFillBig = shiftGrid 20 20 $ unionGrids maxFill $ fillGrid 40 40 Dead