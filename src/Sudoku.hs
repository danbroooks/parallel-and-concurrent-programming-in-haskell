module Sudoku (solve, printGrid) where

import           Data.Array
import qualified Data.Text as T
import           Prelude
import qualified Prelude.List as L

-- Types
type Digit  = Char
type Square = (Char,Char)
type Unit   = [Square]

-- We represent our grid as an array
type Grid = Array Square [Digit]

-- Setting Up the Problem
digits :: [Char]
digits = "123456789"

box :: ((Char, Char), (Char, Char))
box = (('A','1'),('I','9'))

cross :: [Char] -> [Char] -> [Square]
cross rows cols = do
  r <- rows
  c <- cols
  return (r,c)

squares :: [Square]
squares = cross rows digits  -- [('A','1'),('A','2'),('A','3'),...]
  where
    rows = "ABCDEFGHI"

peers :: Array Square [Square]
peers = array box [(s, set (units!s)) | s <- squares ]
      where
        set = L.ordNub . concat

unitlist :: [Unit]
unitlist = [ cross rows [c] | c <- digits ] ++
            [ cross [r] digits | r <- rows ] ++
            [ cross rs cs | rs <- ["ABC","DEF","GHI"],
                            cs <- ["123","456","789"]]
  where
    rows = "ABCDEFGHI"

-- this could still be done more efficiently, but what the heck...
units :: Array Square [Unit]
units = array box [(s, [filter (/= s) u | u <- unitlist, s `elem` u ]) |
                    s <- squares]

allPossibilities :: Grid
allPossibilities = array box [ (s, digits) | s <- squares ]

-- Parsing a grid into an Array
parsegrid :: [Char] -> Maybe Grid
parsegrid g = do
  _ <- regularGrid
  foldM assign allPossibilities (zip squares g)
  where
    regularGrid =
      if all (`elem` validChars) g
         then Just g
         else Nothing

    validChars :: [Char]
    validChars =
      "0.-123456789"

-- Propagating Constraints
assign        :: Grid -> (Square, Digit) -> Maybe Grid
assign g (s,d) = if d `elem` digits
                 -- check that we are assigning a digit and not a '.'
                  then do
                    let ds = g ! s
                        toDump = L.remove d ds
                    foldM eliminate g (zip (repeat s) toDump)
                  else return g

eliminate     ::  Grid -> (Square, Digit) -> Maybe Grid
eliminate g (s,d) = 
  let cell = g ! s in
  if d `notElem` cell then return g -- already eliminated
  -- else d is deleted from s' values
    else do let newCell = L.remove d cell
                newV = g // [(s,newCell)]
            newV2 <- case newCell of
            -- contradiction : Nothing terminates the computation
                 []   -> Nothing
            -- if there is only one value left in s, remove it from peers
                 [d'] -> do let peersOfS = peers ! s
                            foldM eliminate newV (zip peersOfS (repeat d'))
            -- else : return the new grid
                 _    -> return newV
            -- Now check the places where d appears in the peers of s
            foldM (locate d) newV2 (units ! s)

locate :: Digit -> Grid -> Unit -> Maybe Grid
locate d g u = case filter ((d `elem`) . (g !)) u of
                []  -> Nothing
                [s] -> assign g (s,d)
                _   -> return g

-- Search
search :: Grid -> Maybe Grid
search g = 
  case [(l,(s,xs)) | (s,xs) <- assocs g, let l = length xs, l /= 1] of
            [] -> return g
            ls -> do let (_,(s,ds)) = minimum ls
                     msum [assign g (s,d) >>= search | d <- ds]

solve :: Text -> Maybe Grid
solve = search <=< parsegrid . T.unpack

-- Display solved grid
printGrid :: Grid -> IO ()
printGrid = putStrLn . gridToString

gridToString :: Grid -> Text
gridToString g = T.unlines $ T.pack <$> l5
  where
    l0 = elems g
    -- [("1537"),("4"),...]
    l1 = (map (\s -> " " ++ s ++ " ")) l0
    -- ["1 "," 2 ",...]
    l2 = (map concat . sublist 3) l1
    -- ["1  2  3 "," 4  5  6 ", ...]
    l3 = (sublist 3) l2
    -- [["1  2  3 "," 4  5  6 "," 7  8  9 "],...]
    l4 = (map (concat . intersperse "|")) l3
    -- ["1  2  3 | 4  5  6 | 7  8  9 ",...]
    l5 = (concat . intersperse [line] . sublist 3) l4
    line = hyphens ++ "+" ++ hyphens ++ "+" ++ hyphens
    hyphens = replicate 9 '-'

sublist :: Int -> [a] -> [[a]]
sublist _ [] = []
sublist n xs = ys : sublist n zs
  where
    (ys,zs) = splitAt n xs
