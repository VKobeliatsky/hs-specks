module Specks
  ( shuffledSpecks
  , pickRandom
  , speckCoords
  , prettySpecks
  , Speck(Speck, Cursor)
  , GameField
  ) where

import           Control.Monad.State
import           Data.Array
import           Data.Foldable
import           System.Random

data Speck
  = Speck Int
  | Cursor
  deriving (Show, Eq)

type GameField = Array (Int, Int) Speck

pickRandom :: StateT [a] IO a
pickRandom = do
  arr <- get
  i <- lift
    $ getStdRandom
    $ randomR (1, length arr - 1)
  let (left, right) = splitAt i arr
  put $ init left ++ right
  return $ last left

shuffledSpecks :: (Int, Int) -> IO GameField
shuffledSpecks (m, n) = listArray ((0, 0), (m-1, n-1)) . fst <$> runStateT
  (mapM (\i -> if i == lastIndex then return Cursor else pickRandom) [0..lastIndex])
  (specks $ m * n)
    where
      lastIndex = m * n - 1
      specks count = map (\i -> if i == count then Cursor else Speck i) [1..count]

speckCoords :: Speck -> GameField -> Maybe (Int, Int)
speckCoords target field = foldl'
  (\result (coords, item) -> case result of
    Just _ -> result
    _      -> if item == target then Just coords else Nothing
  )
  Nothing $ assocs field

prettySpecks :: GameField -> String
prettySpecks field =
  let
    lastCol = snd . snd $ bounds field
  in
    seq lastCol
    $ foldMap
      (\((_, col), speck) -> show speck ++ if col == lastCol then "\n" else " \t")
      $ assocs field
