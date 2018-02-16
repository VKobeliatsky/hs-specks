{-# LANGUAGE MultiWayIf #-}

module Specks
  ( shuffledSpecks
  , pickRandom
  , speckCoords
  , swap
  , neighbourSpecks
  , prettySpecks
  , Speck(Speck, Cursor)
  , Field
  , module Data.Array
  ) where

import           Control.Monad.State
import           Data.Array
import           Data.Foldable
import           System.Random

data Speck
  = Speck Int
  | Cursor
  deriving (Show, Eq)

type Coords = (Int, Int)

type Field = Array Coords Speck

pickRandom :: StateT [a] IO a
pickRandom = do
  arr <- get
  i <- lift
    $ getStdRandom
    $ randomR (1, length arr - 1)
  let (left, right) = splitAt i arr
  put $ init left ++ right
  return $ last left

shuffledSpecks :: Coords -> IO Field
shuffledSpecks (m, n) = listArray ((0, 0), (m-1, n-1)) <$> evalStateT
  (mapM (\i -> if i == lastIndex then return Cursor else pickRandom) [0..lastIndex])
  (specks $ m * n)
    where
      lastIndex = m * n - 1
      specks count = map (\i -> if i == count then Cursor else Speck i) [1..count]

speckCoords :: Speck -> Field -> Maybe Coords
speckCoords target field = foldl'
  (\result (coords, item) -> case result of
    Just _ -> result
    _      -> if item == target then Just coords else Nothing
  )
  Nothing $ assocs field

neighbourSpecks :: Coords -> Field -> [Speck]
neighbourSpecks (i, j) field =
  map (field!)
  $ filter
    (inRange (bounds field))
    [(i-1, j), (i, j+1), (i+1, j), (i, j-1)]

swap :: Coords -> Coords -> Field -> Field
swap i j field =
  field // map
    (\a@(x, _) -> if
      | x == i    -> (i, field!j)
      | x == j    -> (j, field!i)
      | otherwise -> a
    )
    (assocs field)

prettySpecks :: Field -> String
prettySpecks field =
  let
    lastCol = snd . snd $ bounds field
    render (Speck n) = show n
    render Cursor    = "_"
    separator col = if col == lastCol then "\n" else "\t"
  in
    seq lastCol
    $ foldMap
      (\((_, col), speck) -> render speck ++ separator col)
      $ assocs field
