{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.State
import qualified Data.Matrix         as Matrix
import qualified Data.Vector         as Vector
import           System.Random

data Speck
  = Speck Int
  | Cursor
  deriving (Show, Eq)

type GameField = Matrix.Matrix Speck

specks :: Int -> Vector.Vector Speck
specks count = Vector.generate count $ Speck . (+1)

pickRandom :: StateT (Vector.Vector a) IO a
pickRandom = do
  v <- get
  i <- lift
    $ getStdRandom
    $ randomR (1, Vector.length v)
  let (left, right) = Vector.splitAt i v
  put $ Vector.concat [Vector.init left, right]
  return $ Vector.last left

shuffledSpecks :: (Int, Int) -> IO GameField
shuffledSpecks (m, n) = fst <$> runStateT
  (sequence $ Matrix.matrix m n speckOrCursor)
  (specks $ m * n - 1)
    where
      speckOrCursor (i, j) = if i * j == m * n
        then return Cursor
        else pickRandom

matrixElemIndex :: Eq a => a -> Matrix.Matrix a -> Maybe (Int, Int)
matrixElemIndex target matrix = Vector.ifoldr
  (\j vector result -> maybe
    result (\i -> Just (i+1, j+1))
    $ Vector.elemIndex target vector
  ) Nothing
  $ matrixToVectors matrix
  where
    matrixToVectors m = Vector.generate
      (Matrix.ncols m)
      $ flip Matrix.getCol m . (+1)

main :: IO ()
main = do
  m <- shuffledSpecks (2, 4)
  putStrLn $ Matrix.prettyMatrix m
  let target = matrixElemIndex Cursor m
  print target
  print $ maybe Nothing (return . (flip $ uncurry Matrix.getElem) m) target
