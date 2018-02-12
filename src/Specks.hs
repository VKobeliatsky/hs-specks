module Specks
  ( shuffledSpecks
  , speckIndex
  , prettySpecks
  , Speck(Speck, Cursor)
  , GameField
  ) where

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

speckIndex :: Speck -> GameField -> Maybe (Int, Int)
speckIndex target matrix = Vector.ifoldr
  (\j vector result -> maybe
    result (\i -> Just (i+1, j+1))
    $ Vector.elemIndex target vector
  ) Nothing
  $ matrixToVectors matrix
  where
    matrixToVectors m = Vector.generate
      (Matrix.ncols m)
      $ flip Matrix.getCol m . (+1)

prettySpecks :: GameField -> String
prettySpecks = Matrix.prettyMatrix
