{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Specks

main :: IO ()
main = do
  field <- shuffledSpecks (4, 4)
  putStrLn $ prettySpecks field
