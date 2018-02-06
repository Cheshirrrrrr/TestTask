{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception (bracket)
import           Data.Default      (def)
import           Database.Bolt

import           Queries

myConfiguration :: BoltCfg
myConfiguration = def { user = "neo4j", password = "12345" }

main :: IO ()
main = bracket
         (connect myConfiguration)
         close
         (\pipe -> do res <- run pipe (shortestPath 1 6)
                    --res <- run pipe (getReaction 1)
                    --_ <- run pipe (putReaction 3 4 2 25 20 (Reaction 21 "c1") (Molecule 41 "c2" "c3") 20)
                      print res)
