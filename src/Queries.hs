{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Queries
  (
    getReaction
  , putReaction
  , shortestPath
  ) where

import qualified Data.Text         as T (Text, pack)
import           Database.Bolt     (BoltActionT, at, query, query_)
import           NeatInterpolation

import           Types             (Molecule (..), Reaction (..),
                                    toMoleculeOrReaction, toReaction)

-- | Get `Reaction` from the database by index
getReaction :: Int -> BoltActionT IO (Maybe Reaction)
getReaction rId = do records <- query gen
                     case records of
                        []    -> return Nothing
                        (x:_) -> (x `at` "r") >>= toReaction
                    where gen :: T.Text
                          gen = do
                                 let reactId = T.pack (show rId)
                                 [text|MATCH (r:Reaction) WHERE r.id = ${reactId} RETURN r|]


-- | Create `Reaction` in the database, also create `Molecule`, that is obtained from the given `Reaction`, and create all relationships with this `Reaction`
putReaction :: Int -> Int -> Int -> Float -> Float -> Reaction -> Molecule -> Float -> BoltActionT IO ()
putReaction fmId smId cId t p (Reaction rId rName) (Molecule mId mSm mIN) am = query_ gen
  where gen :: T.Text
        gen  = do
                let fId = T.pack (show fmId)
                let sId = T.pack (show smId)
                let catId = T.pack (show cId)
                let molId = T.pack (show mId)
                let reactId = T.pack (show rId)
                let temp = T.pack (show t)
                let pres = T.pack (show p)
                let a = T.pack (show am)
                [text|MATCH (m1:Molecule { id: ${fId} }),
                             (m2:Molecule { id: ${sId} }),
                             (c:Catalyst { id: ${catId} })
                      CREATE (nm:Molecule { id: ${molId}, smiles: "${mSm}", iupacName: "${mIN}" })
                      CREATE (r:Reaction { id: ${reactId}, name: "${rName}" })
                      CREATE (m1)-[:REAGENT_IN]->(r)
                      CREATE (m2)-[:REAGENT_IN]->(r)
                      CREATE (c)-[:ACCELERATE { temperature: ${temp}, pressure: ${pres} }]->(r)
                      CREATE (r)-[:PRODUCT_FROM { amount: ${a} }]->(nm)|]

-- | Find the shortest path between two given `Molecule`-s, return a list of Nodes (`Molecule` or `Reacion`) in this path
shortestPath :: Int -> Int -> BoltActionT IO [Either Molecule Reaction]
shortestPath fmId smId = do records <- query gen
                            nodes <- traverse (`at` "nd") records
                            traverse toMoleculeOrReaction nodes
                           where gen :: T.Text
                                 gen = do
                                        let fId = T.pack (show fmId)
                                        let sId = T.pack (show smId)
                                        [text|MATCH (fm:Molecule { id: ${fId} }),(sm:Molecule { id: ${sId} }),
                                                     p = shortestPath((fm)-[*..50]-(sm))
                                              UNWIND NODES(p) AS nd
                                              RETURN nd|]
