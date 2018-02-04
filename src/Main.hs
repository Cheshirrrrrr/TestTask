{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Exception (bracket)
import           Data.Default      (def)
import qualified Data.Text         as T (Text, pack)
import           Database.Bolt
import           NeatInterpolation

data Molecule = Molecule { _moleculeId     :: Int
                         , _moleculeSmiles :: T.Text
                         , _iupacName      :: T.Text
                         }
  deriving (Show, Eq)


data Catalyst = Catalyst { _catalystId     :: Int
                         , _catalystSmiles :: T.Text
                         , _catalystName   :: Maybe T.Text
                         }
  deriving (Show, Eq)


data Reaction = Reaction { _reactionId   :: Int
                         , _reactionName :: T.Text
                         }
  deriving (Show, Eq)


data MoleculeToReaction = MoleculeToReaction { _fromMolecule :: Molecule
                                             , _mtoReaction  :: Reaction
                                             }
  deriving (Show, Eq)


data MoleculeFromReaction = MoleculeFromReaction { _molecule     :: Molecule
                                                 , _fromReaction :: Reaction
                                                 , _amount       :: Float
                                                 }
  deriving (Show, Eq)


data CatalystToReaction = CatalystToReaction { _ctoReaction :: Reaction
                                             , _catalyst    :: Catalyst
                                             , _temperature :: Float
                                             , _pressure    :: Float
                                             }
  deriving (Show, Eq)


toReaction :: Monad m => Value -> m (Maybe Reaction)
toReaction v = do node :: Node <- exact v
                  let nProps = nodeProps node
                  let lbl = labels node
                  if "Reaction" `elem` lbl
                    then do reactionId   <- (nProps `at` "id") >>= exact
                            reactionName <- (nProps `at` "name") >>= exact
                            return $ Just (Reaction reactionId reactionName)
                    else return Nothing

toMolecule :: Monad m => Value -> m (Maybe Molecule)
toMolecule v = do node :: Node <- exact v
                  let nProps = nodeProps node
                  let lbl = labels node
                  if "Molecule" `elem` lbl
                    then do moleculeId     <- (nProps `at` "id") >>= exact
                            moleculeSmiles <- (nProps `at` "smiles") >>= exact
                            iupacName      <- (nProps `at` "iupacName") >>= exact
                            return $ Just (Molecule moleculeId moleculeSmiles iupacName)
                    else return Nothing

toMoleculeOrReaction :: Monad m => Value -> m (Either Molecule Reaction)
toMoleculeOrReaction v = do node :: Node <- exact v
                            let lbl = labels node
                            if "Molecule" `elem` lbl
                                then fmap (maybe (error "something strange...") Left) (toMolecule v)
                                else fmap (maybe (error "Node is not Molecule or Reaction") Right) (toReaction v)

getReaction :: Int -> BoltActionT IO (Maybe Reaction)
getReaction rId = do records <- query gen
                     case records of
                        []    -> return Nothing
                        (x:_) -> (x `at` "r") >>= toReaction
                    where gen :: T.Text
                          gen = do
                                 let reactId = T.pack (show rId)
                                 [text|MATCH (r:Reaction) WHERE r.id = ${reactId} RETURN r|]


--MATCH (n) DETACH DELETE n
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
