{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types
  (
    Molecule (..), Catalyst (..), Reaction (..)
  , MoleculeToReaction (..), MoleculeFromReaction (..), CatalystToReaction (..)
  , toReaction
  , toMolecule
  , toMoleculeOrReaction
  ) where

import qualified Data.Text     as T (Text)
import           Database.Bolt (Node (..), Value, at, exact)

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


-- | Try to convert BOLT Value to `Reaction`, if it's not possible - return Nothing
toReaction :: Monad m => Value -> m (Maybe Reaction)
toReaction v = do node :: Node <- exact v
                  let nProps = nodeProps node
                  let lbl = labels node
                  if "Reaction" `elem` lbl
                    then do reactionId   <- (nProps `at` "id") >>= exact
                            reactionName <- (nProps `at` "name") >>= exact
                            return $ Just (Reaction reactionId reactionName)
                    else return Nothing

-- | Try to convert BOLT Value to `Molecule`, if it's not possible - return Nothing
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

-- | Try to convert BOLT Value to `Molecule` OR `Reaction`, if it's not possible - throw error with message
toMoleculeOrReaction :: Monad m => Value -> m (Either Molecule Reaction)
toMoleculeOrReaction v = do node :: Node <- exact v
                            let lbl = labels node
                            if "Molecule" `elem` lbl
                                then fmap (maybe (error "something strange...") Left) (toMolecule v)
                                else fmap (maybe (error "Node is not Molecule or Reaction") Right) (toReaction v)
