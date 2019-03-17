{-# LANGUAGE NoMonomorphismRestriction #-}  --diagrams
{-# LANGUAGE FlexibleContexts          #-}  --diagrams
{-# LANGUAGE TypeFamilies              #-}  --diagrams

module Poe where
--import qualified Types as AirTypes    
import qualified Diagrams.Prelude               as D
import qualified Diagrams.Backend.SVG.CmdLine   as D

data Color = White | Blue | Yellow

data MobType = MkMobType 
    { mtBaseDamage :: Double
    }

data MobPack = MkMobPack
    { mpCount :: Int
    , mpLocation :: Double
    , mpColor :: Color
    }

data Zone = MkZone 
    { zLen :: Double
    , zMobTypes :: [MobType]
    , zMobPacks :: [MobPack]
    }


diaZone :: Zone -> D.Diagram D.B
diaZone _ = D.circle 1

