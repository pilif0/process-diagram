{-# LANGUAGE StandaloneDeriving #-}

{-|
   Description: Diagrams with qualified ports
   Copyright: (c) Filip Smola, 2024

   Establishes diagrams paired with lists of input and output ports for
   designating connection points in them.
   The ports used are generated from a formalisation, with sufficient
   instantiations made here for them to serve as names in diagrams.
-}
module ProcessComposition.Diagram.PortedDiag
  ( PortedDiag(..)
  , qualifyPortedDiag
  , nameOf
  ) where

import qualified ProcessComposition.Isabelle.Arith        as Arith
import qualified ProcessComposition.Isabelle.Port         as Port
import qualified ProcessComposition.Isabelle.ProcessPaths as PPaths
import qualified ProcessComposition.Isabelle.ProcessPort  as PPort
import qualified ProcessComposition.Isabelle.Resource     as R
import qualified ProcessComposition.Isabelle.ResTerm      as RT

import           Diagrams.Backend.SVG
import           Diagrams.Prelude

import           Data.Typeable

-- Natural numbers
instance Eq Arith.Nat where {
  a == b = Arith.equal_nat a b;
};
deriving instance Ord Arith.Nat

-- Resource terms
deriving instance (Ord a, Ord b) => Ord (RT.Res_term a b)

-- Resources
deriving instance (Ord a, Ord b) => Ord (R.Resource a b)

-- Process port sides
deriving instance Typeable PPort.Process_side
deriving instance Ord PPort.Process_side

-- Ports
deriving instance (Ord a, Ord b) => Ord (Port.Port a b)
deriving instance (Typeable a, Typeable b) => Typeable (Port.Port a b)

-- Process paths
deriving instance Typeable PPaths.Process_inner
deriving instance Ord PPaths.Process_inner

-- Qualified Ports
instance (Eq a, Eq b, Eq c) => Eq (Port.Qualified_port a b c) where {
  a == b = Port.equal_qualified_port a b;
};
deriving instance (Ord a, Ord b, Ord c) => Ord (Port.Qualified_port a b c)
deriving instance (Typeable a, Typeable b, Typeable c)
  => Typeable (Port.Qualified_port a b c)

-- Ports, process paths and qualified ports can be used as diagram names
instance
  ( Typeable a, Eq a, Ord a, Show a
  , Typeable b, Eq b, Ord b, Show b
  ) => IsName (Port.Port a b)
instance IsName PPaths.Process_inner
instance
  ( Typeable a, Eq a, Ord a, Show a
  , Typeable b, Eq b, Ord b, Show b
  , Typeable c, Eq c, Ord c, Show c
  ) => IsName (Port.Qualified_port a b c)

-- | Qualified ports are turned into diagram names by using the port as a name
-- and then qualifying it in sequence by every element of the process path
nameOf :: ( Typeable a, Eq a, Ord a, Show a
          , Typeable b, Eq b, Ord b, Show b
          , IsName c
          ) => Port.Qualified_port a b c
            -> Name
nameOf qp = foldr (.>) (toName . Port.port $ qp) (Port.name qp)

-- | Ported diagram is one with lists of qualified input and output ports
data PortedDiag a b c =
  PortedDiag [Port.Qualified_port a b c] (Diagram B) [Port.Qualified_port a b c]

-- Since qualified ports are taken from generated code, they are not named by
-- arbitrary names (from Diagrams) but only lists of Process_inner.
-- As a result, they are not compatible with the Qualifiable class of diagrams
-- and we need to define our own functions to qualify ported diagrams.
-- (Qualification of qualified ports is already generated.)

-- | Use a process path atom to qualify both the ports and the diagram
qualifyPortedDiag :: PPaths.Process_inner
                  -> PortedDiag a b PPaths.Process_inner
                  -> PortedDiag a b PPaths.Process_inner
qualifyPortedDiag n (PortedDiag i d o) =
  PortedDiag
    (map (Port.qualifyQPort n) i)
    (n .>> d)
    (map (Port.qualifyQPort n) o)
