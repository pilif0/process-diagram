{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}

{-|
   Description: Composition and resource syntax diagrams
   Copyright: (c) Filip Smola, 2024

   Diagrams for process compositions and resources, displaying them as syntax
   trees.
-}
module ProcessComposition.Diagram.SyntaxTree
  ( resTermDiag
  , resourceDiag
  , processDiag
  ) where

import qualified ProcessComposition.Isabelle.Process  as P
import qualified ProcessComposition.Isabelle.Resource as R
import qualified ProcessComposition.Isabelle.ResTerm  as RT

import           ProcessComposition.Diagram.DiagUtil
import           ProcessComposition.Diagram.ResUtil

import           Diagrams.Backend.SVG
import           Diagrams.Prelude
import           Diagrams.TwoD.Layout.Tree

import qualified Graphics.SVGFonts                    as SF

import qualified Data.List                            as L
import           Data.Maybe
import qualified Data.Tree                            as T

import           System.IO.Unsafe                     (unsafePerformIO)

-- * Constants
--
-- $constants
-- Several constants controlling how the diagrams look, such as element sizes
-- and line widths.

-- | Height that text should fit
textHeight :: Double
textHeight = 1

-- | Space to frame text
textPadding :: Double
textPadding = 0.2

-- | Line width to use when connecting nodes
lineW :: Measure (N B)
lineW = output 0.5

-- | Line width to use with nodes
nodeLW :: Measure (N B)
nodeLW = output 1

-- | Space to frame the whole diagram
diagPadding :: Double
diagPadding = 0.1

-- | Horizontal separation of nodes in the tree
treeHSep :: N B
treeHSep = 1

-- | Vertical separation of nodes in the tree
treeVSep :: N B
treeVSep = 2

-- * Syntax Tree Diagrams
--
-- $syntax-diagrams
-- Syntax tree diagrams visualise resources (through their normalised
-- representative term) and process compositions as a tree of constructors and
-- arguments.

-- | Draw a tree node, given a label
node :: String -> Diagram B
node s = box white nodeLW 0 0 (text' textHeight s # frame textPadding)

-- | Turn a resource term into a single syntax tree node, showing its top-most
-- constructor
resTermToNode :: RT.Res_term String String -> Diagram B
resTermToNode RT.Empty            = node "Empty"
resTermToNode RT.Anything         = node "Anything"
resTermToNode (RT.Res a)          = node ("Res '" ++ a ++ "'")
resTermToNode (RT.Copyable a)     = node ("Copy '" ++ a ++ "'")
resTermToNode (RT.Parallel _)     = node "Parallel"
resTermToNode (RT.NonD _ _)       = node "NonD"
resTermToNode (RT.Executable _ _) = node "Executable"
resTermToNode (RT.Repeatable _ _) = node "Repeatable"

-- | Get the list of children for any resource term
resTermChildren :: forall a b. RT.Res_term a b -> [RT.Res_term a b]
resTermChildren RT.Empty            = []
resTermChildren RT.Anything         = []
resTermChildren (RT.Res _)          = []
resTermChildren (RT.Copyable _)     = []
resTermChildren (RT.Parallel as)    = as
resTermChildren (RT.NonD a b)       = [a, b]
resTermChildren (RT.Executable a b) = [a, b]
resTermChildren (RT.Repeatable a b) = [a, b]

-- | Turn a resource term into a tree of node diagrams
resTermDiagTree :: RT.Res_term String String -> T.Tree (Diagram B)
resTermDiagTree x =
  T.Node (resTermToNode x) (map resTermDiagTree (resTermChildren x))

-- | Turn a tree of diagrams into a tree diagram
treeToDiag :: T.Tree (Diagram B) -> Diagram B
treeToDiag t =
  renderTree id
             (arrowBetween' (with & arrowTail .~ noTail
                                  & arrowHead .~ noHead
                                  & shaftStyle %~ lw lineW))
             (symmLayout' (with & slHSep .~ treeHSep
                                & slVSep .~ treeVSep
                                & slWidth .~ fromMaybe (0,0) . extentX
                                & slHeight .~ fromMaybe (0,0) . extentY)
                          t)
  # centerXY # frame diagPadding

-- | Put together resource term drawing
resTermDiag :: RT.Res_term String String -> Diagram B
resTermDiag = treeToDiag . resTermDiagTree

-- | Put together resource drawing
resourceDiag :: R.Resource String String -> Diagram B
resourceDiag = resTermDiag . R.of_resource

-- | Turn a process into a single syntax tree node, showing its top-most
-- constructor
procToNode :: forall c. P.Process String String String c -> Diagram B
procToNode (P.Primitive ins outs l m) = node l
procToNode (P.Identity a) = node ("id(" ++ prettyRes a ++ ")")
procToNode (P.Swap a b) = node ("swap(" ++ prettyRes a ++ ", " ++ prettyRes b ++ ")")
procToNode (P.Seq p q) = node "Seq"
procToNode (P.Par p q) = node "Par"
procToNode (P.Opt p q) = node "Opt"
procToNode (P.InjectL a b) = node ("inL(" ++ prettyRes a ++ ", " ++ prettyRes b ++ ")")
procToNode (P.InjectR a b) = node ("inR(" ++ prettyRes a ++ ", " ++ prettyRes b ++ ")")
procToNode (P.OptDistrIn a b c) = node ("optDistrIn(" ++ prettyRes a ++ ", " ++ prettyRes b ++ ", " ++ prettyRes c ++ ")")
procToNode (P.OptDistrOut a b c) = node ("optDistrOut(" ++ prettyRes a ++ ", " ++ prettyRes b ++ ", " ++ prettyRes c ++ ")")
procToNode (P.Duplicate a) = node ("duplicate(" ++ prettyRes (R.copyable a) ++ ")")
procToNode (P.Erase a) = node ("erase(" ++ prettyRes (R.copyable a) ++ ")")
procToNode (P.Represent p) = node "Represent"
procToNode (P.Apply a b) = node ("apply(" ++ prettyRes a ++ ", " ++ prettyRes b ++ ")")
procToNode (P.Repeat a b) = node ("repeat(" ++ prettyRes a ++ ", " ++ prettyRes b ++ ")")
procToNode (P.Close a b) = node ("close(" ++ prettyRes a ++ ", " ++ prettyRes b ++ ")")
procToNode (P.Once a b) = node ("once(" ++ prettyRes a ++ ", " ++ prettyRes b ++ ")")

-- | Get the list of children for any process
procChildren :: forall a b c d. P.Process a b c d -> [P.Process a b c d]
procChildren (P.Primitive ins outs l m) = []
procChildren (P.Identity a)             = []
procChildren (P.Swap a b)               = []
procChildren (P.Seq p q)                = [p, q]
procChildren (P.Par p q)                = [p, q]
procChildren (P.Opt p q)                = [p, q]
procChildren (P.InjectL a b)            = []
procChildren (P.InjectR a b)            = []
procChildren (P.OptDistrIn a b c)       = []
procChildren (P.OptDistrOut a b c)      = []
procChildren (P.Duplicate a)            = []
procChildren (P.Erase a)                = []
procChildren (P.Represent p)            = [p]
procChildren (P.Apply a b)              = []
procChildren (P.Repeat a b)             = []
procChildren (P.Close a b)              = []
procChildren (P.Once a b)               = []

-- | Turn a process into a tree of diagrams
procDiagTree :: forall c. P.Process String String String c -> T.Tree (Diagram B)
procDiagTree x = T.Node (procToNode x) (map procDiagTree (procChildren x))

-- | Put together process drawing
processDiag :: forall c. P.Process String String String c -> Diagram B
processDiag = treeToDiag . procDiagTree
