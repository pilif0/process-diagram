{-|
   Description: Resource utility functions
   Copyright: (c) Filip Smola, 2024
-}
module ProcessComposition.Diagram.ResUtil
  ( prettyResT
  , prettyRes
  ) where

import qualified ProcessComposition.Isabelle.Resource as R
import qualified ProcessComposition.Isabelle.ResTerm  as RT

import           Data.List                            (intercalate)

-- | Print a string-based resource term
prettyResT :: RT.Res_term String String -> String
prettyResT RT.Empty               = "1"
prettyResT RT.Anything            = "\x22A4"
prettyResT (RT.Res a)             = a
prettyResT (RT.Copyable a)        = "!" ++ a
prettyResT (RT.Parallel as)       = intercalate " \x2219 " (map prettyBody as)
  -- TODO the font we are using does not have \odot (\x2299)
prettyResT (RT.NonD a b)          = prettyBody a ++ " \x2295 " ++ prettyBody b
prettyResT (RT.Executable a b)    = prettyBody a ++ " \x2192 " ++ prettyBody b
  -- TODO the font we are using does not have \multimap (\x22B8)
prettyResT (RT.Repeatable a b)    =
  "![" ++ prettyBody a ++ " \x2192 " ++ prettyBody b ++ "]"
  -- TODO the font we are using does not have \multimap (\x22B8)

prettyBody :: RT.Res_term String String -> String
prettyBody x = case x of
  -- Constants and unary are not parenthesised
  RT.Empty            -> prettyResT x
  RT.Anything         -> prettyResT x
  (RT.Res _)          -> prettyResT x
  (RT.Copyable _)     -> prettyResT x
  -- Nary are parenthesised
  (RT.Parallel _)     -> "(" ++ prettyResT x ++ ")"
  (RT.NonD _ _)       -> "(" ++ prettyResT x ++ ")"
  (RT.Executable _ _) -> "(" ++ prettyResT x ++ ")"
  -- Repeatable process is already parenthesised specially due to ! prefix
  (RT.Repeatable _ _) -> prettyResT x

-- | Print a string-based resource as its normalised representative term
prettyRes = prettyResT . R.of_resource
