{-# LANGUAGE TypeFamilies #-}

module Main
  ( main
  ) where

import           ProcessComposition.Diagram.BoxAndWire (drawPrimitiveBasic,
                                                        drawProcess,
                                                        labelInterface)
import           ProcessComposition.Diagram.SyntaxTree (processDiag,
                                                        resourceDiag)
import           ProcessComposition.Isabelle.Process
import           ProcessComposition.Isabelle.Resource

import           Diagrams.Backend.SVG                  (SVG, renderSVG)
import           Diagrams.Prelude                      (Diagram)
import           Diagrams.TwoD.Size                    (dims2D, height, width)

import           System.Environment                    (getArgs)

-- Pair basic figure processes with file names to demonstrate box-and-wire
demosBox =
  [ ( "box-primitive"
    , Primitive (parallel [res "I", res "J"]) (res "O") "Primitive" ()
    )
  , ( "box-seq"
    , Seq (Primitive (res "A") (res "B") "P" ())
          (Primitive (res "B") (res "C") "Q" ())
    )
  , ( "box-par"
    , Par (Primitive (res "A") (res "B") "P" ())
          (Primitive (res "X") (res "Y") "Q" ())
    )
  , ( "box-opt"
    , Opt (Primitive (res "A") (parallel [res "X", res "Y"]) "P" ())
          (Primitive (res "B") (parallel [res "X", res "Y"]) "Q" ())
    )
  , ( "box-represent"
    , Represent (Primitive (parallel [res "A", res "B"]) (parallel [res "X", res "Y"]) "P" ())
    )
  , ( "box-identity"
    , Identity (parallel [res "A", res "B"])
    )
  , ( "box-swap"
    , Swap (parallel [res "A", res "B"]) (res "C")
    )
  , ( "box-inject_l"
    , InjectL (res "A") (res "B")
    )
  , ( "box-inject_r"
    , InjectR (res "A") (res "B")
    )
  , ( "box-opt_distr_in"
    , OptDistrIn (res "X") (res "A") (res "B")
    )
  , ( "box-opt_distr_out"
    , OptDistrOut (res "X") (res "A") (res "B")
    )
  , ( "box-erase"
    , Erase "data"
    )
  , ( "box-duplicate"
    , Duplicate "data"
    )
  , ( "box-apply"
    , Apply (parallel [res "A", res "B"]) (parallel [res "X", res "Y"])
    )
  , ( "box-repeat"
    , Repeat (parallel [res "A", res "B"]) (parallel [res "X", res "Y"])
    )
  , ( "box-close"
    , Close (parallel [res "A", res "B"]) (parallel [res "X", res "Y"])
    )
  , ( "box-once"
    , Once (parallel [res "A", res "B"]) (parallel [res "X", res "Y"])
    )
  , ( "box-forget"
    , Forget (parallel [res "A", res "B"])
    )
  , ( "box-stretch_id"
    , Par (Primitive (res "A") (res "B") "P" ())
          (Identity (parallel [res "X", res "Y"]))
    )
  , ( "box-stretch_pr"
    , Par (Primitive (res "A") (res "B") "Very Long Name To Test Stretching" ())
          (Primitive (res "X") (res "Y") "Q" ())
    )
  ]

-- Pair basic figure processes with file names to demonstrate process syntax
-- diagrams
demosPSyn =
  [ ( "syn-primitive"
    , Primitive (parallel [res "I", res "J"]) (res "O") "Primitive" ()
    )
  , ( "syn-seq"
    , Seq (Primitive (res "A") (res "B") "P" ())
          (Primitive (res "B") (res "C") "Q" ())
    )
  , ( "syn-par"
    , Par (Primitive (res "A") (res "B") "P" ())
          (Primitive (res "X") (res "Y") "Q" ())
    )
  , ( "syn-opt"
    , Opt (Primitive (res "A") (parallel [res "X", res "Y"]) "P" ())
          (Primitive (res "B") (parallel [res "X", res "Y"]) "Q" ())
    )
  , ( "syn-represent"
    , Represent (Primitive (parallel [res "A", res "B"]) (parallel [res "X", res "Y"]) "P" ())
    )
  , ( "syn-identity"
    , Identity (parallel [res "A", res "B"])
    )
  , ( "syn-swap"
    , Swap (parallel [res "A", res "B"]) (res "C")
    )
  -- TODO any specific and more complex compositions?
  ]

-- Pair simple resources with file names to demonstrate resource syntax
-- diagrams
demosRSyn =
  [ ("res-parallel", parallel [res "A", res "B", res "C"])
  , ("res-res", res "X")
  , ("res-nonD", nonD (res "U") (res "V"))
  , ("res-executable", executable (res "I") (res "O"))
  , ("res-copy", copyable "D")
  ]

-- Helpers for parsing the destination argument and rendering a diagram as SVG
parseDest :: IO String
parseDest = head <$> getArgs

renderSize d = dims2D (f * width d) (f * height d)
  where f = 25

renderDiag :: String -> Diagram SVG -> IO ()
renderDiag p d = renderSVG p (renderSize d) d

-- Describe how each box-and-wire demo is drawn, given a base path and
-- file-process pair
renderBoxDemo :: String -> (String, Process String String String ()) -> IO ()
renderBoxDemo p (f, x) =
  (renderDiag fullpath
  . labelInterface
  . drawProcess drawPrimitiveBasic
  $ x)
  >> putStrLn ("Rendered box-and-wire: " ++ fullpath)
    where
      fullpath = p ++ "/" ++ f ++ ".svg"

-- Describe how each process syntax demo is drawn, given a base path and
-- file-process pair
renderPSynDemo :: String -> (String, Process String String String ()) -> IO ()
renderPSynDemo p (f, x) =
  (renderDiag fullpath
  . processDiag
  $ x)
  >> putStrLn ("Rendered process syntax: " ++ fullpath)
    where
      fullpath = p ++ "/" ++ f ++ ".svg"

-- Describe how each resource syntax demo is drawn, given a base path and
-- file-process pair
renderRSynDemo :: String -> (String, Resource String String) -> IO ()
renderRSynDemo p (f, x) =
  (renderDiag fullpath
  . resourceDiag
  $ x)
  >> putStrLn ("Rendered resource syntax: " ++ fullpath)
    where
      fullpath = p ++ "/" ++ f ++ ".svg"

-- When run, get the base path and render each demo into it
main :: IO ()
main = do
  p <- parseDest
  foldr (\x y -> renderBoxDemo p x >> y) (return ()) demosBox
  foldr (\x y -> renderPSynDemo p x >> y) (return ()) demosPSyn
  foldr (\x y -> renderRSynDemo p x >> y) (return ()) demosRSyn
