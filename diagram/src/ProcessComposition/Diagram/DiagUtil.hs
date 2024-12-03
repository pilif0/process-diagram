{-# LANGUAGE FlexibleContexts #-}

{-|
   Description: Diagram utility functions
   Copyright: (c) Filip Smola, 2024
-}
module ProcessComposition.Diagram.DiagUtil
  ( box
  , text'
  , justLine
  , pointyHead
  , pointyTail
  , connectNames'
  ) where

import           Diagrams.Backend.SVG
import           Diagrams.Core.Types  (keyVal)
import           Diagrams.Prelude

import qualified Graphics.SVGFonts    as SF

import           System.IO.Unsafe     (unsafePerformIO)

-- | Centre a diagram and enclose it in a box
box :: Colour Double -- ^ Background colour of the box
    -> Measure (N B) -- ^ Line width
    -> N B -- ^ Minimum width of the box
    -> N B -- ^ Minimum height of the box
    -> Diagram B -- ^ Diagram to enclose
    -> Diagram B
box col linew minX minY c =
  centerXY c <> rect w h # fc col # lw linew
  where
    w = max (width c) minX
    h = max (height c) minY

-- | Draw a text using SVGFonts (the font `SF.lin`) as a path into a diagram,
-- with black colour.
-- The result is centred.
text' :: Double -- ^ Height the text should fit
      -> String -- ^ Text to draw
      -> Diagram B
text' d s = stroke path # lw none # fc black # center
  where opts = SF.TextOpts (unsafePerformIO font) SF.KERN False
        font = SF.lin
        path = s # SF.svgText opts # SF.fit_height d # SF.drop_rect

-- | Arrow configuration with no head or tail
justLine :: ArrowOpts (N B)
justLine = with & arrowHead .~ noHead & arrowTail .~ noTail

-- | Arrow configuration with only triangle head and no tail
pointyHead :: ArrowOpts (N B)
pointyHead = with & arrowHead .~ tri & arrowTail .~ noTail & headLength .~ 5

-- | Arrow configuration with only triangle tail and no head
pointyTail :: ArrowOpts (N B)
pointyTail = with & arrowHead .~ noHead & arrowTail .~ tri' & tailLength .~ 5

-- | Connect a pair of names in a diagram with an arrow of the given style,
-- offset by adding given points to the location of each name and annotating
-- each arrow with a string title.
-- Tuple input makes it suitable for folding over a list of connections to be
-- made.
connectNames' :: Measure (N B) -- ^ Line width for the arrows
              -> ArrowOpts (N B) -- ^ Options for the arrows
              -> Point (V B) (N B) -- ^ Origin point offset
              -> Point (V B) (N B) -- ^ Destination point offset
              -> (String, Name, Name) -- ^ Title-origin-destination tuple
              -> Diagram B -- ^ Diagram in which to connect
              -> Diagram B -- ^ Same diagram with that connection added
connectNames' w style o1 o2 (r, m, n) = withNames [m, n] (\[a, b] -> atop $
    arrowBetween' style
                  (location a + o1) (location b + o2)
                  # lw w
                  # keyVal ("title", r)
  )
