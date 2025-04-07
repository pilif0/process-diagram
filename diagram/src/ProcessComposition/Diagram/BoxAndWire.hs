{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}

{-|
   Description: Process composition diagrams
   Copyright: (c) Filip Smola, 2024

   Diagrams for process compositions based on boxes and wires.
   They take some inspiration from string diagrams and port graphs.
-}
module ProcessComposition.Diagram.BoxAndWire
  ( drawProcess
  , drawPrimitiveCol
  , drawPrimitiveBasic
  , labelInterface
  ) where

import qualified ProcessComposition.Isabelle.Arith        as Arith
import qualified ProcessComposition.Isabelle.Port         as Port
import qualified ProcessComposition.Isabelle.Process      as P
import qualified ProcessComposition.Isabelle.ProcessPaths as PPaths
import qualified ProcessComposition.Isabelle.ProcessPort  as PPort
import qualified ProcessComposition.Isabelle.Resource     as R
import qualified ProcessComposition.Isabelle.ResTerm      as RT

import           ProcessComposition.Diagram.DiagUtil
import           ProcessComposition.Diagram.PortedDiag    (nameOf)
import qualified ProcessComposition.Diagram.PortedDiag    as PD
import           ProcessComposition.Diagram.ResUtil

import           Diagrams.Backend.SVG
import           Diagrams.Core.Types                      (keyVal)
import           Diagrams.Direction                       (dir)
import           Diagrams.Prelude
import           Diagrams.TwoD.Arc                        (arcT)
import           Diagrams.TwoD.Arrow                      (arrowFromLocatedTrail')

import           Data.Typeable

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

-- | Length of wires whenever specifically placing them, such as in between
-- processes, around primitives, ...
-- Usually done by `wireColumn` and `arrColumn`.
wireLength :: N B
wireLength = 1

-- | Line width to use with wires
wireWidth :: Measure (N B)
wireWidth = output 0.5

-- | Line width to use with other shapes
shapeLW :: Measure (N B)
shapeLW = output 1

-- | Vertical space to use when spacing neighbouring wires
wireSpacing :: N B
wireSpacing = 1

-- | Vertical space to use above and below a column of wires when attaching
-- them to boxes
wireColMargin :: N B
wireColMargin = 0.25

-- | Space inserted between subdiagrams of sequential composition
seqSpacing :: N B
seqSpacing = 1

-- | Space inserted between subdiagrams of parallel composition.
parSpacing :: N B
parSpacing = 1

-- | Space inserted between subdiagrams of optional composition
optSpacing :: N B
optSpacing = 1

-- | Side length for the box visualising Once
onceBoxSide :: N B
onceBoxSide = 0.5

-- | Gap between the top line of the Once box and rest of it
onceBoxOpenness :: N B
onceBoxOpenness = 0.25

-- | Radius for the circle visualising Duplicate and Repeat
dupCircRadius :: N B
dupCircRadius = 0.25

-- | Radius for the circle visualising Erase and Close
eraseCircRadius :: N B
eraseCircRadius = dupCircRadius

-- | Side length for the diamond visualising OptDistrIn and OptDistrOut
distrDiaSide :: N B
distrDiaSide = 0.5

-- | Radius of the oplus shape on the input side of optional composition
optInRadius :: N B
optInRadius = 0.5

-- | Spacing to use around the loop-forward wire in visualising Represent
repLoopSpacing :: N B
repLoopSpacing = 0.5

-- | Gap to put between the inside bent wires when visualising Apply
applyInsideGap :: N B
applyInsideGap = 0.5

-- | Space to leave between input and output in visualising Swap, allowing
-- enough space for the diagonal lines to be legible
swapWidth :: N B
swapWidth = 2

-- | Space to leave between input and centre shape in visualising Forget,
-- allowing enough space for the diagonal lines to be legible
forgetGap :: N B
forgetGap = 0.5

-- | Vertical space to use above and below the subdiagram in Represent to pad
-- its box
repPad :: N B
repPad = 0.5

-- | Based on the constants, we can specialise the utility function for
-- connecting names in diagrams with wires
connectNames = connectNames' wireWidth justLine origin origin

-- | Based on the constants, we can specialise the utility function for
-- connecting names in diagrams with arrows
arrConnectNames = connectNames' wireWidth pointyHead origin origin

-- * Setup and Helpers

-- | Ports here use formalised process sides and are labelled by resources with
-- both kinds of atoms being strings
type Port = Port.Port PPort.Process_side (R.Resource String String)

-- | Qualified ports here use formalised process paths on top of what `Port`
-- uses
type QPort = Port.Qualified_port PPort.Process_side (R.Resource String String) PPaths.Process_inner

-- | Ported diagrams here use the same parameters as `QPort`
type PortedDiag = PD.PortedDiag PPort.Process_side (R.Resource String String) PPaths.Process_inner

-- | Resource labelling a qualified port is the one labelling its port
resOf :: QPort -> R.Resource String String
resOf = Port.label . Port.port

-- | Label all input and output ports of a ported diagram with their resources,
-- turning it into a general diagram
labelInterface :: PortedDiag -> Diagram B
labelInterface (PD.PortedDiag is diag os) =
  foldr labelIn (foldr labelOut diag os) is
  where
    portLabel p =
      text' textHeight (prettyRes (Port.label p))
      # frame textPadding
    labelIn qp = withName (nameOf qp) $
      atop . place (alignR $ portLabel (Port.port qp)) . location
    labelOut qp = withName (nameOf qp) $
      atop . place (alignL $ portLabel (Port.port qp)) . location

-- | Build a column of wires of given style for a list of qualified ports.
-- All wires have endpoints named with the relevant qualified port.
-- The placement of that point is governed by the side argument, with left end
-- for input and right end for output.
-- The wire length, width, spacing and margin are governed by module constants.
wireColumn' :: ArrowOpts (N B) -- ^ Arrow style
            -> PPort.Process_side -- ^ Side, deciding named point placement
            -> [QPort] -- ^ List of qualified ports to convert
            -> Diagram B -- ^ Resulting diagram
wireColumn' style s =
  center
  . vsep wireSpacing
  . map (\p ->
      align (
        arrow' style wireLength
        # lw wireWidth
        # keyVal ("title", prettyRes (resOf p)))
      # named (nameOf p))
  where align = case s of
          PPort.In  -> alignL
          PPort.Out -> alignR

-- | Column of plain wires, specialising `wireColumn'`
wireColumn :: PPort.Process_side -> [QPort] -> Diagram B
wireColumn = wireColumn' justLine

-- | Column of wires with an arrow head, specialising `wireColumn'`
arrColumn :: PPort.Process_side -> [QPort] -> Diagram B
arrColumn = wireColumn' pointyHead

-- | Stretch a ported diagram to given width, extending the input and output
-- port wires.
-- The stretched wires use the same names (the relevant qualified ports),
-- relying on `withNames` picking the most recent application of a name.
stretchPorted :: PortedDiag -- ^ Original ported diagram
              -> N B -- ^ Target width
              -> PortedDiag -- ^ Stretched diagram of at least target width
stretchPorted (PD.PortedDiag iPorts diag oPorts) target
  | target <= width diag = PD.PortedDiag iPorts diag oPorts
  | otherwise = PD.PortedDiag iPorts d oPorts
  where
    delta = (target - width diag) / 2
    stretchIn p = withName (nameOf p)
      ( flip atop
      . place ( named (nameOf p)
              . alignL
              $ arrow' justLine delta
                # lw wireWidth
                # keyVal ("title", prettyRes (resOf p)))
      . translateX (- delta)
      . location)
    stretchOut p = withName (nameOf p)
      ( flip atop
      . place ( named (nameOf p)
              . alignR
              $ arrow' justLine delta
                # lw wireWidth
                # keyVal ("title", prettyRes (resOf p)))
      . translateX delta
      . location)
    d = foldr stretchIn (foldr stretchOut diag oPorts) iPorts

-- | Construct qualified ports for parallel parts of a resource using the given
-- side, assuming their index starts at zero and qualifying with the empty path
resourceQPorts :: PPort.Process_side
               -> R.Resource String String
               -> [QPort]
resourceQPorts s = map (`Port.QPort` []) . PPort.parallelPorts Arith.zero_nat s

-- * Drawing Process Compositions
--
-- $drawingCompositions
-- Compositions are drawn by recursion over their stucture.
-- Individual actions produce diagrams with input and output ports based on
-- parameters of the action in question.
-- Composition actions combine the diagrams of their children into one result.

-- | Construct the ported diagram visualising the given process composition.
-- The composition must have strings as both types of resource atoms and as
-- labels to facilitate their printing as part of the diagram.
--
-- The way primitive actions are drawn is expected as a parameter (with
-- `drawPrimitiveBasic` being a simple option), allowing customisation.
-- For instance, the colour of the box could be influenced by the metadata.
drawProcess ::
  (R.Resource String String -> R.Resource String String -> String -> m -> PortedDiag) -- ^ Primitive action drawing function
  -> P.Process String String String m -- ^ Process composition
  -> PortedDiag -- ^ Ported diagram visualising the process composition
drawProcess drawPrimitive (P.Primitive ins outs l m) = drawPrimitive ins outs l m
drawProcess drawPrimitive (P.Identity a)             = drawId a
drawProcess drawPrimitive (P.Swap a b)               = drawSwap a b
drawProcess drawPrimitive (P.Seq p q)                =
  if P.valid (P.Seq p q)
    then drawSeq diagP diagQ
    else error "invalid"
  where
    diagP = PD.qualifyPortedDiag PPaths.SeqL $ drawProcess drawPrimitive p
    diagQ = PD.qualifyPortedDiag PPaths.SeqR $ drawProcess drawPrimitive q
drawProcess drawPrimitive (P.Par p q)                =
  if P.valid (P.Par p q)
    then drawPar strP strQ
    else error "invalid"
  where
    diagP = PD.qualifyPortedDiag PPaths.ParL $ drawProcess drawPrimitive p
    diagQ = PD.qualifyPortedDiag PPaths.ParR $ drawProcess drawPrimitive q
    widthPorted (PD.PortedDiag _ x _) = width x
    strP = diagP `stretchPorted` max (widthPorted diagP) (widthPorted diagQ)
    strQ = diagQ `stretchPorted` max (widthPorted diagP) (widthPorted diagQ)
drawProcess drawPrimitive (P.Opt p q)                =
  if P.valid (P.Opt p q)
    then drawOpt strP strQ (P.input p) (P.input q)
    else error "invalid"
  where
    diagP = PD.qualifyPortedDiag PPaths.OptL $ drawProcess drawPrimitive p
    diagQ = PD.qualifyPortedDiag PPaths.OptR $ drawProcess drawPrimitive q
    widthPorted (PD.PortedDiag _ x _) = width x
    strP = diagP `stretchPorted` max (widthPorted diagP) (widthPorted diagQ)
    strQ = diagQ `stretchPorted` max (widthPorted diagP) (widthPorted diagQ)
drawProcess drawPrimitive (P.InjectL a b)            = drawInjectL a b
drawProcess drawPrimitive (P.InjectR a b)            = drawInjectR a b
drawProcess drawPrimitive (P.OptDistrIn a b c)       = drawDistrIn a b c
drawProcess drawPrimitive (P.OptDistrOut a b c)      = drawDistrOut a b c
drawProcess drawPrimitive (P.Duplicate a)            = drawDup a
drawProcess drawPrimitive (P.Erase a)                = drawErase a
drawProcess drawPrimitive (P.Represent p)            =
  if P.valid (P.Represent p)
    then drawRepresent diagP (P.input p) (P.output p)
    else error "invalid"
  where
    diagP = PD.qualifyPortedDiag PPaths.Rep $ drawProcess drawPrimitive p
drawProcess drawPrimitive (P.Apply a b)              = drawApply a b
drawProcess drawPrimitive (P.Repeat a b)             = drawRepeat a b
drawProcess drawPrimitive (P.Close a b)              = drawClose a b
drawProcess drawPrimitive (P.Once a b)               = drawOnce a b
drawProcess drawPrimitive (P.Forget a)               = drawForget a

-- ** Individual Actions

-- | Draw a box diagram with input and output arrow wires for a primitive
-- action, with the given background colour.
-- This is can be used to easily give `drawProcess` a basic way for visualising
-- primitive actions.
-- While this function takes the action metadata, it is unused and only there
-- to fit the expected type signature.
drawPrimitiveCol :: Colour Double -- ^ Background colour for the box
                 -> R.Resource String String -- ^ Input resource
                 -> R.Resource String String -- ^ Output resource
                 -> String -- ^ Label
                 -> m -- ^ Metadata (unused)
                 -> PortedDiag
drawPrimitiveCol col i o l m =
  PD.PortedDiag iPorts
                (hcat [inputs, procBox, outputs])
                oPorts
  where
    -- Ports for input and output resources
    iPorts = resourceQPorts PPort.In i
    oPorts = resourceQPorts PPort.Out o
    -- Wire columns for those ports
    inputs = arrColumn PPort.In iPorts
    outputs = arrColumn PPort.Out oPorts
    -- Main box with a label and minimum height accommodating the input and
    -- output wire columns
    procBox =
      box col shapeLW
      0 (max (height inputs + 2 * wireColMargin) (height outputs + 2 * wireColMargin))
      (text' textHeight l # frame textPadding)

-- | Basic specialisation of `drawPrimitiveCol` to always use white background
drawPrimitiveBasic :: R.Resource String String
                   -> R.Resource String String
                   -> String
                   -> m
                   -> PortedDiag
drawPrimitiveBasic = drawPrimitiveCol white

-- | Draw a diagram for the executable resource Apply action as a box with two
-- bent wires inside - from input down and from down to output - and a small
-- label.
-- The inside bent wires are to suggest relationship with Represent diagram.
-- For the basic input and output use normal wires, but for the executable
-- input bring the wire in from below.
drawApply :: R.Resource String String
          -> R.Resource String String
          -> PortedDiag
drawApply a b = PD.PortedDiag (iPorts ++ ePorts) diag oPorts
  where
    -- Ports for the basic input and output, and executable input
    iPorts = resourceQPorts PPort.In a
    oPorts = resourceQPorts PPort.Out b
    ePorts = resourceQPorts PPort.In (R.executable a b)
    -- Wire columns for those ports
    inputs = arrColumn PPort.In iPorts
    outputs = arrColumn PPort.Out oPorts
    exec = arrColumn PPort.In ePorts
    -- Centre shape: box with two bent inside wires and a label
    minHeight =
      max ( max (height inputs + 2 * wireColMargin)
                (height outputs + 2 * wireColMargin))
          (textHeight + 2 * textPadding) -- Minimum height of Primitive
    label = text' (textHeight * 0.75) "Apply" # frame (textPadding / 2)
    applyInsideSide = minHeight / 2
    minWidth =
      max (2 * applyInsideSide + applyInsideGap)
          (width label)
    bentWire d sweep = arrowFromLocatedTrail' pointyHead trailLocated
      where trail = arcT d sweep
            trailScaled = scale applyInsideSide trail
            trailLocated = trailScaled `at` P (applyInsideSide *^ fromDirection d)
    inWire = bentWire yDir (-1/4 @@ turn)
    outWire = bentWire (xDir # reflectX) (-1/4 @@ turn)
    insideWires = hsep applyInsideGap [inWire, outWire] :: Diagram B
    vGap = max (textPadding / 2) (minHeight - height label - height insideWires)
    procBox =
      box white shapeLW
      minWidth minHeight
      (vsep vGap [label # centerXY, insideWires # centerXY])
    -- Attach basic input and output wires
    diag' = hcat [inputs, procBox, outputs]
    -- Build executable input wire, coming in from below
    execSide = minWidth / 2
    execBend = arc' execSide xDir (- 1/4 @@ turn) # alignB
    execWire = hcat [exec, execBend]
    -- Attach it from below
    diag = vcat [diag', execWire]


-- | Draw a diagram for identity, consisting of a column of points named with
-- both the corresponding input and output port
drawId :: R.Resource String String -- ^ Input and output resource
       -> PortedDiag
drawId r = PD.PortedDiag iPorts points oPorts
  where
    -- Ports for input and output of the resources
    iPorts = resourceQPorts PPort.In r
    oPorts = resourceQPorts PPort.Out r
    -- Corresponding ports have points named with them, spaced in a column
    pt qpi qpo = pointDiagram origin # named (nameOf qpi) # named (nameOf qpo)
    points = vsep wireSpacing $ zipWith pt iPorts oPorts

drawSwap :: R.Resource String String -> R.Resource String String -> PortedDiag
drawSwap a b = PD.PortedDiag iPorts wires oPorts
  where
    -- Ports for the input and output orderings of the resources
    iPorts = resourceQPorts PPort.In a ++ resourceQPorts PPort.In b
    oPorts = resourceQPorts PPort.Out b ++ resourceQPorts PPort.Out a
    -- Point columns for the ports
    inputs =
      vsep wireSpacing
      . map (\p -> pointDiagram origin # named (nameOf p))
      $ iPorts
    outputs =
      vsep wireSpacing
      . map (\p -> pointDiagram origin # named (nameOf p))
      $ oPorts
    -- Connections labelled by resources between input and output variants of
    -- each port (not they match in order, unlike the actual ports)
    connections = zip3
      (map (prettyRes . resOf) iPorts)
      (map nameOf (resourceQPorts PPort.In a ++ resourceQPorts PPort.In b))
      (map nameOf (resourceQPorts PPort.Out a ++ resourceQPorts PPort.Out b))
    -- Space the input and output points and form those connections
    spaced = hsep swapWidth [inputs, outputs]
    wires = foldr arrConnectNames spaced connections

-- | Draw a diagram for the repeatable to executable resource conversion action
-- Once as an open box with a wire coming in and out.
drawOnce :: R.Resource String String
         -> R.Resource String String
         -> PortedDiag
drawOnce a b = PD.PortedDiag iPorts (hcat [input, procBox, outputs]) oPorts
  where
    -- Ports for the input repeatable resource and output executable resource
    iPorts = resourceQPorts PPort.In (R.repeatable a b)
    oPorts = resourceQPorts PPort.Out (R.executable a b)
    -- Wire columns for the ports (will only have one wire each)
    input = arrColumn PPort.In iPorts
    outputs = arrColumn PPort.Out oPorts
    -- The box is empty and has white background
    openBox side openness =
      fromVertices (map p2 [(0, side), (0, 0), (side, 0), (side, side), (0, side + openness)])
      # strokeLine # translate (r2 (side / 2, side / 2))
    procBox = openBox onceBoxSide onceBoxOpenness # fc white # lw shapeLW

-- | Draw a diagram for the duplication of a resource, visualising both the
-- Duplicate action for copyable atoms and Repeat action for repeatable
-- resources.
drawDup' :: R.Resource String String -- ^ Resource to duplicate
        -> PortedDiag
drawDup' r = PD.PortedDiag iPorts diag oPorts
  where
    -- Ports for the input and output resources
    -- (rest assumes there will only be one input port and two output ports)
    iPorts = resourceQPorts PPort.In r
    oPorts = resourceQPorts PPort.Out (R.parallel [r, r])
    -- Input wire column (should only have one wire)
    input = arrColumn PPort.In iPorts
    -- Centre is an empty circle with white background
    procCirc = circle dupCircRadius # fc white # lw shapeLW
    -- Top output goes from upper-right to top of the circle
    topOut = cubicSpline False [orig, bend, dest]
      # namePoint (const dest) name
      # lw wireWidth
        where
          orig = p2 (0, dupCircRadius)
          bend = orig .+^ r2 (wireLength / 3, wireSpacing / 5)
          dest = p2 (wireLength, wireSpacing / 2)
          name = nameOf (head oPorts)
    -- Bottom output goes from lower-right to bottom of the circle
    botOut = cubicSpline False [orig, bend, dest]
      # namePoint (const dest) name
      # lw wireWidth
        where
          orig = p2 (0, - dupCircRadius)
          bend = orig .+^ r2 (wireLength / 3, - wireSpacing / 5)
          dest = p2 (wireLength, - wireSpacing / 2)
          name = nameOf (last oPorts)
    -- Whole diagram puts input wire before circle with top and bottom output
    -- wires atop it
    diag = input ||| (procCirc <> topOut <> botOut)

-- | Specialise `drawDup'` to copyable atoms
drawDup :: String -> PortedDiag
drawDup c = drawDup' (R.copyable c)

-- | Specialise `drawDup'` to repeatably executable resources
drawRepeat :: R.Resource String String -> R.Resource String String -> PortedDiag
drawRepeat a b = drawDup' (R.repeatable a b)

-- | Draw a diagram for the erasing of a resource, visualising both the Erase
-- action for copyable atoms and Close action for repeatable resources.
drawErase' :: R.Resource String String -- ^ Resource to erase
           -> PortedDiag
drawErase' r = PD.PortedDiag iPorts diag []
  where
    -- Ports for the input resource (rest assumes there will only be one)
    iPorts = resourceQPorts PPort.In r
    -- Input wire column (should only be one wire)
    input = arrColumn PPort.In iPorts
    -- Centre is an empty circle with white background
    procCirc = circle eraseCircRadius # fc white # lw shapeLW
    -- Whole diagram puts input wire before circle and empty space
    diag = input ||| procCirc ||| strutX wireLength

-- | Specialise `drawErase'` to copyable atoms
drawErase :: String -> PortedDiag
drawErase c = drawErase' (R.copyable c)

-- | Specialise `drawErase'` to repeatably executable resources
drawClose :: R.Resource String String -> R.Resource String String -> PortedDiag
drawClose a b = drawErase' (R.repeatable a b)

-- | Draw a diagram for non-deterministic resource injection actions,
-- visualising either direction of injection based on a parameter.
drawInject' :: Either () () -- ^ Direction of injection
            -> R.Resource String String -- ^ First resource
            -> R.Resource String String -- ^ Second resource
            -> PortedDiag
drawInject' dir a b = PD.PortedDiag iPorts diag oPorts
  where
    -- Pick the chosen input resource and relevant label
    (input, label) =
      case dir of
        Left _  -> (a, "L")
        Right _ -> (b, "R")
    -- Ports from the input resource
    iPorts = resourceQPorts PPort.In input
    -- Ports from the output resource (rest assumes there will only be one)
    oPorts = resourceQPorts PPort.Out (R.nonD a b)
    -- Input and output wires
    inputs = arrColumn PPort.In iPorts
    output = arrColumn PPort.Out oPorts
    -- Centre is a triangle pointing right with label chosen by direction and
    -- large enough to accommodate the input wires (at least 1 unit tall)
    vertHeight = max 1 (height inputs)
    tex = text' (0.75 * vertHeight) label
    tri =
      (rotateBy (-1/4) . triangle $ vertHeight)
      # lw shapeLW
      # fc white
    -- Whole diagram puts input wires before triangle atop text and output wire
    diag = hcat [inputs, tex <> tri, output]

-- | Specialise `drawInject'` to left-injection
drawInjectL :: R.Resource String String -> R.Resource String String -> PortedDiag
drawInjectL = drawInject' (Left ())

-- | Specialise `drawInject'` to right-injection
drawInjectR :: R.Resource String String -> R.Resource String String -> PortedDiag
drawInjectR = drawInject' (Right ())

-- | Draw a diagram for distributing parallel resources into both children of a
-- non-deterministic resource.
drawDistrIn :: R.Resource String String
            -> R.Resource String String
            -> R.Resource String String
            -> PortedDiag
drawDistrIn a b c = PD.PortedDiag iPorts diag oPorts
  where
    -- Input and output resources
    input = R.parallel [a, R.nonD b c]
    output = R.nonD (R.parallel [a, b]) (R.parallel [a, c])
    -- Input and output ports
    iPorts = resourceQPorts PPort.In input
    oPorts = resourceQPorts PPort.Out output
    -- Deterministic input, non-deterministic input and output wires
    iWires1 = arrColumn PPort.In (init iPorts)
    iWires2 = arrColumn PPort.In [last iPorts]
    oWires = arrColumn PPort.Out oPorts
    -- Centre is a rotated square with its top named by ()
    get Nothing  = error "Distribution diamond ray tracing failed"
    get (Just x) = x
    central =
      rotateBy (1/8) (square distrDiaSide)
      # fc white
      # lw shapeLW
      # namePoint (get . rayTraceP origin unitY) ()
    -- Base diagram is centre shape with the non-deterministic wires
    diagBase = hcat [iWires2, central, oWires]
    -- Calculate spacing compensation for the centre shape
    centreHeight = sqrt (2 * distrDiaSide ^ 2)
    delta = centreHeight / 2
    spacing = wireSpacing - delta
    -- Whole diagram adds to that the deterministic input wires and connects
    -- them to the the central shape
    diag = foldr (connectNames' wireWidth justLine unitX origin)
                 (alignL iWires1 === strutY spacing === alignL diagBase)
                 (zip3 (map (prettyRes . resOf) (init iPorts))
                       (map nameOf (init iPorts))
                       (repeat (toName ())))
    -- TODO Central shape stretches the envelope, leading to imperfect
    -- alignment with surrounding wires.

-- | Draw a diagram for distributing parallel resources out of both children of
-- a non-deterministic resource.
drawDistrOut :: R.Resource String String
             -> R.Resource String String
             -> R.Resource String String
             -> PortedDiag
-- TODO is there a way of handling both distributions with one function?
drawDistrOut a b c = PD.PortedDiag iPorts diag oPorts
  where
    -- Input and output resources
    input = R.nonD (R.parallel [a, b]) (R.parallel [a, c])
    output = R.parallel [a, R.nonD b c]
    -- Input and output ports
    iPorts = resourceQPorts PPort.In input
    oPorts = resourceQPorts PPort.Out output
    -- Input, deterministic output and non-deterministic output wires
    iWires = arrColumn PPort.In iPorts
    oWires1 = arrColumn PPort.Out (init oPorts)
    oWires2 = arrColumn PPort.Out [last oPorts]
    -- Centre is a rotated square with its top named by ()
    get Nothing  = error "Distribution diamond ray tracing failed"
    get (Just x) = x
    central =
      rotateBy (1/8) (square distrDiaSide)
      # fc white
      # lw shapeLW
      # namePoint (get . rayTraceP origin unitY) ()
    -- Base diagram is centre shape with the non-deterministic wires
    diagBase = hcat [iWires, central, oWires2]
    -- Calculate spacing compensation for the centre shape
    centreHeight = sqrt (2 * distrDiaSide ^ 2)
    delta = centreHeight / 2
    spacing = wireSpacing - delta
    -- Whole diagram adds to that the deterministic input wires and connects
    -- them to the the central shape
    diag = foldr (connectNames' wireWidth justLine unit_X origin)
                 (alignR oWires1 === strutY spacing === alignR diagBase)
                 (zip3 (map (prettyRes . resOf) (init oPorts))
                       (map nameOf (init oPorts))
                       (repeat (toName ())))
    -- TODO Central shape stretches the envelope, leading to imperfect
    -- alignment with surrounding wires.

-- | Draw a diagram for forgetting all information about a resource.
drawForget :: R.Resource String String
           -> PortedDiag
drawForget a = PD.PortedDiag iPorts diag oPorts
  where
    -- Input and output ports
    iPorts = resourceQPorts PPort.In a
    oPorts = resourceQPorts PPort.Out R.anything
    -- Input and output wires
    iPoints =
      center
      . vsep wireSpacing
      . map (\p -> pointDiagram origin # named (nameOf p))
      $ iPorts
    oPoints =
      center
      . vsep wireSpacing
      . map (\p -> pointDiagram origin # named (nameOf p))
      $ oPorts
    -- Centre shape is a cross, with its centre named with ()
    centre =
      ( (center . rotateBy (-1/6) $ arrow' justLine 1 # lw shapeLW) <>
        (center . rotateBy (1/6) $ arrow' justLine 1 # lw shapeLW))
      # named ()
    -- Base diagram is input wires, centre shape and output wires, spaced apart
    baseDiag = hsep forgetGap [iPoints, centre, oPoints]
    -- Whole diagram connects input wires to centre and centre to output wire
    diag' =
      foldr arrConnectNames
            baseDiag
            (zip3 (map (prettyRes . resOf) oPorts) (repeat (toName ())) (map nameOf oPorts))
    diag =
      foldr connectNames
            diag'
            (zip3 (map (prettyRes . resOf) iPorts) (map nameOf iPorts) (repeat (toName ())))

-- ** Composition Actions

-- | Combine diagrams to represent sequential composition of processes,
-- juxtaposing them horizontally and connecting outputs of the first to inputs
-- of the second.
-- Centers the two diagrams before combining them.
drawSeq :: PortedDiag
        -> PortedDiag
        -> PortedDiag
drawSeq (PD.PortedDiag i1 d1 o1) (PD.PortedDiag i2 d2 o2) =
  PD.PortedDiag i1 mergedDiag o2
    where mergedDiag =
            foldr connectNames
                  (hsep seqSpacing [center d1, center d2])
                  (zip3 (map (prettyRes . resOf) o1)
                        (map nameOf o1)
                        (map nameOf i2))

-- | Combine diagrams to represent parallel composition of processes,
-- juxtaposing them vertically.
-- Centers the two diagrams before combining them.
drawPar :: PortedDiag -> PortedDiag -> PortedDiag
drawPar (PD.PortedDiag i1 d1 o1) (PD.PortedDiag i2 d2 o2) =
  PD.PortedDiag (i1 ++ i2) mergeDiag (o1 ++ o2)
    where mergeDiag = vsep parSpacing [center d1, center d2]

-- | Connecting components of the optional composition diagram requires extra
-- names: top and bottom of the input and output prisms
data PrismPt = Top PPort.Process_side | Bot PPort.Process_side
  deriving (Typeable, Eq, Ord, Show)
instance IsName PrismPt

-- | Combine diagrams to represent optional composition of processes,
-- juxtaposing them vertically and connecting their inputs and outputs to
-- prisms representing the splitting/merging of alternatives.
-- Centers the two diagrams.
drawOpt :: PortedDiag
        -> PortedDiag
        -> R.Resource String String -- ^ Input resource of first process
        -> R.Resource String String -- ^ Input resource of second process
        -> PortedDiag
drawOpt (PD.PortedDiag i1 d1 o1) (PD.PortedDiag i2 d2 o2) inL inR =
  PD.PortedDiag iPorts mergedDiag oPorts
    where
      -- Input and output ports
      iPorts = resourceQPorts PPort.In (R.nonD inL inR)
      oPorts = o1 -- By composition validity: o1 == o2
      -- Input and output wires
      iWires = arrColumn PPort.In iPorts
      oWires = arrColumn PPort.Out oPorts
      -- Input prism is an oplus
      iPrism = (hrule (2 * optInRadius) <> vrule (2 * optInRadius) <> circle optInRadius)
        # fc white # lw shapeLW
        # namePoint (const . p2 $ (0, optInRadius)) (Top PPort.In)
        # namePoint (const . p2 $ (0, - optInRadius)) (Bot PPort.In)
      -- Output prism is a triangle pointing left with right side fitting output
      get Nothing  = error "Opt prism ray tracing failed"
      get (Just x) = x
      oPrism = (rotateBy (1/4) . triangle $ max 1 (height oWires))
        # fc white # lw shapeLW
        # namePoint (get . rayTraceP origin (unitY + unit_X)) (Top PPort.Out)
        # namePoint (get . rayTraceP origin (unit_Y + unit_X)) (Bot PPort.Out)
      -- Base diagram is: input prism, vertically juxtaposed subdiagrams,
      -- output prism; last is skipped for no output
      -- Attaching input and output wires is delayed to avoid name clashes when
      -- connecting, because output wire names are shared with first subdiagram
      parts = centerY (vsep optSpacing [center d1, center d2])
      baseDiag = hsep optSpacing $
        [iPrism, parts] ++ [oPrism | not (null oPorts)]
      -- Connections we are forming are between prism tops and first
      -- subdiagram, and prism bottoms and second subdiagram, connecting from
      -- input prism to subdiagram inputs and from subdiagram outputs to output
      -- prism
      connections =
          zip3 (map (prettyRes . resOf) i1)
               (map nameOf i1)
               (repeat . toName $ Top PPort.In)
       ++ zip3 (map (prettyRes . resOf) i2)
               (map nameOf i2)
               (repeat . toName $ Bot PPort.In)
       ++ zip3 (map (prettyRes . resOf) o1)
               (map nameOf o1)
               (repeat . toName $ Top PPort.Out)
       ++ zip3 (map (prettyRes . resOf) o2)
               (map nameOf o2)
               (repeat . toName $ Bot PPort.Out)
      conDiag = foldr connectNames baseDiag connections
      -- Whole diagram additionally includes the delayed input and output wires
      mergedDiag = hcat [iWires, conDiag, oWires]

-- | Augment a diagram to visualise introducing a repeatably executable
-- resource representing a process, enclosing the it in a box and looping its
-- input to the output side.
drawRepresent :: PortedDiag
              -> R.Resource String String -- ^ Input resource
              -> R.Resource String String -- ^ Output resource
              -> PortedDiag
drawRepresent (PD.PortedDiag i d o) a b =
  PD.PortedDiag [] diag oPorts
    where
      -- Output ports
      oPorts = resourceQPorts PPort.Out (R.repeatable a b)
      -- Output wires
      oWires = arrColumn PPort.Out oPorts
      -- Box the subdiagram, arc its input up and put a wire above it; box that
      boxed =
        center d <>
        rect (width d) (height d + 2 * repPad) # fc white # lw shapeLW
      loopWire = arrow' pointyTail (width boxed) # lw wireWidth
      arc = arc' ((height boxed / 2 + repLoopSpacing) / 2)
                 (direction unit_Y)
                 (-1/2 @@ turn)
      diagInner =
        strutY repLoopSpacing
        ===
        ( alignT arc |||
          alignT (vsep repLoopSpacing [alignL loopWire, alignL boxed]))
      diagOuter =
        center diagInner <>
        rect (width diagInner) (height diagInner) # fc white # lw shapeLW
      -- Whole diagram adds the output wire
      diag = diagOuter ||| oWires
