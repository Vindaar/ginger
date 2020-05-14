import math
import sugar
import chroma
import options
export options

import sequtils
import strformat
from seqmath import linspace
import strutils

import ginger / [macroUtils, backends, types]
export types, backends, macroUtils

from os import splitFile

# TODO: think about renaming `Coord1D` to someting like Unit?

# TODO: implement some more units so that we can use it to define
# distances in absolute numbers (without knowing width / height)
# instead of relatives, which will be useful for things which should
# NOT depend on the aspect ratio of the plot, e.g. distance of label
# from an axis

## backendLayer
## implements the prototype backend layer
# requires
# done in backends.nim / backendsCairo.nim

## baseLayer
## implements the prototype base layer

const
  grey92* = color(0.92, 0.92, 0.92)
  grey20* = color(0.20, 0.20, 0.20)
  black* = color(0.0, 0.0, 0.0)
  white* = color(1.0, 1.0, 1.0)
  transparent* = color(0.0, 0.0, 0.0, 0.0)

# makes use of backends layer
type
  GraphObjectKind* = enum
    goAxis, # a plot axis
    goText, # general text
    goLabel, # an axis label
    goTick, # an axis tick
    goTickLabel, # a tick label (i.e. the number or text)
    goPoint, # a general point
    goLine, # a general line
    goRect, # a general rectangle
    goGrid, # the plot grid (lines along the ticks)
    goPolyLine, # a line connecting several points
    goComposite # an object consisting of several other GraphObjects

  CompositeKind* = enum
    cmpErrorBar # an error bar consisting of potentially several lines

  TickKind* = enum
    tkOneSide, # only outside the plot
    tkBothSides # inside and outside the plot

  GraphObject* = ref object
    name*: string # name of the Graph Object. Currently mainly used for debugging
    children*: seq[GraphObject]
    style*: Option[Style]
    rotateInView*: Option[(float, Point)] # rotation of viewport applied to all objs
    rotate*: Option[float] # rotation around center position
    case kind*: GraphObjectKind
    of goLine, goAxis:
      lnStart*: Coord
      lnStop*: Coord
    of goLabel, goText, goTickLabel:
      txtText*: string
      txtFont*: Font
      txtPos*: Coord
      txtAlign*: TextAlignKind
      # NOTE: do we need this really?
      txtRotate*: float # possible additional rotation
    of goGrid:
      gdOrigin: Coord     # Coordinate of origin of plot viewport
      gdOriginDiag: Coord # and corner diagonal along viewport
                          # used to define bounds of grid lines
      gdXPos*: seq[Coord1D] # stores X position of lines to draw
      gdYPos*: seq[Coord1D]
    of goTick:
      tkMajor*: bool # is a major tick, e.g. large tick w/ label
      tkPos*: Coord
      tkAxis*: AxisKind
      tkKind*: TickKind
      tkSecondary*: bool
    of goPoint:
      ptMarker*: MarkerKind
      ptSize*: float # can be removed, due to style
      ptColor*: Color # can be removed, due to style
      ptPos*: Coord
    of goPolyLine:
      plPos*: seq[Coord]
    of goRect:
      reOrigin*: Coord
      #reBottom*: float
      #reWidth*: float
      #reHeight*: float
      reWidth*: Quantity
      reHeight*: Quantity
    of goComposite:
      cmpKind*: CompositeKind # a purely generic kind to describe the composite
                              # used for debugging / echoing
    #else: discard

  # TODO:
  # - write function that applies `Style`!

  Scale* = tuple[low, high: float]

  UnitKind* = enum
    ukRelative, # relative to viewport (0.0, 1.0)
    #ukSqRelative, # squared relative coordinates. Use the smaller of the
                  # two relative coordinates in absolute terms as a basis
    ukData, # based on xScale, yScale of data
    ukStrWidth,#, # based on width of a string in a given fontsize
    ukStrHeight, # same as strWidth except for height
    # kinds requiring absolute scale
    ukPoint, # absolute size of image ~= points
    ukCentimeter, # absolute cm based on dpi of 72.27
    ukInch # absolute inch, 2.54 cm = 1 inch

    #ckSquareRelative # relative coords but square. Takes min(width, height)
                     # as def for (0, 1)
    # ...

  # A quantity consisting of a value and a unit, e.g. 1 cm, 1 in, etc.
  Quantity* = tuple[val: float, unit: UnitKind] #tuple[val: float, kind: UnitKind]
    #val*: float
    #kind*: UnitKind

  Coord1D* = object
    pos*: float
    case kind*: UnitKind
    of ukRelative:
      discard
    of ukPoint, ukCentimeter, ukInch:
      # length of scale in points
      # 72.27 points in inch
      # 2.54 cm in inch
      # Option since this is only used to convert it to relative coords.
      length*: Option[Quantity]
    of ukData:
      scale*: Scale
      axis*: AxisKind
    of ukStrWidth, ukStrHeight:
      # NOTE: Coord1D of strWidth is not really a coordinate, but rather a
      # quantity! It's `pos` field refers to how many times the `strWidth` is
      # to be taken
      text*: string
      font*: Font
    #of ckMixed:
    #  # all outstanding variables still need to be
    #  outstanding: seq[Coord1D]

  Coord* = object
    x*: Coord1D
    y*: Coord1D

  # - implement coordinate transformations between viewport
  #   and global coordinates
  # - coordinates should be relative coordinates in [0.0, 1.0]
  Viewport* = ref object
    # parameters describing the embedding into the parent
    # given relative coords
    name*: string # name of the viewport (useful for debugging)
    parent*: string # name of viewport's parent (useful for debugging)
    style*: Style
    xScale*: Scale
    yScale*: Scale
    rotate*: Option[float]
    scale*: Option[float] # scaling factor to scale elements by
    origin*: Coord # "origin" of the viewport, i.e. the location of the
                  # (left, bottom) position in relative coordinates (ukRelative)
    width*: Quantity # width in relative coordinates
    height*: Quantity
    objects*: seq[GraphObject] # NOTE: when adding objects manually, be aware
                               # that certain transformations have to be applied
                               # manually beforehand!
    children*: seq[Viewport]
    wView*: Quantity # absolute width, height in points (pixels) of viewport
    hView*: Quantity
    wImg*: Quantity # absolute width, height in points (pixels) of image
    hImg*: Quantity
    backend*: BackendKind

const DPI = 72.27

proc pointWidth*(view: Viewport): Quantity {.inline.}
proc pointHeight*(view: Viewport): Quantity {.inline.}

template defaultFont(pts = 12.0): untyped = Font(family: "sans-serif", size: pts, color: color(0.0, 0.0, 0.0))

template setFontOrDefault(setTo, fontArg: untyped): untyped =
  ## Helper tempalte to set the `txtFont` field of `setTo` given an option
  ## `fontArg` or use the default above
  if fontArg.isSome:
    setTo.txtFont = fontArg.get
  else:
    setTo.txtFont = defaultFont()

template XAxisYPos*(view: Option[Viewport] = none[Viewport](),
                    margin = 0.0,
                    isSecondary = false): untyped =
  ## Y position of the X axis.
  if view.isSome:
    if not isSecondary:
      let viewEl = view.get
      Coord1D(pos: pointHeight(viewEl).val + margin,
              length: some(pointHeight(viewEl)),
              kind: ukPoint)
    else:
      Coord1D(pos: -margin,
              length: some(pointHeight(view.unsafeGet)),
              kind: ukPoint)
  else:
    if not isSecondary:
      Coord1D(pos: 1.0, kind: ukRelative)
    else:
      Coord1D(pos: 0.0, kind: ukRelative)

template YAxisXPos*(view: Option[Viewport] = none[Viewport](),
                    margin = 0.0,
                    isSecondary = false): untyped =
  ## X position of the Y axis.
  if view.isSome:
    if not isSecondary:
      Coord1D(pos: -margin,
              length: some(pointWidth(view.unsafeGet)),
              kind: ukPoint)
    else:
      let viewEl = view.get
      Coord1D(pos: pointWidth(viewEl).val + margin,
              length: some(viewEl.wImg),
              kind: ukPoint)
  else:
    if not isSecondary:
      Coord1D(pos: 0.0, kind: ukRelative)
    else:
      Coord1D(pos: 1.0, kind: ukRelative)

func formatTickValue*(f: float, scale = 0.0): string =
  ## performs the formatting of tick labels from the given values
  ## Uses fixed point notation for values < 1e5 and > 1e-5. Otherwise
  ## exponential notation with precision 4, zeros are trimmed.
  ## A `scale`, which is (if possible) just the diff of two tick places
  ## is used to determine whether the given value `f` is supposed to be
  ## the zero value on the scale. From e.g. `linspace` we might end up
  ## with `5.1234e-17` for the string representation of zero. The scale
  ## is required to know whether the values aren't inherently this small.
  if abs(f) < scale / 10.0:
    # IMPORTANT: we make a big assumption here! Namely that our tick positions
    # are "reasonably" placed. If we find a value that's smaller than a
    # `1/10` of the scale it's probably supposed to be a `"0"`. For some weird
    # asymmetric tick posisionts this might actually be wrong!
    result = "0"
  elif abs(f) >= 1e5 or abs(f) <= 1e-5:
    result = f.formatBiggestFloat(format = ffScientific,
                                  precision = 4)
  else:
    result = f.formatBiggestFloat(format = ffDefault,
                                  precision = 4)
  result.trimZeros()

#template XAxis2YPos*(view: Option[Viewport] = none[Viewport](),
#                     margin = 0.0): untyped =
#  ## Y position of the secondary X axis.
#  if view.isSome:
#    Coord1D(pos: -margin,
#            length: some(view.get.hView),
#            kind: ukPoint)
#  else:
#    Coord1D(pos: 0.0, kind: ukRelative)
#
#template YAxis2XPos*(view: Option[Viewport] = none[Viewport](),
#                     margin = 0.0): untyped =
#  ## X position of the secondary Y axis.
#  if view.isSome:
#    let viewEl = view.get
#    Coord1D(pos: width(viewEl).toPoints(some(viewEl.wView)).val + margin,
#            length: some(viewEl.wImg),
#            kind: ukPoint)
#  else:
#    Coord1D(pos: 1.0, kind: ukRelative)

template info(f: varargs[untyped]): untyped =
  when declared(verbose):
    echo(f)

template warn(f: varargs[untyped]): untyped =
  ## TODO: replace by some other log level
  when declared(verbose):
    echo(f)

template debug(f: varargs[untyped]): untyped =
  ## TODO: replace by some other log level
  when declared(verbose):
    echo(f)

func quant*(val: float, unit: UnitKind): Quantity = (val: val, unit: unit)

template cmToInch(x: float): float = x / 2.54
template inchToAbs(x: float): float = x * DPI
template absToInch(x: float): float = x / DPI
template inchToCm(x: float): float = x * 2.54

func toPoints*(q: Quantity,
               length: Option[Quantity] = none[Quantity]()): Quantity =
  ## returns the quantity converted to Points. This will fail if attempted to
  ## convert a non absolute unit!
  case q.unit
  of ukPoint: result = q
  of ukCentimeter: result = quant(q.val.cmToInch.inchToAbs, ukPoint)
  of ukInch: result = quant(q.val.inchToAbs, ukPoint)
  of ukRelative:#, ukSqRelative:
    # NOTE: for ukSqRelative the caller needs to make sure the given length corresponds to
    # the smaller length of the viewport
    if length.isSome:
      result = quant(q.val * length.get().toPoints.val, ukPoint)
    else:
      raise newException(ValueError, "Cannot convert quantity with unit " &
        $q.unit & " to points without a length!")
  else:
    raise newException(ValueError, "Cannot convert quantity with unit " &
      $q.unit & " to points!")

func toInch*(q: Quantity): Quantity =
  ## returns the quantity converted to inch. This will fail if attempted to
  ## convert a non absolute unit!
  case q.unit
  of ukPoint: result = quant(q.val.absToInch, ukInch)
  of ukCentimeter: result = quant(q.val.inchToCm, ukInch)
  of ukInch: result = q
  else:
    raise newException(ValueError, "Cannot convert quantity with unit " &
      $q.unit & " to inch!")

func toCentimeter*(q: Quantity): Quantity =
  ## returns the quantity converted to centimeter. This will fail if attempted to
  ## convert a non absolute unit!
  case q.unit
  of ukPoint:
    result = quant(q.val.absToInch.inchToCm, ukCentimeter)
  of ukCentimeter: result = q
  of ukInch: result = quant(q.val.inchToCm, ukCentimeter)
  else:
    raise newException(ValueError, "Cannot convert quantity with unit " &
      $q.unit & " to centimeter!")

proc toRelative*(q: Quantity,
                 length: Option[Quantity] = none[Quantity](),
                 scale: Option[Scale] = none[Scale]()): Quantity =
  ## returns a quantity as a relative length (typically) in a given viewport
  if q.unit != ukRelative and q.unit != ukData:
    doAssert((length.isSome and
              length.get.unit in ukPoint .. ukInch),
             "length scale needed to convert quantity to relative value!")
    warn "[WARNING]: Converting quantity ", q, " to relative via ", length.get, "!"
  elif q.unit != ukRelative and q.unit == ukData:
    doAssert(scale.isSome,
             "length scale needed to convert quantity to relative value!")
    warn "[WARNING]: Converting quantity ", q, " to relative via ", scale.get, "!"
  case q.unit
  of ukRelative:
    result = q
  of ukPoint:
    result = quant(q.val / length.get.toPoints.val, ukRelative)
  of ukCentimeter, ukInch:
    result = quant(q.toPoints.val / length.get.toPoints.val, ukRelative)
  of ukData:
    info "[INFO]: conversion of ukData quant to relative. Assuming `length` is data scale!"
    if scale.isSome:
      let sc = scale.unsafeGet
      # NOTE: we do ``not`` subtract the lower scale from `q.val`, because this is
      # a ``quantity`` and not a ccoordinate!
      result = quant(q.val / (sc.high - sc.low), ukRelative)
    else:
      raise newException(Exception, "Need a scale to convert quantity of kind " &
        "`ukData` to relative!")
  else:
    raise newException(Exception, "Cannot convert quantity " & $q & " to " &
      "relative quantity!")


func to*(q: Quantity, kind: UnitKind,
         length = none[Quantity](),
         scale = none[Scale]()): Quantity =
  if q.unit == kind: return q
  case kind
  of ukPoint: q.toPoints(length = length)
  of ukInch: q.toInch()
  of ukCentimeter: q.toCentimeter
  of ukRelative: q.toRelative(length = length, scale = scale)
  #of ukData:
  else: raise newException(ValueError, "Cannot convert to `" & $kind & "`!")

proc `+`*(c1, c2: Coord1D): Coord1D
proc `-`*(c1, c2: Coord1D): Coord1D
proc `*`*(c1, c2: Coord1D): Coord1D
proc `/`*(c1, c2: Coord1D): Coord1D

proc times*(q1, q2: Quantity,
            length: Option[Quantity] = none[Quantity](),
            scale: Option[Scale] = none[Scale](),
            asCoordinate = false): Quantity =
  ## multiplication of two quantities. If one of the quantities is absolute
  ## the result will also be absolute!
  ## If `asCoordinate` is true and on of the quantities is given in `ukData`,
  ## the multiplication will be handled by interpreting the value `q` as belonging
  ## to a coordinate on the `scale` instead of a distance defined by the
  ## total scale length `(scale.high - scale.low)`. In practice this means
  ## `scale.low` is subtracted from the `q` beforehand.
  if q1.unit == q2.unit:
    # just multiply the values
    result = quant((q1.val * q2.val), q1.unit)
  else:
    case q1.unit
    of ukPoint, ukInch, ukCentimeter:
      case q2.unit
      of ukRelative:
        result = quant(q1.val * q2.val, q1.unit)
      of ukPoint, ukInch, ukCentimeter:
        # divide as points, convert back
        result = quant(
          (q1.toPoints().val * q2.toPoints(length = length).val),
          ukPoint)
          .to(q1.unit, length = length, scale = scale)
      of ukStrWidth, ukStrHeight, ukData:
        raise newException(ValueError, "Cannot do arithmetic on `" & $q2.unit & "`!")
    of ukRelative:
      case q2.unit
      of ukRelative: doAssert false, "not possible!"
      of ukPoint, ukCentimeter, ukInch:
        # just divide
        result = quant((q1.val * q2.val), q2.unit)
      of ukStrWidth, ukStrHeight, ukData:
        raise newException(ValueError, "Cannot do arithmetic on `" & $q2.unit & "`!")
    of ukData:
      # return as relative
      var q1Final: Quantity
      if asCoordinate:
        doAssert scale.isSome
        q1Final = quant(q1.val - scale.unsafeGet.low, ukData)
      result = quant(
        (q1Final.toRelative(length = length,
                       scale = scale).val *
         q2.toRelative(length = length,
                       scale = scale).val),
        ukRelative)
    else:
      raise newException(ValueError, "Cannot do arithmetic on `" & $q1.unit & "`!")

proc divide*(q1, q2: Quantity,
             length: Option[Quantity] = none[Quantity](),
             scale: Option[Scale] = none[Scale](),
             asCoordinate = false): Quantity =
  ## multiplication of two quantities. If one of the quantities is absolute
  ## the result will also be absolute!
  ## If `asCoordinate` is true and on of the quantities is given in `ukData`,
  ## the division will be handled by interpreting the value `q` as belonging
  ## to a coordinate on the `scale` instead of a distance defined by the
  ## total scale length `(scale.high - scale.low)`. In practice this means
  ## `scale.low` is subtracted from the `q` beforehand.
  if q1.unit == q2.unit:
    # just divide the values
    result = quant((q1.val / q2.val), q1.unit)
  else:
    case q1.unit
    of ukPoint, ukInch, ukCentimeter:
      case q2.unit
      of ukRelative:
        result = quant(q1.val / q2.val, q1.unit)
      of ukPoint, ukInch, ukCentimeter:
        # divide as points, convert back
        result = quant(
          (q1.toPoints().val / q2.toPoints(length = length).val),
          ukPoint)
          .to(q1.unit, length = length, scale = scale)
      of ukStrWidth, ukStrHeight, ukData:
        raise newException(ValueError, "Cannot do arithmetic on `" & $q2.unit & "`!")
    of ukRelative:
      case q2.unit
      of ukRelative: doAssert false, "not possible!"
      of ukPoint, ukCentimeter, ukInch:
        # just divide
        result = quant((q1.val / q2.val), q2.unit)
      of ukStrWidth, ukStrHeight, ukData:
        raise newException(ValueError, "Cannot do arithmetic on `" & $q2.unit & "`!")
    of ukData:
      # return as relative
      var q1Final: Quantity
      if asCoordinate:
        doAssert scale.isSome
        q1Final = quant(q1.val - scale.unsafeGet.low, ukData)
      result = quant(
        (q1Final.toRelative(length = length,
                       scale = scale).val /
         q2.toRelative(length = length,
                       scale = scale).val),
        ukRelative)
    else:
      raise newException(ValueError, "Cannot do arithmetic on `" & $q1.unit & "`!")

proc add*(q1, q2: Quantity,
          length: Option[Quantity] = none[Quantity](),
          scale: Option[Scale] = none[Scale](),
          asCoordinate = false): Quantity =
  ## multiplication of two quantities. If one of the quantities is absolute
  ## the result will also be absolute!
  ## If `asCoordinate` is true and on of the quantities is given in `ukData`,
  ## the addition will be handled by interpreting the value `q` as belonging
  ## to a coordinate on the `scale` instead of a distance defined by the
  ## total scale length `(scale.high - scale.low)`. In practice this means
  ## `scale.low` is subtracted from the `q` beforehand.
  if q1.unit == q2.unit:
    # just add the values
    result = quant((q1.val + q2.val), q1.unit)
  else:
    case q1.unit
    of ukPoint, ukInch, ukCentimeter:
      case q2.unit
      of ukPoint, ukInch, ukCentimeter, ukRelative:
        # multiply as points, convert back
        result = quant(
          (q1.toPoints().val + q2.toPoints(length = length).val),
          ukPoint)
          .to(q1.unit, length = length, scale = scale)
      of ukStrWidth, ukStrHeight, ukData:
        raise newException(ValueError, "Cannot do arithmetic on `" & $q2.unit & "`!")
    of ukRelative:
      case q2.unit
      of ukRelative: doAssert false, "not possible!"
      of ukPoint, ukCentimeter, ukInch:
        # add as points, convert back
        result = quant(
          (q1.toPoints(length = length).val + q2.toPoints().val),
          ukPoint)
          .to(q2.unit, length = length, scale = scale)
      of ukStrWidth, ukStrHeight, ukData:
        raise newException(ValueError, "Cannot do arithmetic on `" & $q2.unit & "`!")
    of ukData:
      # return as relative
      var q1Final: Quantity
      if asCoordinate:
        doAssert scale.isSome
        q1Final = quant(q1.val - scale.unsafeGet.low, ukData)
      result = quant(
        (q1Final.toRelative(length = length,
                       scale = scale).val +
         q2.toRelative(length = length,
                       scale = scale).val),
        ukRelative)
    else:
      raise newException(ValueError, "Cannot do arithmetic on `" & $q1.unit & "`!")

proc sub*(q1, q2: Quantity,
          length: Option[Quantity] = none[Quantity](),
          scale: Option[Scale] = none[Scale](),
          asCoordinate = false): Quantity =
  ## subtraction of two quantities. If one of the quantities is absolute
  ## the result will also be absolute!
  ## If `asCoordinate` is true and on of the quantities is given in `ukData`,
  ## the subtraction will be handled by interpreting the value `q` as belonging
  ## to a coordinate on the `scale` instead of a distance defined by the
  ## total scale length `(scale.high - scale.low)`. In practice this means
  ## `scale.low` is subtracted from the `q` beforehand.
  if q1.unit == q2.unit:
    # just subtract the values
    result = quant((q1.val - q2.val), q1.unit)
  else:
    case q1.unit
    of ukPoint, ukInch, ukCentimeter:
      case q2.unit
      of ukPoint, ukInch, ukCentimeter, ukRelative:
        # multiply as points, convert back
        result = quant(
          (q1.toPoints().val - q2.toPoints(length = length).val),
          ukPoint)
          .to(q1.unit, length = length, scale = scale)
      of ukStrWidth, ukStrHeight, ukData:
        raise newException(ValueError, "Cannot do arithmetic on `" & $q2.unit & "`!")
    of ukRelative:
      case q2.unit
      of ukRelative: doAssert false, "not possible!"
      of ukPoint, ukCentimeter, ukInch:
        # add as points, convert back
        result = quant(
          (q1.toPoints(length = length).val - q2.toPoints().val),
          ukPoint)
          .to(q2.unit, length = length, scale = scale)
      of ukStrWidth, ukStrHeight, ukData:
        raise newException(ValueError, "Cannot do arithmetic on `" & $q2.unit & "`!")
    of ukData:
      # return as relative
      var q1Final: Quantity
      if asCoordinate:
        doAssert scale.isSome
        q1Final = quant(q1.val - scale.unsafeGet.low, ukData)
      result = quant(
        (q1Final.toRelative(length = length,
                       scale = scale).val -
         q2.toRelative(length = length,
                       scale = scale).val),
        ukRelative)
    else:
      raise newException(ValueError, "Cannot do arithmetic on `" & $q1.unit & "`!")

func initCoord1D*(at: float, kind: UnitKind = ukRelative): Coord1D =
  ## returns a Coord1D at coordinate `at` of kind `kind`
  result = Coord1D(pos: at, kind: kind)

template c1*(at: float, kind: UnitKind = ukRelative): Coord1D =
  initCoord1D(at, kind)

proc initCoord1d*(view: Viewport, at: float,
                  axKind: AxisKind,
                  kind: UnitKind = ukPoint): Coord1D =
  ## Full name should be `initCoord1D`, but since it's a convenience function
  ## given name is short.
  ## initialize a Coord1D based on the given viewport. This is useful for
  ## absolute or data variables, which depend on the absolute sizes or scales
  result = Coord1D(pos: at, kind: kind)
  var length: Quantity
  var optLength: Option[Quantity]
  case axKind
  of akX:
    length = pointWidth(view)
  of akY:
    length = pointHeight(view)
  case kind
  of ukPoint:
    result.length = some(length.toPoints)
  of ukCentimeter:
    result.length = some(length.toCentimeter)
  of ukInch:
    result.length = some(length.toInch)
  of ukData:
    case axKind
    of akX:
      result.scale = view.xScale
    of akY:
      result.scale = view.yScale
    result.axis = axKind
  of ukStrWidth, ukStrHeight:
    raise newException(Exception, "Strwidth not yet implemented!")
  else: discard

template c1*(view: Viewport, at: float,
             axKind: AxisKind,
             kind: UnitKind = ukPoint): Coord1D =
  initCoord1D(view, at, axKind, kind)

proc initCoord*(x, y: float, kind: UnitKind = ukRelative): Coord =
  ## returns a coordinate at coordinates x, y of kind `kind`
  result = Coord(x: initCoord1D(x, kind = kind),
                 y: initCoord1D(y, kind = kind))

template c*(x, y: float, kind: UnitKind = ukRelative): Coord =
  initCoord(x, y, kind)

proc initCoord*(view: Viewport, x, y: float,
                kind: UnitKind = ukRelative): Coord =
  ## length and scale aware (using Viewport) initCoord, that automatically
  ## assigns the correct length and scale for absolute / data coords
  result = Coord(x: view.initCoord1D(x, akX, kind),
                 y: view.initCoord1D(x, akY, kind))

template c*(view: Viewport, x, y: float, kind: UnitKind = ukRelative): Coord =
  initCoord(view, x, y, kind)


func ggColorHue*(num: int,
                 hueStart = 15.0,
                 chroma = 100.0,
                 luminance = 65.0
                ): seq[Color] =
  ## returns the default ggplot2 color hue for `num` colors
  let hues = linspace(hueStart, hueStart + 360.0, num + 1)
  result = hues.mapIt(color(ColorHCL(h: it, c: chroma, l: luminance)))

func eitherOrRaise[T](either: Option[T],
                      `or`: Option[T]): T {.raises: [ValueError,
                                                     ref UnpackError].} =
  ## returns either `x.get` or `or.get` or raise `ValueError`
  ## should in principle only be used if either of the two `certainly` contains
  ## a value!
  if either.isSome:
    result = either.get()
  elif `or`.isSome:
    result = `or`.get()
  else:
    raise newException(ValueError, "Neither of the two optionals contains a value!")

func pretty*(gobj: GraphObject, indent = 0): string =
  # string conversion function of `GraphObject`
  result = repeat(' ', indent) & &"(GraphObject.name: {gobj.name},\n"
  result &= repeat(' ', indent) & &"GraphObject.kind: {gobj.kind},\n"
  case gobj.kind
  of goLine, goAxis:
    result &= repeat(' ', indent) & &"lnStart: {gobj.lnStart},\n"
    result &= repeat(' ', indent) & &"lnStop: {gobj.lnStop}\n"
  of goLabel, goText, goTickLabel:
    result &= repeat(' ', indent) & &"txtText: {gobj.txtText},\n"
    result &= repeat(' ', indent) & &"txtPos: {gobj.txtPos},\n"
    result &= repeat(' ', indent) & &"txtAlign: {gobj.txtAlign},\n"
    result &= repeat(' ', indent) & &"txtRotate: {gobj.txtRotate},\n"
    result &= repeat(' ', indent) & &"txtFont: {gobj.txtFont}\n"
  of goGrid:
    result &= repeat(' ', indent) & &"gdXPos: {gobj.gdXPos},\n"
    result &= repeat(' ', indent) & &"gdYPos: {gobj.gdYPos}\n"
    result &= repeat(' ', indent) & &"gdOrigin: {gobj.gdOrigin},\n"
    result &= repeat(' ', indent) & &"gdOriginDiag: {gobj.gdOriginDiag}\n"
  of goTick:
    result &= repeat(' ', indent) & &"tkPos: {gobj.tkPos},\n"
    result &= repeat(' ', indent) & &"tkMajor: {gobj.tkMajor},\ntkAxis: {gobj.tkAxis}\n"
  of goPoint:
    result &= repeat(' ', indent) & &"ptPos: {gobj.ptPos},\n"
    result &= repeat(' ', indent) & &"ptMarker: {gobj.ptMarker},\n"
    result &= repeat(' ', indent) & &"ptSize: {gobj.ptSize},\n"
    result &= repeat(' ', indent) & &"ptColor: {gobj.ptColor}\n"
  of goPolyLine:
    result &= repeat(' ', indent) & &"plPos: {gobj.plPos}\n"
  of goRect:
    result &= repeat(' ', indent) & &"reOrigin: {gobj.reOrigin},\n"
    result &= repeat(' ', indent) & &"reWidth: {gobj.reWidth},\n"
    result &= repeat(' ', indent) & &"reHeight: {gobj.reHeight}\n"
  else:
    result &= repeat(' ', indent) & &"<no conversion for {gobj.kind}\n"
  result &= repeat(' ', indent) & &"style: {gobj.style},\n"
  result &= repeat(' ', indent) & &"rotate: {gobj.rotate},\n"
  result &= repeat(' ', indent) & &"rotateInView: {gobj.rotateInView},\n"
  result &= repeat(' ', indent) & &"children:\n"
  if gobj.children.len == 0:
    result &= repeat(' ', indent + 2) & "@[]\n"
  else:
    result &= "\n"
  for ch in gobj.children:
    result &= pretty(ch, indent + 2)
  result &= ")\n"

func `$`*(gobj: GraphObject): string =
  result = gobj.pretty

func toRelative*(p: Coord1D,
                 length: Option[Quantity] = none[Quantity]()): Coord1D =
  ## converts the given coordinate to a relative coordinate
  case p.kind
  of ukRelative:
    result = p
  of ukPoint, ukCentimeter, ukInch:
    if p.length.isSome or length.isSome:
      # either get p.length or use length
      let ln = eitherOrRaise(p.length, length)
      var newPos: float
      case p.kind
      of ukPoint:
        newPos = p.pos / ln.toPoints.val
      of ukCentimeter:
        newPos = p.pos / ln.toCentimeter.val
      of ukInch:
        newPos = p.pos / ln.toInch.val
      else: raise newException(Exception, "UnitKind is invalid!")
      result = Coord1D(pos: newPos,
                       kind: ukRelative)
    else:
      raise newException(ValueError, "Cannot convert `" & $p.kind & "` into " &
        " relative coordinate if no length scale given!")
  of ukData:
    # TODO: this REQUIRES that the sclae of the Coord is up to date!
    # After potential change of data scale of plot, this will fail!
    case p.axis
    of akX:
      result = Coord1D(pos: (p.pos - p.scale.low) / (p.scale.high - p.scale.low),
                       kind: ukRelative)
    of akY:
      result = Coord1D(pos: 1.0 - (p.pos - p.scale.low) / (p.scale.high - p.scale.low),
                       kind: ukRelative)
  of ukStrWidth, ukStrHeight:
    # can either use cairo's internals, e.g. get the extent of the string in a
    # given font, or assuming a font size in dots calculate from DPI?
    # Do the former for now
    let extents = getTextExtent(p.text, p.font)
    let relevantDim = if p.kind == ukStrWidth: extents.x_bearing + extents.x_advance
                      else: extents.y_advance - extents.y_bearing
    # TODO: assume we can only use `width` here. Maybe have to consider bearing too!
    if length.isSome:
      result = Coord1D(pos: (p.pos * relevantDim) / length.unsafeGet.toPoints.val,
                       kind: ukRelative)
    else:
      raise newException(Exception,
                         "Conversion from StrWidth to relative requires a length scale!")

func toPoints*(p: Coord1D,
               length: Option[Quantity] = none[Quantity]()): Coord1D =
  ## converts the given coordinate to point based absolute values
  case p.kind
  of ukRelative:
    if length.isSome:
      let len = length.unsafeGet.toPoints
      result = Coord1D(pos: p.pos * len.val,
                       length: some(len),
                       kind: ukPoint)
    else:
      raise newException(Exception, "Cannot convert relative coordinate to " &
        "absolute points without a length scale!")
  of ukPoint, ukCentimeter, ukInch:
    var newPos: float
    case p.kind
    of ukPoint:
      newPos = p.pos
    of ukCentimeter:
      newPos = p.pos.cmToInch.inchToAbs
    of ukInch:
      newPos = p.pos.inchToAbs
    else: raise newException(Exception, "UnitKind is invalid!")
    var resLength: Option[Quantity]
    if p.length.isSome:
      resLength = some(p.length.get.toPoints)
    result = Coord1D(pos: newPos,
                     length: resLength,
                     kind: ukPoint)
  of ukData:
    result = result.toRelative.toPoints(length = length)
  of ukStrWidth, ukStrHeight:
    # can either use cairo's internals, e.g. get the extent of the string in a
    # given font, or assuming a font size in dots calculate from DPI?
    # Do the former for now
    let extents = getTextExtent(p.text, p.font)
    # TODO: assume we can only use `width` here. Maybe have to consider bearing too!
    let relevantDim = if p.kind == ukStrWidth: extents.x_bearing + extents.x_advance
                      else: extents.y_advance - extents.y_bearing#extents.height
    result = Coord1D(pos: p.pos * relevantDim,
                     kind: ukPoint)

func toRelative*(p: Coord): Coord =
  ## converts the given coordinate to a relative coordinate
  result = Coord(x: p.x.toRelative,
                 y: p.y.toRelative)

proc `==`*(c1, c2: Coord1D): bool =
  ## TODO: after certain conversion we may end up with unequal values
  ## due to floating point errors. Handle?
  # check for equality of absolute value coordinate kinds first
  if c1.kind in ukPoint .. ukInch and
     c2.kind in ukPoint .. ukInch:
    # do not require length scale
    result = c1.toPoints.pos == c2.toPoints.pos
  else:
    result = c1.toRelative.pos == c2.toRelative.pos

proc `==`*(c1, c2: Coord): bool =
  result = if c1.x == c2.x and
              c1.y == c2.y:
             true
           else:
             false

func equalKindAndScale(c1, c2: Coord1D): bool =
  ## checks whether c1 and c2 are of the same kind and if it's an
  ## absolute unit whether the two scales match
  if c1.kind != c2.kind:
    result = false
  else:
    let kind = c1.kind
    case kind
    of ukRelative:
      result = true
    of ukPoint, ukCentimeter, ukInch:
      result = if c1.length == c2.length:
                 true
               else:
                 false
    of ukData:
      result = if c1.scale == c2.scale and
                  c1.axis == c2.axis:
                 true
               else:
                 false
    else:
      raise newException(Exception, "strwidth comparison not implemented yet!")

func isAbsolute(c: Coord1D): bool =
  c.kind in {ukStrWidth, ukStrHeight, ukPoint, ukCentimeter, ukInch}

func compatibleKindAndScale(c1, c2: Coord1D): bool =
  ## checks whether c1 and c2 are of the same kind and if it's an
  ## absolute unit whether the two scales match
  if c1.isAbsolute and c2.isAbsolute:
    result = true
  elif c1.kind != c2.kind:
    # other unequal kinds are not compatible, only absolute ones
    result = false
  else:
    let kind = c1.kind
    case kind
    of ukRelative:
      result = true
    of ukData:
      result = if c1.scale == c2.scale and
                  c1.axis == c2.axis:
                 true
               else:
                 false
    of ukPoint .. ukInch: result = true # redundant, first if branch
    else:
      raise newException(Exception, "strwidth comparison not implemented yet!")

proc `+`*(c1, c2: Coord1D): Coord1D =
  ## adds two Coord1D by converting to relative coordinates if necessary.
  ## If both coordinates have the same kind and their potential scales
  ## match (if in absolute units), the result will be of the same kind
  ## as the input. Otherwise this will be a lossy conversion to relative
  ## coordinates
  if c1.compatibleKindAndScale(c2):
    if c1.isAbsolute and c2.isAbsolute:
      result = Coord1D(kind: ukPoint)
      # assign to var so we can extract `length` if any
      let c1Pts = c1.toPoints
      result.pos = c1Pts.pos + c2.toPoints.pos
      result.length = c1Pts.length
    else:
      # assign to c1 to keep the scales
      result = c1
      result.pos = c1.pos + c2.pos
  else:
    if c1.kind in {ukPoint, ukInch, ukCentimeter}:
      result = c1
      # convert to quantities and perform math on those
      let scale = if c2.kind == ukData: some(c2.scale) else: none[Scale]()
      let res = add(quant(c1.pos, c1.kind),
                    quant(c2.pos, c2.kind),
                    length = c1.length,
                    scale = scale,
                    asCoordinate = true)
      if res.unit == ukRelative:
        result = c1(res.val, ukRelative)
      else:
        result.pos = res.val
    elif c2.kind in {ukPoint, ukInch, ukCentimeter}:
      # convert to quantities and perform math on those
      result = c2
      # convert to quantities and perform math on those
      let scale = if c1.kind == ukData: some(c1.scale) else: none[Scale]()
      let res = add(quant(c1.pos, c1.kind),
                    quant(c2.pos, c2.kind),
                    length = c2.length,
                    scale = scale,
                    asCoordinate = true)
      if res.unit == ukRelative:
        result = c1(res.val, ukRelative)
      else:
        result.pos = res.val
    else:
      result = Coord1D(pos: c1.toRelative.pos + c2.toRelative.pos,
                       kind: ukRelative)

proc `-`*(c1, c2: Coord1D): Coord1D =
  ## subtracts two Coord1D by converting to relative coordinates if necessary.
  ## If both coordinates have the same kind and their potential scales
  ## match (if in absolute units), the result will be of the same kind
  ## as the input. Otherwise this will be a lossy conversion to relative
  ## coordinates
  if c1.compatibleKindAndScale(c2):
    # assign to c1 to keep correct scale
    result = c1
    if c1.isAbsolute and c2.isAbsolute:
      result.pos = c1.toPoints.pos - c2.toPoints.pos
      result.kind = ukPoint
    else:
      result.pos = c1.pos - c2.pos
  else:
    if c1.kind in {ukPoint, ukInch, ukCentimeter}:
      result = c1
      # convert to quantities and perform math on those
      let scale = if c2.kind == ukData: some(c2.scale) else: none[Scale]()
      let res = sub(quant(c1.pos, c1.kind),
                    quant(c2.pos, c2.kind),
                    length = c1.length,
                    scale = scale,
                    asCoordinate = true)
      if res.unit == ukRelative:
        result = c1(res.val, ukRelative)
      else:
        result.pos = res.val
    elif c2.kind in {ukPoint, ukInch, ukCentimeter}:
      result = c2
      # convert to quantities and perform math on those
      let scale = if c1.kind == ukData: some(c1.scale) else: none[Scale]()
      let res = sub(quant(c1.pos, c1.kind),
                    quant(c2.pos, c2.kind),
                    length = c2.length,
                    scale = scale,
                    asCoordinate = true)
      if res.unit == ukRelative:
        result = c1(res.val, ukRelative)
      else:
        result.pos = res.val
    else:
      result = Coord1D(pos: c1.toRelative.pos - c2.toRelative.pos,
                       kind: ukRelative)

proc `*`*(c1, c2: Coord1D): Coord1D =
  ## subtracts two Coord1D by converting to relative coordinates if necessary.
  ## If both coordinates have the same kind and their potential scales
  ## match (if in absolute units), the result will be of the same kind
  ## as the input. Otherwise this will be a lossy conversion to relative
  ## coordinates
  ## NOTE: a multiplication does ``not`` imply a multiplication of dimensions,
  ## but rather a pure "value multiplication", i.e. 1 cm * 1 cm != 1 cm^2!
  if c1.compatibleKindAndScale(c2):
    # assign to c1 to keep correct scale
    result = c1
    if c1.isAbsolute and c2.isAbsolute:
      result.pos = c1.toPoints.pos * c2.toPoints.pos
      result.kind = ukPoint
    else:
      result.pos = c1.pos * c2.pos
  else:
    if c1.kind in {ukPoint, ukInch, ukCentimeter}:
      result = c1
      # convert to quantities and perform math on those
      let scale = if c2.kind == ukData: some(c2.scale) else: none[Scale]()
      let res = times(quant(c1.pos, c1.kind),
                      quant(c2.pos, c2.kind),
                      length = c1.length,
                      scale = scale,
                      asCoordinate = true)
      if res.unit == ukRelative:
        result = c1(res.val, ukRelative)
      else:
        result.pos = res.val
    elif c2.kind in {ukPoint, ukInch, ukCentimeter}:
      result = c2
      # convert to quantities and perform math on those
      let scale = if c1.kind == ukData: some(c1.scale) else: none[Scale]()
      let res = times(quant(c1.pos, c1.kind),
                      quant(c2.pos, c2.kind),
                      length = c2.length,
                      scale = scale,
                      asCoordinate = true)
      if res.unit == ukRelative:
        result = c1(res.val, ukRelative)
      else:
        result.pos = res.val
    else:
      result = Coord1D(pos: c1.toRelative.pos * c2.toRelative.pos,
                       kind: ukRelative)

proc `/`*(c1, c2: Coord1D): Coord1D =
  ## divides two Coord1D by converting to relative coordinates.
  ## Note that this may be a lossy conversion, e.g. if one is given
  ## in `ukPoint` (length is lost)
  ## NOTE: a division does ``not`` imply a division of dimensions,
  ## but rather a pure "value division", i.e. 1 cm * 1 cm != 1 cm^2!
  if c1.equalKindAndScale(c2):
    # assign to c1 to keep correct scale
    result = c1
    if c1.isAbsolute and c2.isAbsolute:
      result.pos = c1.toPoints.pos / c2.toPoints.pos
      result.kind = ukPoint
    else:
      result.pos = c1.pos / c2.pos
  else:
    if c1.kind in {ukPoint, ukInch, ukCentimeter}:
      result = c1
      # convert to quantities and perform math on those
      let scale = if c2.kind == ukData: some(c2.scale) else: none[Scale]()
      let res = divide(quant(c1.pos, c1.kind),
                       quant(c2.pos, c2.kind),
                       length = c1.length,
                       scale = scale,
                       asCoordinate = true)
      if res.unit == ukRelative:
        result = c1(res.val, ukRelative)
      else:
        result.pos = res.val
    elif c2.kind in {ukPoint, ukInch, ukCentimeter}:
      result = c2
      # convert to quantities and perform math on those
      let scale = if c1.kind == ukData: some(c1.scale) else: none[Scale]()
      let res = divide(quant(c1.pos, c1.kind),
                       quant(c2.pos, c2.kind),
                       length = c2.length,
                       scale = scale,
                       asCoordinate = true)
      if res.unit == ukRelative:
        result = c1(res.val, ukRelative)
      else:
        result.pos = res.val
    else:
      result = Coord1D(pos: c1.toRelative.pos / c2.toRelative.pos,
                       kind: ukRelative)

proc to*(p: Coord1D, toKind: UnitKind,
         absLength = none[Quantity](),
         datScale = none[Scale](),
         datAxis = none[AxisKind](),
         strText = none[string](), strFont = none[Font]()): Coord1D =
  ## converts the given 1D Coordinate position in a certain coordinate
  ## system to the same position in a target coordinate system
  ## NOTE: this procedure is a potentially lossy conversion!
  # first check whether it's only a unit conversion between absolute values
  # in this case the conversion is loss free
  if p.kind == toKind: return p
  if toKind in ukPoint .. ukInch and
     p.kind in ukPoint .. ukInch:
    case toKind
    of ukPoint:
      result = p.toPoints
    of ukCentimeter:
      let newPos = p.toPoints.pos.absToInch.inchToCm
      result = Coord1D(pos: newPos,
                       length: some(p.length.get.toCentimeter),
                       kind: ukCentimeter)
    of ukInch:
      let newPos = p.toPoints.pos.absToInch
      result = Coord1D(pos: newPos,
                       length: some(p.length.get.toInch),
                       kind: ukInch)
    else: raise newException(Exception, "CoordKing is invalid!")
  else:
    # first convert any point to a relative point, from which we can
    # calculate any other position easiest
    var pRel: Coord1D
    if absLength.isSome:
      pRel = p.toRelative(absLength)
    else:
      pRel = p.toRelative

    case toKind
    of ukRelative: result = p # nothing to do
    of ukPoint:
      doAssert absLength.isSome, "Conversion to absolute requires a length scale!"
      result = Coord1D(pos: pRel.pos * absLength.get().toPoints.val,
                       length: some(absLength.get.toPoints),
                       kind: ukPoint)
    of ukInch:
      doAssert absLength.isSome, "Conversion to inches requires an absolute length scale!"
      # assumes absLength is size in points!
      result = Coord1D(pos: pRel.pos * absLength.get().toPoints.val / DPI,
                       length: some(absLength.get.toInch),
                       kind: ukInch)
    of ukCentimeter:
      doAssert absLength.isSome, "Conversion to inches requires an absolute length scale!"
      # assumes absLength is size in points!
      const inch = 2.54
      result = Coord1D(pos: pRel.pos * absLength.get().toPoints.val / DPI * inch,
                       length: some(absLength.get.toCentimeter),
                       kind: ukCentimeter)
    of ukData:
      doAssert datScale.isSome, "Conversion to data requires a scale!"
      doAssert datAxis.isSome, "Conversion to data requires an axis!"
      let scale = datScale.get()
      result = Coord1D(pos: (scale.high - scale.low) * pRel.pos + scale.low,
                       scale: scale,
                       kind: ukData)
    of ukStrWidth, ukStrHeight:
      doAssert strText.isSome, "Conversion to string width requires an string!"
      doAssert strFont.isSome, "Conversion to string width requires a Font!"
      raise newException(Exception, "Conversion to string width not yet implemented!")

proc to*(p: Coord, toKind: UnitKind,
         absWidth = none[Quantity](), absHeight = none[Quantity](),
         datXScale = none[Scale](), datYScale = none[Scale](),
         strText = none[string](), strFont = none[Font]()): Coord
  {.deprecated: "Please use `to` for the individual `Coord1D` objects instead!".} =
  ## converts the given Coordinate position in a certain coordinate
  ## system to the same position in a target coordinate system
  ## TODO: if we decide to keep this proc, replace it by calling `to` for
  ## `Coord1D` on individual fields instead.
  # first convert any point to a relative point, from which we can
  # calculate any other position easiest
  case toKind
  of ukRelative: result = p.toRelative # nothing to do
  of ukPoint:
    doAssert absWidth.isSome, "Conversion to absolute requires a width!"
    doAssert absHeight.isSome, "Conversion to absolute requires a height!"
    result = Coord(x: p.x.to(ukPoint, absLength = absWidth),
                   y: p.y.to(ukPoint, absLength = absHeight))
  of ukInch:
    doAssert absWidth.isSome, "Conversion to inches requires a width!"
    doAssert absHeight.isSome, "Conversion to inches requires a height!"
    result = Coord(x: p.x.to(ukInch, absLength = absWidth),
                   y: p.y.to(ukInch, absLength = absHeight))
  of ukCentimeter:
    doAssert absWidth.isSome, "Conversion to cm requires a width!"
    doAssert absHeight.isSome, "Conversion to cm requires a height!"
    result = Coord(x: p.x.to(ukCentimeter, absLength = absWidth),
                   y: p.y.to(ukCentimeter, absLength = absHeight))
  of ukData:
    doAssert datXScale.isSome, "Conversion to data requires an X scale!"
    doAssert datYScale.isSome, "Conversion to data requires a Y scale!"
    let
      xScale = datXScale.get()
      yScale = datYScale.get()
    result = Coord(x: p.x.to(ukData, datScale = datXScale, datAxis = some(akX)),
                   y: p.y.to(ukData, datScale = datYScale, datAxis = some(akY)))
  of ukStrWidth, ukStrHeight:
    doAssert strText.isSome, "Conversion to string width requires an string!"
    doAssert strFont.isSome, "Conversion to string width requires a Font!"
    raise newException(Exception, "Conversion to string width not yet implemented!")

func patchCoord(c: Coord1D, length: Quantity): Coord1D =
  ## patches the given coordinate in case it requries an absolute
  ## scale (ukPoint .. ukInch) to have a length field
  result = c
  if result.kind in ukPoint .. ukInch and not result.length.isSome:
    result.length = some(length)

func patchCoord(c: Coord, view: Viewport): Coord =
  ## patches the coordinate if it contains an absolute scale to have
  ## width / height inherited from its viewport
  result = Coord(x: c.x.patchCoord(view.wImg),
                 y: c.y.patchCoord(view.hImg))

proc `[]`*(view: Viewport, idx: int): Viewport =
  ## returns the `idx` child of `view`
  if view.children.len > idx:
    result = view.children[idx]
  else:
    raise newException(IndexError, "`idx` is invalid for " & $view.children.len &
      " children viewports!")

proc `[]`*(view: var Viewport, idx: int): var Viewport =
  ## returns the `idx` child of `view` as a mutable object
  if view.children.len > idx:
    result = view.children[idx]
  else:
    raise newException(IndexError, "`idx` is invalid for " & $view.children.len &
      " children viewports!")

proc embedInto(view: Viewport, into: Viewport): Viewport
proc updateSizeNewRoot(view: var Viewport) =
  ## recursively updates all children viewports to the (possibly) new
  ## size of the new root `view`.
  ## This is useful when creating a new viewport, which is to emcompass two
  ## fully fledged viewports (e.g. two ggplotnim plots).
  for ch in mitems(view.children):
    ch.wImg = view.wImg
    ch.hImg = view.hImg
    # TODO: also update the `wView`, `hView` fields!
    ch.updateSizeNewRoot()

proc `[]=`*(view: var Viewport, idx: int, viewToSet: Viewport) =
  ## override the `idx` child of `view` with `viewToSet`
  if view.children.len > idx:
    view.children[idx] = viewToSet
  else:
    raise newException(IndexError, "`idx` is invalid for " & $view.children.len &
      " children viewports!")

proc embedAt*(view: var Viewport, idx: int, viewToEmbed: Viewport) =
  ## embeds the `viewToEmbed` into `view` at child index `idx`. This proc
  ## updates the widths and heights to the `view.wImg`, `view.hImg` of all
  ## `viewToEmbed` children. This proc is to be used if one wishes to assign
  ## to `viewToEmbed` to a child of `view`, which have different sizes.
  ## Useful to combine two or more finished views to e.g. a grid.
  if view.children.len > idx:
    view[idx] = embedInto(viewToEmbed, view[idx])
    view.updateSizeNewRoot()
  else:
    raise newException(IndexError, "`idx` is invalid for " & $view.children.len &
      " children viewports!")

proc len*(view: Viewport): int = view.children.len
proc high*(view: Viewport): int = view.len - 1

proc left(view: Viewport): Coord1D =
  ## returns the left (x) position of the `Viewport` in `ukRelative`
  ## as a 1D coordinate
  result = view.origin.x.toRelative

proc bottom(view: Viewport): Coord1D =
  ## returns the bottom (y) position of the `Viewport` in `ukRelative`
  ## as a 1D coordinate
  result = view.origin.y.toRelative

proc width*(view: Viewport): Quantity =
  ## returns the width of the `Viewport` in `ukRelative`
  ## NOTE: this procedure is a no-op, if the width is already stored as a
  ## ukRelative!
  result = view.width.toRelative(length = some(view.wImg))

proc height*(view: Viewport): Quantity =
  ## returns the height of the `Viewport` in `ukRelative`
  ## NOTE: this procedure is a no-op, if the height is already stored as a
  ## ukRelative!
  result = view.height.toRelative(length = some(view.hImg))

proc pointWidth*(view: Viewport): Quantity {.inline.} =
  ## returns the width of the given viewport in absolute points
  assert view.wView.unit == ukPoint
  result = times(view.wView, view.width.toRelative(length = some(view.wView)),
                 length = some(view.wView))

proc pointHeight*(view: Viewport): Quantity {.inline.} =
  ## returns the height of the given viewport in absolute points
  assert view.hView.unit == ukPoint
  result = times(view.hView, view.height.toRelative(length = some(view.hView)),
                 length = some(view.hView))

func updateScale(view: Viewport, c: var Coord1D) =
  ## update the scale coordinate of the 1D coordinate `c` in place
  if c.kind == ukData:
    case c.axis
    of akX:
      c.scale = view.xScale
    of akY:
      c.scale = view.yScale

func updateScale(view: Viewport, c: Coord1D): Coord1D =
  ## update the scale coordinate of the 1D coordinate `c` in place
  result = c
  view.updateScale(result)

func updateScale(view: Viewport, c: var Coord) =
  ## update the scale coordinate of the coordinate `c` in place
  view.updateScale(c.x)
  view.updateScale(c.y)

func updateScale(view: Viewport, c: Coord): Coord =
  ## same as above, but returns a mutated copy
  result = c
  view.updateScale(result)

func updateDataScale(view: Viewport, obj: var GraphObject) =
  ## updates the data scale associated to the `obj`
  case obj.kind
  of goLine, goAxis:
    view.updateScale(obj.lnStart)
    view.updateScale(obj.lnStop)
  of goLabel, goText, goTickLabel:
    view.updateScale(obj.txtPos)
  of goGrid:
    obj.gdXPos.applyIt(view.updateScale(it))
    obj.gdYPos.applyIt(view.updateScale(it))
  of goTick:
    view.updateScale(obj.tkPos)
  of goPoint:
    view.updateScale(obj.ptPos)
  of goPolyLine:
    obj.plPos.applyIt(view.updateScale(it))
  of goRect:
    view.updateScale(obj.reOrigin)
  of goComposite:
    # call this func for all children of the composite
    for ch in mitems(obj.children):
      view.updateDataScale(ch)
  #else:
  #  raise newException(Exception, "updating of " & $(obj.kind) & " not yet implemented!")

func updateDataScale*(view: Viewport,
                      objs: var seq[GraphObject]) =
  ## updates the data scales associated to the `objs` to the current
  ## `view.(x|y)scale`. This is important, because due to calculation
  ## of tick locations (and thus new range scale) the associated scales
  ## may be wrong.
  ## Potentially other objects may also have an associated scale (e.g.
  ## the user may want to define some location in scale coordinates),
  ## hence check for any Coord in the object.
  for p in mitems(objs):
    view.updateDataScale(p)

func updateDataScale*(view: Viewport) =
  ## Updates the data scales of all children viewports of `view` and their
  ## objects recursively
  ## Note that if `view` contains many children or children with many objects
  ## this may potentially be quite expensive!
  view.updateDataScale(view.objects)
  for ch in mitems(view.children):
    ch.xScale = view.xScale
    ch.yScale = view.yScale
    # and call for child itself
    ch.updateDataScale()

func addObj*(view: Viewport, obj: GraphObject) =
  ## adds the given `obj` to the viewport's objects and makes
  ## sure it inherits all properties, e.g. Style and data scales
  var mobj = obj
  # check if user assigned a style to overwrite viewport style
  if not obj.style.isSome:
    mobj.style = some(view.style)
  # (potentially) update data scale
  # debugecho "Updating scale of ", mobj
  #view.updateDataScale(mobj)
  # debugecho "Is now ", mobj
  # debugecho "\n\n"
  view.objects.add mobj

func addObj*(view: Viewport, objs: varargs[GraphObject]) =
  ## adds the `objs` to the viewport's objects and makes
  ## sure they inherit all properties, e.g. Style and data scales
  for obj in objs:
    view.addObj obj

proc convertToKind(c: Coord1D, toKind: Coord1D): Coord1D =
  ## converts the coordinate `c` to the kind of `toKind`
  case toKind.kind
  of ukRelative:
    result = c.toRelative
  of ukPoint:
    if toKind.length.isSome:
      result = c.to(ukPoint,
                    absLength = toKind.length)
    else:
      raise newException(ValueError, "Conversion to `ukPoint` requires a length!")
  of ukInch:
    if toKind.length.isSome:
      result = c.to(ukInch,
                    absLength = toKind.length)
    else:
      raise newException(ValueError, "Conversion to `ukInch` requires a length!")
  of ukCentimeter:
    if toKind.length.isSome:
      result = c.to(ukCentimeter,
                    absLength = toKind.length)
    else:
      raise newException(ValueError, "Conversion to `ukCentimeter` requires a length!")
  of ukData:
    result = c.to(ukData,
                  datScale = some(toKind.scale),
                  datAxis = some(toKind.axis))
  else:
    raise newException(Exception, "convertToKind not implemented for " & $toKind.kind)

proc embedInto*(q: Quantity, axKind: AxisKind, view: Viewport): Quantity =
  ## Embeds the quantity `q` into the viewport `view`
  ## NOTE: Embedding a quantity is a special case. A quantity (in our case)
  ## describes a length. This means that any absolute quantity ``must not``
  ## be scaled, whereas a relative quantity "0.2 * viewport size" ``must`` be.
  case q.unit
  of ukRelative:
    case axKind
    of akX:
      result = times(view.width, q, length = some(pointWidth(view)), scale = some(view.xScale))
    of akY:
      result = times(view.height, q, length = some(pointHeight(view)), scale = some(view.yScale))
  of ukPoint, ukCentimeter, ukInch:
    # do nothing, already an absolute value. Respect that!
    result = q
  of ukData:
    case axKind
    of akX:
      result = quant(
        width(view).val * q.toRelative(
          scale = some(view.xScale)
        ).val,
        ukRelative)
    of akY:
      result = quant(
        height(view).val * q.toRelative(
          scale = some(view.yScale)
        ).val,
        ukRelative)
  else:
    raise newException(Exception, "Embedding not implemented for quantity of " &
      " kind: " & $q)

proc embedInto(c: Coord1D, axKind: AxisKind, view: Viewport): Coord1D =
  ## embeds the coordinate `c` into `Viewport`
  var pos: Coord1D
  case axKind
  of akX:
    case c.kind
    of ukPoint, ukCentimeter, ukInch:
      let origAbs = view.origin.x.to(ukPoint, absLength = some(view.wImg))
      # NOTE: for a Coord1D giving a *value* the following is a scaling, but for
      # a Coord1D giving a *coordinate* it is a translation!
      pos = origAbs + c
    else:
      pos = Coord1D(pos: left(view).pos + width(view).val * c.toRelative.pos,
                    kind: ukRelative)
    result = pos
  of akY:
    case c.kind
    of ukPoint, ukCentimeter, ukInch:
      let origAbs = view.origin.y.to(ukPoint, absLength = some(view.hImg))
      pos = origAbs + c
    else:
      pos = Coord1D(pos: bottom(view).pos + height(view).val * c.toRelative.pos,
                    kind: ukRelative)
    result = pos

proc embedInto(c: Coord, view: Viewport): Coord =
  ## embeds the coordinate `c` into `Viewport`
  let
    cX = c.x.embedInto(akX, view)
    cY = c.y.embedInto(akY, view)
  result = Coord(x: cX, y: cY)

proc embedInto(view: Viewport, into: Viewport): Viewport =
  ## embeds the given `view` into the `into` Viewport by embedding
  ## the (left, bottom) coordinates and scaling the width / height
  result = view
  result.origin = result.origin.embedInto(into)
  result.width = embedInto(result.width, akX, into)
  result.height = embedInto(result.height, akY, into)

proc point(c: Coord): Point =
  ## converts the given coordinate to `ukRelative` and returns the position as a
  ## `Point`
  result = (x: c.x.pos, y: c.y.pos)

################################################################################
############ INIT FUNCTIONS
################################################################################






proc initViewport*(origin: Coord,
                   width, height: Quantity,
                   style = none[Style](),
                   xScale = none[Scale](),
                   yScale = none[Scale](),
                   rotate = none[float](),
                   scale = none[float](),
                   name = "",
                   parent = "",
                   wImg = 640.0,
                   hImg = 480.0,
                   wParentView: Option[Quantity] = none[Quantity](),
                   hParentView: Option[Quantity] = none[Quantity](),
                   backend = bkCairo): Viewport =
  ## initializes a `Viewport` with `origin` in any coordinate system
  ## with 1D coordinates providing width and height
  ## Uses Coord1D to allow to define sizes in arbitrary units
  result = Viewport(name: name,
                    parent: parent,
                    origin: origin,
                    width: width,
                    height: height,
                    rotate: rotate,
                    scale: scale,
                    wImg: quant(wImg, ukPoint),
                    hImg: quant(hImg, ukPoint),
                    backend: backend)
  debug "[DEBUG]: Initing viewport ", width.toRelative(some(result.wImg))
  if wParentView.isSome and hParentView.isSome:
    doAssert wParentView.get.unit == ukPoint and
      hParentView.get.unit == ukPoint, "parent size must be given in `ukPoint`!"
    result.wView = wParentView.get#quant(wParentView.get.toRelative(result.wImg).val, ukPoint)
    result.hView = hParentView.get#quant(hParentView.get.toRelative(result.hImg).val, ukPoint)
  else:
    warn "[WARNING]: initializing viewport at ", origin, " with absolute " &
      "sizes equal to image sizes!"
    result.wView = result.wImg#quant(wImg * width.toRelative(result.wImg).val, ukPoint)
    result.hView = result.hImg#quant(wImg * width.toRelative(result.hImg).val, ukPoint)#quant(hImg, ukPoint)
  if style.isSome:
    result.style = style.get()
  if xScale.isSome:
    result.xScale = xScale.get()
  if yScale.isSome:
    result.yScale = yScale.get()

proc initViewport*(left = 0.0, bottom = 0.0, width = 1.0, height = 1.0,
                   style = none[Style](),
                   xScale = none[Scale](),
                   yScale = none[Scale](),
                   rotate = none[float](),
                   scale = none[float](),
                   name = "",
                   parent = "",
                   wImg = 640.0,
                   hImg = 480.0,
                   backend = bkCairo): Viewport =
  ## convenience init function for Viewport using relative coordinates
  ## NOTE: this function should only be used to create a Viewport within the
  ## main image viewport! Otherwise use the `addViewport` procs on a viewport!
  let origin = Coord(x: Coord1D(pos: left, kind: ukRelative),
                     y: Coord1D(pos: bottom, kind: ukRelative))
  let
    widthCoord = quant(width, ukRelative)
    heightCoord = quant(height, ukRelative)
  result = initViewport(origin = origin, width = widthCoord, height = heightCoord,
                        style = style, xScale = xScale, yScale = yScale,
                        rotate = rotate, scale = scale,
                        name = name, parent = parent,
                        wImg = wImg, hImg = hImg,
                        backend = backend)

proc addViewport*(view: Viewport,
                  origin: Coord,
                  width, height: Quantity,
                  style = none[Style](),
                  xScale = none[Scale](),
                  yScale = none[Scale](),
                  rotate = none[float](),
                  scale = none[float](),
                  name = ""): Viewport =
  ## add a new viewport with the given settings to the `view`
  ## TODO: do not return viewchild???
  var viewChild = initViewport(origin = origin.patchCoord(view),
                               width = width,#.patchCoord(view.wImg),
                               height = height,#.patchCoord(view.hImg),
                               style = style, xScale = xScale, yScale = yScale,
                               rotate = rotate, scale = scale,
                               name = name, parent = view.name,
                               wImg = view.wImg.toPoints.val,
                               hImg = view.hImg.toPoints.val,
                               # TODO: clean this up
                               wParentView = some(pointWidth(view)),
                               hParentView = some(pointHeight(view)),
                               backend = view.backend)
  # override width and height
  ## echo "TODO: make sure we want to give child viewport scaled (wImg, hImg)!"
  #viewChild.wImg = quant(view.wImg.val * width.toRelative(view.wImg).val, ukPoint)
  #viewChild.hImg = quant(view.hImg.val * height.toRelative(view.hImg).val, ukPoint)
  # TODO: this is not useful, since all objects have value semantics, i.e. if we change it
  # the change is not reflected, since we work on a copy.
  #view.children.add viewChild
  result = viewChild

proc addViewport*(view: Viewport,
                  left = 0.0, bottom = 0.0, width = 1.0, height = 1.0,
                  style = none[Style](),
                  xScale = none[Scale](),
                  yScale = none[Scale](),
                  rotate = none[float](),
                  scale = none[float](),
                  name = ""): Viewport =
  ## add a new viewport with the given settings to the `view`, set at relative
  ## coordinates (left, bottom), (width, height)
  ## TODO: Do not return viewchild???
  let origin = Coord(x: Coord1D(pos: left, kind: ukRelative),
                     y: Coord1D(pos: bottom, kind: ukRelative))
  let
    widthCoord = quant(width, ukRelative)
    heightCoord = quant(height, ukRelative)
  var
    xSc: Option[Scale]
    ySc: Option[Scale]
  if not xScale.isSome:
    xSc = some(view.xScale)
  else:
    xSc = xScale
  if not yScale.isSome:
    ySc = some(view.yScale)
  else:
    ySc = yScale
  result = view.addViewport(origin = origin, width = widthCoord, height = heightCoord,
                            style = style, xScale = xSc, yScale = ySc,
                            rotate = rotate, scale = scale,
                            name = name)

proc initAxis(view: Viewport,
              axKind: AxisKind,
              width = 1.0,
              color = color(0.0, 0.0, 0.0)): GraphObject =
  var axis = GraphObject(kind: goAxis,
                         lnStart: initCoord(0.0, 1.0),
                         lnStop: initCoord(1.0, 1.0),
                         style: some(Style(color: color,
                                           lineWidth: width)))
  case axKind
  of akX:
    result = axis
    result.name = "xAxis"
  of akY:
    result = replace(axis):
      lnStart = initCoord(0.0, 0.0)
      lnStop = initCoord(0.0, 1.0)
    result.name = "yAxis"

proc xaxis(view: Viewport,
           width = 1.0,
           color = color(0.0, 0.0, 0.0)): GraphObject =
  result = view.initAxis(akX, width, color)

proc yaxis(view: Viewport,
           width = 1.0,
           color = color(0.0, 0.0, 0.0)): GraphObject =
  result = view.initAxis(akY, width, color)

proc initRect*(view: Viewport,
               origin: Coord,
               width, height: Quantity,
               color = color(0.0, 0.0, 0.0),
               gradient = none[Gradient](),
               style = none[Style](),
               rotate = none[float](),
               name = "rect"): GraphObject =
  result = GraphObject(kind: goRect,
                       name: name,
                       reOrigin: origin.patchCoord(view),
                       reWidth: width,
                       reHeight: height,
                       rotate: rotate)
  if style.isSome:
    result.style = style
  else:
    result.style = some(Style(lineWidth: 0.0,
                              color: color(0.0, 0.0, 0.0, 0.0),
                              size: 0.0,
                              lineType: ltSolid,
                              fillColor: color,
                              gradient: gradient))

proc initRect*(view: Viewport,
               left, bottom, width, height: float,
               color = color(0.0, 0.0, 0.0),
               gradient = none[Gradient](),
               style = none[Style](),
               name = "rect"): GraphObject =
  let origin = Coord(x: Coord1D(pos: left, kind: ukRelative),
                     y: Coord1D(pos: bottom, kind: ukRelative))
  let
    widthCoord = quant(width, ukRelative)
    heightCoord = quant(height, ukRelative)
  result = view.initRect(origin = origin,
                         width = widthCoord,
                         height = heightCoord,
                         color = color,
                         style = style,
                         gradient = gradient,
                         name = name)

proc initText*(view: Viewport,
               origin: Coord,
               text: string,
               textKind: static GraphObjectKind,
               alignKind: TextAlignKind,
               font: Option[Font] = none[Font](),
               rotate = none[float](),
               name = "text"): GraphObject =
  ## Creates a text based `GraphObject` of kind `textKind`.
  ## This proc does ``not`` support multiple lines! Use `initMultiText`
  ## instead if that is required.
  assert textKind in {goText, goLabel, goTickLabel}
  result = GraphObject(kind: textKind,
                       name: name,
                       txtText: text,
                       txtAlign: alignKind,
                       txtPos: origin.patchCoord(view))
  if rotate.isSome:
    result.rotate = some(rotate.get())
  setFontOrDefault(result, font)

proc drawBoundary*(view: Viewport,
                   color = none[Color](),
                   writeName = false,
                   writeNumber = none[int](),
                   style = none[Style]()) =
  ## Adds a boundary (a rectangle) around the given viewport
  var mstyle: Option[Style]
  if style.isNone:
    let c = if color.isSome: color.unsafeGet else: black
    mstyle = some(Style(lineWidth: 1.0,
                        color: c,
                        fillColor: transparent,
                        lineType: ltSolid))
  else: mstyle = style
  let rect = view.initRect(left = 0.0, bottom = 0.0,
                           width = 1.0, height = 1.0,
                           style = mstyle)
  view.addObj rect
  if writeName:
    # write name of layout in center
    let text = view.initText(c(0.5, 0.5),
                             text = view.name,
                             textKind = goText,
                             alignKind = taCenter)
    view.addObj text
  if writeNumber.isSome:
    # write name of layout in center
    let text = view.initText(c(0.5, 0.5),
                             text = $writeNumber.unsafeGet,
                             textKind = goText,
                             alignKind = taCenter)
    view.addObj text

proc strHeight*(val: float, font: Font): Coord1D =
  ## returns a Coord1D of kind `ukStrHeight` for the given
  ## number of times the string height `val` for font `font`.
  ## We use `'W'` to determine the height of the given font
  result = Coord1D(pos: val, kind: ukStrHeight,
                   text: "W",
                   font: font)

proc getStrHeight*(text: string, font: Font): Quantity =
  ## returns a quantity of the height of the given `text` under
  ## the given `font`, taking into account multiple lines. The
  ## result is of kind `ukPoint`.
  ## NOTE: This currently uses a hardcoded line spacing, which is
  ## the same as the one used in `initMultiLineText` below!
  let numLines = text.splitLines.len
  # ``N lines + (N - 1) * (LineSpacing - 1.0)``
  result = quant(
    val = toPoints(
      strHeight(numLines.float * 1.75, font),
      #strHeight((numLines).float + (numLines - 1).float * 0.75, font)
      #strHeight((numLines.float - 1.0) * 1.75, font)
    ).pos,
    unit = ukPoint
  )

proc getStrWidth*(text: string, font: Font): Quantity =
  ## returns a Quantity of kind `ukStrWidth` for the given
  ## string under the font `font` of unit kind `ukPoint`
  result = quant(
    val = toPoints(
      Coord1D(pos: 1.0, kind: ukStrWidth,
              text: text,
              font: font)
    ).pos,
    unit = ukPoint
  )

proc initMultiLineText*(view: Viewport,
                        origin: Coord,
                        text: string,
                        textKind: static GraphObjectKind,
                        alignKind: TextAlignKind,
                        fontOpt: Option[Font] = none[Font](),
                        rotate = none[float](),
                        name = "multiLineText"): seq[GraphObject] =
  ## Creates a text based `GraphObject` of kind `textKind`.
  ## This proc will split the input `text` by lines and return
  ## a properly spaced sequence of text objects, which conform to
  ## the desired text alignment and font while keeping a spacing
  ## of 0.75 of the text height.
  ## NOTE: The current spacing of 0.75 seems excessive to me. As far
  ## as I'm aware typical spacings range from 0.2 to 0.45
  ## Maybe our calculation for the string height or the addition of
  ## those coordinates is off! Thus for the time being, we leave this
  ## hardcoded, since this works for now.
  assert textKind in {goText, goLabel, goTickLabel}
  let font = if fontOpt.isSome: fontOpt.unsafeGet else: defaultFont()
  let lines = text.splitLines
  let totalHeight = getStrHeight(text, font)
  let numLines = lines.len
  for idx, line in lines:
    # calculate new y position based on previous position and
    # number of lines
    # we subtract -1.0 * height for (N - 1) spacings
    # and an additional (-0.5 * height) to account for current text being
    # centered on center of line, not bottom
    let newY = origin.y - toRelative(
      strHeight((numLines.float - idx.float - 0.5).float * 1.75, font),
      length = some(view.hImg)
    )
    let newOrigin = Coord(x: origin.x,
                          y: newY)
    result.add view.initText(origin = newOrigin,
                             text = line,
                             textKind = textKind,
                             alignKind = alignKind,
                             font = some(font),
                             rotate = rotate,
                             name = $name & $idx
    )

proc scaleTo(p: Point, view: Viewport): Point =
  ## scales the point from data coordinates to viewport coordinates
  discard

proc initLine*(view: Viewport,
               start: Coord,
               stop: Coord,
               style: Option[Style] = none[Style](),
               name = "line"): GraphObject =
  result = GraphObject(kind: goLine,
                       name: name,
                       lnStart: start,
                       lnStop: stop)
  if style.isSome:
    result.style = style
  else:
    result.style = some(
      Style(lineWidth: 1.0,
            color: black)
    )

func initPoint*(view: Viewport,
                pos: Coord,
                style: Style,
                name = "point"): GraphObject =
  result = GraphObject(kind: goPoint,
                       name: name,
                       ptMarker: style.marker,
                       ptSize: style.size,
                       ptColor: style.color,
                       ptPos: pos)

proc initPoint*(view: Viewport,
                pos: Coord,
                size = 3.0,
                marker: MarkerKind = mkCircle,
                color = color(0.0, 0.0, 0.0),
                name = "point"): GraphObject {.inline.} =
  let style = Style(marker: marker, size: size, color: color)
  result = view.initPoint(pos = pos, style = style, name = name)

proc initPoint*(view: Viewport,
                pos: Point,
                size = 3.0,
                marker: MarkerKind = mkCircle,
                color = color(0.0, 0.0, 0.0),
                name = "point"): GraphObject {.inline.} =
  let style = Style(marker: marker, size: size, color: color)
  let pos = Coord(x: Coord1D(pos: pos.x,
                             scale: view.xScale,
                             axis: akX,
                             kind: ukData),
                  y: Coord1D(pos: pos.y,
                             scale: view.yScale,
                             axis: akY,
                             kind: ukData))
  result = view.initPoint(pos = pos, style = style, name = name)

func isScaleNonTrivial(c: Coord1D): bool =
  doAssert c.kind == ukData, "coord must be of kind ukData!"
  result = not (c.scale.low == c.scale.high)

func isScaleNonTrivial(c: Coord): bool =
  result = c.x.isScaleNonTrivial and c.y.isScaleNonTrivial

proc initErrorBar*(view: Viewport,
                   pt: Coord,
                   errorUp: Coord1D,
                   errorDown: Coord1D,
                   axKind: AxisKind,
                   ebKind: ErrorBarKind,
                   style: Option[Style] = none[Style](),
                   name = "errorBar"): GraphObject =
  ## creates an error bar for the point `pt` of kind `ebKind` with the
  ## errors given by `errorUp`  and `errorDown` along the axis `axKind`.
  ## If the `axKind` is `akX`, `errorUp` will describe the increase along
  ## the X axis (to the right).
  ## NOTE: this proc assumes that if the errors are given as `ukData`, the
  ## scales associated are the same as for the data point!
  result = GraphObject(kind: goComposite, name: name)
  if style.isSome:
    result.style = style
  else:
    result.style = some(
      Style(lineWidth: 1.0, # describes width of lines
            color: black,
            size: 10.0) # describes length of orthogonal line of `ebLinesT`
    )
  # in case the user hands the errors as `ukData`, update the scale
  #doAssert pt.ptPos.isScaleNonTrivial, "Data scale must be non trivial!"
  #var
  #  # error variables with appropriate scales, if `ukData`
  #  errUp: Coord1D
  #  errDown: Coord1D
  #errUp = view.updateScale(errorUp)
  #errDown = view.updateScale(errorDown)
  template createLines(axKind, x1, x2, y1, y2: untyped): untyped =
    let chUp = view.initLine(
      start = pt,
      stop = Coord(
        x: x1,
        y: y1
      ),
      style = style
    )
    let chDown = view.initLine(
      start = pt,
      stop = Coord(
        x: x2,
        y: y2,
      ),
      style = style
    )
    result.children = @[chDown, chUp]

  case ebKind
  of ebLines:
    case axKind
    of akX:
      createLines(akX,
                  x1 = errorUp,
                  x2 = errorDown,
                  y1 = pt.y,
                  y2 = pt.y)
    of akY:
      createLines(akY,
                  x1 = pt.x,
                  x2 = pt.x,
                  y1 = errorUp,
                  y2 = errorDown)
  of ebLinesT:
    let locStyle = result.style.get()
    case axKind
    of akX:
      createLines(akX,
                  x1 = errorUp,
                  x2 = errorDown,
                  y1 = pt.y,
                  y2 = pt.y)
      let chRight = view.initLine(
        start = Coord(x: errorUp,
                      y: pt.y - view.c1(locStyle.size, akY, ukPoint)),
        stop = Coord(x: errorUp,
                     y: pt.y + view.c1(locStyle.size, akY, ukPoint)),
        style = style
      )
      let chLeft = view.initLine(
        start = Coord(x: errorDown,
                      y: pt.y - view.c1(locStyle.size, akY, ukPoint)),
        stop = Coord(x: errorDown,
                     y: pt.y + view.c1(locStyle.size, akY, ukPoint)),
        style = style
      )
      result.children.add @[chRight, chLeft]
    of akY:
      createLines(akY,
                  x1 = pt.x,
                  x2 = pt.x,
                  y1 = errorUp,
                  y2 = errorDown)
      let chUp = view.initLine(
        start = Coord(x: pt.x - view.c1(locStyle.size, akX, ukPoint),
                      y: errorUp),
        stop = Coord(x: pt.x + view.c1(locStyle.size, akX, ukPoint),
                     y: errorUp),
        style = style
      )
      let chDown = view.initLine(
        start = Coord(x: pt.x - view.c1(locStyle.size, akX, ukPoint),
                      y: errorDown),
        stop = Coord(x: pt.x + view.c1(locStyle.size, akX, ukPoint),
                     y: errorDown),
        style = style
      )
      result.children.add @[chUp, chDown]
  #else: discard

proc initErrorBar*(view: Viewport,
                   pt: Point,
                   errorUp: Coord1D,
                   errorDown: Coord1D,
                   axKind: AxisKind,
                   ebKind: ErrorBarKind,
                   style: Option[Style] = none[Style](),
                   name = "errorBar"): GraphObject =
  ## This version of `initErrorBar` uses the `errorUp` and `errorDown`
  ## values as absolute values. There is no reference point. The
  ## given `pt` is used to determine the x or y value of the
  ## orthogonal axis.
  var pos: Coord
  case axKind
  of akX:
    pos = Coord(x: Coord1D(pos: pt.x,
                           scale: view.xScale,
                           axis: akX,
                           kind: ukData),
                y: Coord1D(pos: pt.y,
                           scale: view.yScale,
                           axis: akY,
                           kind: ukData))
  of akY:
    pos = Coord(x: Coord1D(pos: pt.x,
                           scale: view.xScale,
                           axis: akX,
                           kind: ukData),
                y: Coord1D(pos: pt.y,
                           scale: view.yScale,
                           axis: akY,
                           kind: ukData))
  result = view.initErrorBar(pos, errorUp, errorDown,
                             axKind, ebKind, style, name)


proc initPolyLine*(view: Viewport,
                   pos: seq[Coord],
                   style: Option[Style] = none[Style](),
                   name = "polyLine"): GraphObject =
  result = GraphObject(kind: goPolyLine, name: name)
  if style.isSome:
    result.style = style
  else:
    result.style = some(Style(lineWidth: 2.0,
                              lineType: ltSolid,
                              color: black,
                              fillColor: transparent))
  result.plPos = pos

proc initPolyLine*(view: Viewport,
                   pos: seq[Point],
                   style: Option[Style] = none[Style](),
                   name = "polyLine"): GraphObject =
  var posCoords = newSeq[Coord](pos.len)
  for i, p in pos:
    posCoords[i] = Coord(
      x: Coord1D(pos: p.x, scale: view.xScale, axis: akX, kind: ukData),
      y: Coord1D(pos: p.y, scale: view.yScale, axis: akY, kind: ukData)
    )
  result = initPolyLine(view, pos = posCoords, style = style, name = name)


proc initAxisLabel[T: Quantity | Coord1D](view: Viewport,
                                          label: string,
                                          axKind: AxisKind,
                                          margin: T,
                                          font: Option[Font] = none[Font](),
                                          name = "AxisLabel",
                                          isCustomMargin = false,
                                          isSecondary = false,
                                          rotate: Option[float] = none[float]()): GraphObject =
  ## If `isCustomMargin` is set, the raw `margin` value is used to set
  ## the margin. Otherwise a 0.5cm offset is added to the margin, since that way
  ## the margin is relative to the right/top edge of the tick label positions.
  ## margin is positive value!
  # start with an offset of 0.5cm on top of the given `margin`
  var marginVal: float
  if not isCustomMargin:
    marginVal = quant(0.5, ukCentimeter).toPoints.val
  # the minimum, if `margin + marginVal` ends up being smaller is
  # `1 cm`
  let marginMin = quant(1.0, ukCentimeter).toPoints.val
  when T is Quantity:
    marginVal += margin.toPoints.val
  else:
    marginVal += margin.toPoints.pos
  if marginVal < marginMin and not isCustomMargin:
    marginVal = marginMin

  result = GraphObject(kind: goLabel,
                       name: name,
                       txtText: label,
                       txtAlign: taCenter)
  # set font or use default
  setFontOrDefault(result, font)
  var gobjName = name
  case axKind
  of akX:
    # TODO: fix positions based on absolute unit (e.g. cm) instead of
    # relatives?
    let yPos = XAxisYPos(some(view), marginVal, isSecondary = isSecondary)
    result.txtPos = Coord(x: initCoord1D(0.5),
                          y: ypos)
    if gobjName == "AxisLabel":
      gobjName = "x" & name
  of akY:
    let xPos = YAxisXPos(some(view), marginVal, isSecondary = isSecondary)
    result.txtPos = Coord(x: xPos,
                          y: initCoord1D(0.5))
    result.rotate = some(-90.0)
    if gobjName == "AxisLabel":
      gobjName = "y" & name
  # apply given rotation
  if rotate.isSome:
    var rot = if result.rotate.isSome: result.rotate.get else: 0.0
    rot = rotate.get + rot
    result.rotate = some(rot)
  # set the name
  result.name = gobjName

proc xlabel*(view: Viewport,
             label: string,
             margin: Coord1D,
             font = defaultFont(),
             name = "xLabel",
             isSecondary = false,
             rotate = none[float]()): GraphObject =
  result = view.initAxisLabel(label = label,
                              axKind = akX,
                              margin = margin,
                              font = some(font),
                              name = name,
                              isSecondary = isSecondary,
                              rotate = rotate)

proc xlabel*(view: Viewport,
             label: string,
             font = defaultFont(),
             margin = 1.0,
             name = "xLabel",
             isCustomMargin = false,
             isSecondary = false,
             rotate = none[float]()): GraphObject =
  ## margin assumed to be in `cm`!
  result = view.initAxisLabel(label = label,
                              axKind = akX,
                              margin = quant(margin, ukCentimeter),
                              font = some(font),
                              name = name,
                              isCustomMargin = isCustomMargin,
                              isSecondary = isSecondary,
                              rotate = rotate)

proc ylabel*(view: Viewport,
             label: string,
             margin: Coord1D,
             font = defaultFont(),
             name = "yLabel",
             isSecondary = false,
             rotate = none[float]()): GraphObject =
  result = view.initAxisLabel(label = label,
                              axKind = akY,
                              margin = margin,
                              font = some(font),
                              name = name,
                              isSecondary = isSecondary,
                              rotate = rotate)

proc ylabel*(view: Viewport,
             label: string,
             font = defaultFont(),
             margin = 1.0,
             name = "yLabel",
             isCustomMargin = false,
             isSecondary = false,
             rotate = none[float]()): GraphObject =
  ## Margin assumed to be in `cm`!
  result = view.initAxisLabel(label = label,
                              axKind = akY,
                              margin = quant(margin, ukCentimeter),
                              font = some(font),
                              name = name,
                              isCustomMargin = isCustomMargin,
                              isSecondary = isSecondary,
                              rotate = rotate)

template xLabelOriginOffset(isSecondary = false): untyped =
  if not isSecondary:
    Coord1D(pos: -0.4,
            kind: ukCentimeter,
            length: some(pointWidth(view)))
  else:
    Coord1D(pos: 0.4,
            kind: ukCentimeter,
            length: some(pointWidth(view)))

template yLabelOriginOffset(isSecondary = false): untyped =
  if not isSecondary:
    Coord1D(pos: 0.5,
            kind: ukCentimeter,
            length: some(pointHeight(view)))
  else:
    # TODO: check if value good!
    Coord1D(pos: -0.5,
            kind: ukCentimeter,
            length: some(pointHeight(view)))

proc setTextAlignKind(axKind: AxisKind,
                      isSecondary = false,
                      alignOverride = none[TextAlignKind]()): TextAlignKind =
  ## sets the `TextAlignKind` correctly taking into account both the
  ## axis we're considering and whether an override is available
  if alignOverride.isSome:
    result = alignOverride.get
  else:
    case axKind
    of akX: result = taCenter
    of akY:
      if not isSecondary:
        result = taRight
      else:
        result = taLeft

proc initTickLabel(view: Viewport,
                   tick: GraphObject,
                   labelTxt: string,
                   font: Option[Font] = none[Font](),
                   rotate = none[float](),
                   margin = none[Coord1D](),
                   name = "tickLabel",
                   isSecondary = false,
                   alignToOverride = none[TextAlignKind]()): GraphObject =
  doAssert tick.kind == goTick, "object must be a `goTick` to create a `goTickLabel`!"
  let mfont = if font.isNone: some(defaultFont(8.0)) else: font
  var label: GraphObject
  var gobjName = name
  var origin: Coord
  let loc = tick.tkPos

  let alignTo = setTextAlignKind(tick.tkAxis, isSecondary, alignToOverride)
  case tick.tkAxis
  of akX:
    let yOffset = if margin.isSome: margin.unsafeGet
                  else: yLabelOriginOffset(isSecondary)
    origin = Coord(x: loc.x,
                   y: (loc.y + yOffset).toRelative)
    if gobjName == "tickLabel":
      gobjName = "x" & name
    result = view.initText(origin, labelTxt, textKind = goTickLabel,
                           alignKind = alignTo,
                           font = mfont,
                           rotate = rotate,
                           name = gobjName)
  of akY:
    let xOffset = if margin.isSome: margin.unsafeGet
                  else: xLabelOriginOffset(isSecondary)
    origin = Coord(x: (loc.x + xOffset).toRelative,
                   y: loc.y)
    if gobjName == "tickLabel":
      gobjName = "y" & name
    result = view.initText(origin, labelTxt,
                           textKind = goTickLabel,
                           alignKind = alignTo,
                           font = mfont,
                           rotate = rotate,
                           name = gobjName)

proc axisCoord*(c: Coord1D, axKind: AxisKind,
                isSecondary = false): Coord =
  ## A convenience proc, which returns a `Coord` on the given `axKind`.
  ## `c` is the `Coord1D` along that axis.
  case axKind
  of akX:
    result = Coord(x: c,
                   y: XAxisYPos(isSecondary = isSecondary))
  of akY:
    result = Coord(x: YAxisXPos(isSecondary = isSecondary),
                   y: c)

proc tickLabels*(view: Viewport, ticks: seq[GraphObject],
                 font: Option[Font] = none[Font](),
                 margin = none[Coord1d](),
                 isSecondary = false,
                 format: proc(x: float): string = nil,
                ): seq[GraphObject] =
  ## returns all tick labels for the given ticks
  ## TODO: Clean up the auto subtraction code!
  doAssert ticks[0].kind == goTick
  let axKind = ticks[0].tkAxis
  let mfont = if font.isNone: some(defaultFont(8.0)) else: font
  var pos: seq[float]
  case axKind
  of akX:
    pos = ticks.mapIt(it.tkPos.x.pos)
  of akY:
    pos = ticks.mapIt(it.tkPos.y.pos)

  # tick scale (= tick difference) is used to determine if a value is supposed to
  # be exactly 0
  let tickScale = (pos.max - pos.min) / (pos.len - 1).float
  let fmt =
    if format != nil: format
    else: (proc(x: float): string = formatTickValue(x, tickScale))
  # determine pretty if we have to modify values
  let strs = pos.mapIt(fmt(it))
  let strslen = strs.len
  var newPos: seq[float]
  if format == nil: 
    let strsunique = strs.deduplicate.len
    if strsunique < strslen:
      # normal stringification loses information, fix
      let min = pos.min
      newPos = pos.mapIt(it - min)
      # based on this, add an additional text in top left
      let maxtick = ticks[^1]
      var coord: Coord
      var rotate: Option[float]
      case axKind
      of akX:
        coord = axisCoord(maxTick.tkPos.x, akX, isSecondary)
        coord.y = Coord1D(pos: coord.y.toPoints(length = some(view.hImg)).pos + quant(1.5, ukCentimeter).toPoints.val,
                          kind: ukPoint)
        rotate = none[float]()
      of akY:
        coord = axisCoord(maxTick.tkPos.y, akY, isSecondary)
        coord.x = Coord1D(pos: coord.x.toPoints(length = some(view.wImg)).pos - quant(2.0, ukCentimeter).toPoints.val,
                          kind: ukPoint)
        rotate = some(-90.0)
      # text that describes what was subtracted
      result.add view.initText(coord,
                              &"+{fmt(min)}",
                              textKind = goText,
                              alignKind = taRight,
                              font = font,
                              rotate = rotate,
                              name = "axisSubtraction")
  for i in 0 ..< ticks.len:
    let labelTxt =
      if newPos.len > 0: fmt(newPos[i])
      else: strs[i]
    result.add view.initTickLabel(tick = ticks[i], font = font,
                                  labelTxt = labelTxt,
                                  margin = margin,
                                  isSecondary = isSecondary)

proc initTick(view: Viewport,
              axKind: AxisKind,
              major: bool,
              at: Coord,
              tickKind: TickKind = tkOneSide,
              style: Option[Style] = none[Style](),
              name = "tick",
              isSecondary = false): GraphObject =
  result = GraphObject(kind: goTick,
                       name: name,
                       tkPos: at.patchCoord(view),
                       tkMajor: major,
                       tkAxis: axKind,
                       tkKind: tickKind,
                       tkSecondary: isSecondary)
  if style.isSome:
    result.style = style
  else:
    result.style = some(Style(lineWidth: 1.0, # width of tick
                              color: color(0.0, 0.0, 0.0),
                              size: 5.0, # total length of tick
                              lineType: ltSolid))

proc tickLabels*(view: Viewport,
                 tickPos: seq[Coord1D],
                 tickLabels: seq[string],
                 axKind: AxisKind,
                 font: Option[Font] = none[Font](),
                 isSecondary = false,
                 rotate = none[float](),
                 margin = none[Coord1D](),
                 alignToOverride = none[TextAlignKind]()
                ): (seq[GraphObject], seq[GraphObject]) =
  ## Overload of `tickLabels`, which allows to define custom tick label
  ## texts for ticks at positions `tickPos`
  doAssert tickPos.len == tickLabels.len, "must have as many tick positions " &
    "as label texts!"
  let mfont = if font.isNone: some(defaultFont(8.0)) else: font
  result[0] = newSeq[GraphObject](tickPos.len)
  result[1] = newSeq[GraphObject](tickPos.len)
  for i in 0 ..< tickPos.len:
    let tick = view.initTick(axKind = axKind,
                             at = axisCoord(tickPos[i], axKind, isSecondary),
                             major = true,
                             isSecondary = isSecondary)
    result[0][i] = tick
    result[1][i] = view.initTickLabel(tick = tick, font = mfont,
                                      labelTxt = tickLabels[i],
                                      isSecondary = isSecondary,
                                      rotate = rotate,
                                      margin = margin,
                                      alignToOverride = alignToOverride)

# taken straight from: *cough*
# https://stackoverflow.com/questions/4947682/intelligently-calculating-chart-tick-positions
proc niceNumber(val: float, round: bool): float =
  var niceFrac: float
  let exponent = val.log10.floor.int
  let frac = val / pow(10, exponent.float)

  if round:
    if frac < 1.5:
      niceFrac = 1.0
    elif frac < 3.0:
      niceFrac = 2.0
    elif frac < 7.0:
      niceFrac = 5.0
    else:
      niceFrac = 10.0
  else:
    if frac <= 1.0:
      niceFrac = 1.0
    elif frac <= 2.0:
      niceFrac = 2.0
    elif frac <= 5.0:
      niceFrac = 5.0
    else:
      niceFrac = 10.0

  result = niceFrac * pow(10, exponent.float)

proc calcTickLocations*(scale: Scale, numTicks: int): (Scale, float, int) =
  # TODO: extend for log scale
  # Check for special cases
  if scale.low == scale.high:
    raise newException(ValueError, "A data scale is required to calculate " &
      "tick positions!")
  let
    axEnd = scale.high
    axStart = scale.low

  let axWidth = axEnd - axStart;

  # Compute the new nice range and ticks
  let niceRange = niceNumber(axEnd - axStart, false)
  let niceTick = niceNumber(niceRange / (numTicks - 1).float, true)

  # Compute the new nice start and end values
  let newaxStart = floor(axStart / niceTick) * niceTick
  let newaxEnd = ceil(axEnd / niceTick) * niceTick
  let newNumTicks = ((newAxEnd - newAxStart) / niceTick).round.int
  result = ((low: newAxStart, high: newAxEnd),
            niceTick,
            ((newAxEnd - newAxStart) / niceTick).round.int)

proc filterByBoundScale(tickPos: var seq[Coord], axKind: AxisKind,
                        boundScale: Option[Scale]) =
  ## performs the filtering of the tick positions given an optional
  ## `boundScale`. Only those tick positions inside the bound scale
  ## will remain
  proc getAx(c: Coord, axKind: AxisKind): float =
    case axKind
    of akX: c.x.pos
    of akY: c.y.pos
  if boundScale.isSome:
    let bscale = boundScale.unsafeGet
    tickPos = tickPos.filterIt(getAx(it, axKind) >= bscale.low and
                               getAx(it, axKind) <= bscale.high)

proc initTicks*(view: Viewport,
                axKind: AxisKind,
                numTicks: int = 0,
                tickLocs: seq[Coord] = @[],
                tickKind: TickKind = tkOneSide,
                major = true,
                style: Option[Style] = none[Style](),
                updateScale = true,
                isSecondary = false,
                boundScale = none[Scale]()): seq[GraphObject] =
  ## Initializes the tick positions for the given `axKind` for either
  ## `major` or `minor` (`major == false`) ticks.
  ## If `updateScale` is true will recursively update all data scales
  ## associated to the viewports children and objects. Set `updateScale`
  ## to `false` only if you're certain that the update is unnecessary!
  ## `boundScale` can be used to bound the possible range of the range
  ## in which any ticks will be created.
  ## The algorithm will run as it does without, but only those ticks inside
  ## of the bounds will actually be added.
  ## This however does ``not`` affect the result if called with `tickLocs`! In
  ## that case the caller is responsible for selecting the ticks. Also the
  ## updated data scale of the viewport may lie outside the `boundScale`!
  # check whether there
  if numTicks == 0 and tickLocs.len == 0:
    raise newException(ValueError, "Either need a number of ticks or tick " &
      "locations if auto tick locations not used!")
  if numTicks == 0 and tickLocs.len > 0:
    for loc in tickLocs:
      result.add initTick(view, axKind = axKind, major = major, at = loc,
                          tickKind = tickKind, style = style,
                          isSecondary = isSecondary)
  elif numTicks > 0:
    var scale: Scale
    if axKind == akX:
      scale = view.xScale
    else:
      scale = view.yScale

    let (newScale, newWidth, newNumTicks) = calcTickLocations(scale, numTicks)
    let tickScale = if boundScale.isSome: boundScale.unsafeGet
                    else: newScale
    var autoTickLocs: seq[Coord]
    case axKind
    of akX:
      autoTickLocs = linspace(newScale.low, newScale.high, newNumTicks + 1).mapIt(
        axisCoord(Coord1D(pos: it,
                          kind: ukData,
                          scale: tickScale,
                          axis: akX),
                  akX,
                  isSecondary)
      )
    of akY:
      autoTickLocs = linspace(newScale.low, newScale.high, newNumTicks + 1).mapIt(
        axisCoord(Coord1D(pos: it,
                          kind: ukData,
                          scale: tickScale,
                          axis: akY),
                  akY,
                  isSecondary)
      )
    # if there is a `boundScale` filter by it
    autoTickLocs.filterByBoundScale(axKind, boundScale)
    result = view.initTicks(axKind, tickLocs = autoTickLocs,
                            tickKind = tickKind,
                            major = major, style = style,
                            isSecondary = isSecondary)
    # finally update the scale associated to the view
    case axKind
    of akX:
      view.xScale = tickScale
    of akY:
      view.yScale = tickScale

    # and update the scales of all objects owned by the viewport
    if updateScale:
      view.updateDataScale()

proc xticks*(view: Viewport,
             numTicks: int = 10,
             tickLocs: seq[Coord] = @[],
             major = true,
             tickKind: TickKind = tkOneSide,
             style: Option[Style] = none[Style](),
             updateScale = true,
             isSecondary = false
            ): seq[GraphObject] =
  ## generates the ticks for the x axis. Note that this updates the data
  ## scale of all children and objects of the given viewport. In order
  ## for this to as inexpensive as possible make sure to call this
  ## rather before all objects have been added.
  result = view.initTicks(akX,
                          numTicks = numTicks,
                          tickLocs = tickLocs,
                          tickKind = tickKind,
                          major = true,
                          style = style,
                          updateScale = updateScale,
                          isSecondary = isSecondary)

proc yticks*(view: Viewport,
             numTicks: int = 10,
             tickLocs: seq[Coord] = @[],
             major = true,
             tickKind: TickKind = tkOneSide,
             style: Option[Style] = none[Style](),
             updateScale = true,
             isSecondary = false): seq[GraphObject] =
  ## generates the ticks for the y axis. Note that this updates the data
  ## scale of all children and objects of the given viewport. In order
  ## for this to as inexpensive as possible make sure to call this
  ## rather before all objects have been added.
  result = view.initTicks(akY,
                          numTicks = numTicks,
                          tickLocs = tickLocs,
                          tickKind = tickKind,
                          major = true,
                          style = style,
                          updateScale = updateScale,
                          isSecondary = isSecondary)

func calcMinorTicks(ticks: seq[GraphObject], axKind: AxisKind): seq[Coord1D] =
  ## calculates the position in the middle between each tick and
  ## the successive tick
  doAssert ticks[0].kind == goTick, "Elements for grid lines must be `goTick`!"
  result = newSeq[Coord1D](ticks.len - 1)
  var scale: Scale
  case axKind
  of akX:
    scale = ticks[0].tkPos.x.scale
  of akY:
    scale = ticks[0].tkPos.y.scale
  let cdiv2 = Coord1D(pos: 2.0, kind: ukData, scale: scale, axis: axKind)
  for i in 0 ..< ticks.high: # ignore last tick
    doAssert ticks[i].kind == goTick, "Elements for grid lines must be `goTick`!"
    # calculate position between both ticks
    case axKind
    of akX:
      let midPos = (ticks[i].tkPos.x + ticks[i+1].tkPos.x) / cdiv2
      result[i] = midPos
    of akY:
      let midPos = (ticks[i].tkPos.y + ticks[i+1].tkPos.y) / cdiv2
      result[i] = midPos

proc initGridLines*(view: Viewport,
                    xticks: Option[seq[GraphObject]] = none[seq[GraphObject]](),
                    yticks: Option[seq[GraphObject]] = none[seq[GraphObject]](),
                    major = true,
                    style: Option[Style] = none[Style](),
                    name = "GridLines"): GraphObject =
  doAssert xticks.isSome or yticks.isSome, "At least one of xticks, yticks " &
    "required for grid lines!"
  # TODO: could use `calcTickLocations` to calculate based on Viewport!?
  result = GraphObject(kind: goGrid)
  if style.isSome:
    result.style = style
  else:
    var lineWidth = 1.0
    if not major:
      lineWidth = lineWidth / 3.0
    result.style = some(Style(lineWidth: lineWidth,
                              color: white,
                              lineType: ltSolid))
  var gobjName = name
  if major:
    # take tick positions directly
    if xticks.isSome:
      result.gdXPos = xticks.unsafeGet.mapIt(it.tkPos.x)
    if yticks.isSome:
      result.gdYPos = yticks.unsafeGet.mapIt(it.tkPos.y)
    if gobjName == "GridLines":
      gobjName = "x" & gobjName
  else:
    # if log scale, no minor
    #if not logScale:
    # if normal scale, minor lines at (x_i + x_{i+1})/2
    if xticks.isSome:
      let ticks = xticks.get()
      result.gdXPos = calcMinorTicks(ticks, akX)
    if yticks.isSome:
      let ticks = yticks.get()
      result.gdYPos = calcMinorTicks(ticks, akY)
    if gobjName == "GridLines":
      gobjName = "y" & gobjName
  # set the name
  result.name = gobjName

proc fillEmptySizesEvenly(s: seq[Quantity],
                          length: Quantity,
                          num: int,
                          scale: Scale,
                          ignoreOverflow = false,
                         ): seq[Quantity] =
  ## filters out the 0 sized Coord1D from `s` and replaces them with
  ## evenly sized sizes filling up to a total of 1.0
  ## Length is the width / height of the viewport in which the layout
  ## will be set
  let zeroNum = s.filterIt(it.toRelative(length = some(length),
                                         scale = some(scale)).val == 0.0).len
  if zeroNum == 0:
    result = s
  else:
    let sumWidths = s.mapIt(it.toRelative(length = some(length),
                                          scale = some(scale)).val).foldl(a + b)
    let remainWidth = (1.0 - sumWidths) / zeroNum.float
    if not ignoreOverflow and remainWidth < 0:
      raise newException(ValueError, "Given layout sizes exceed the viewport " &
        "size. Remaining sizes cannot be filled! Total size: " & $sumWidths &
        " Remaining rows/cols: " & $zeroNum)
    for i in 0 ..< num:
      if s[i].toRelative(length = some(length),
                         scale = some(scale)).val == 0:
        result.add quant(remainWidth, ukRelative)
      else:
        result.add s[i]

proc layout*(view: Viewport,
             cols, rows: int,
             colWidths: seq[Quantity] = @[],
             rowHeights: seq[Quantity] = @[],
             margin: Quantity = quant(0.0, ukRelative),
             ignoreOverflow = false) =
  ## creates a layout of viewports within the given `view` of
  ## `cols` columns and `rows` rows. Optionally the widths and
  ## heights of the cols / rows may be set. If none are given,
  ## the widths / heights will be evenly sized.
  ## Any width or height that has a relative size of 0.0, will be
  ## considered as unspecified. In this case we split the remaining
  ## space after the other sizes are summed between those.
  ## If a `margin` is given, each viewport created will be surrounded
  ## by that margin in all directions.
  ## If `ignoreOverflow` is true a layout which technically does not fit
  ## into the existing `view` will still be allowed. Otherwise (default)
  ## a `ValueError` is raised.
  # extend the seq of children to accomodate for layout
  const nameTmpl = "$#/layout_$#"
  doAssert colWidths.len == cols or colWidths.len == 0, "there must be " &
    "one column width for each column!"
  doAssert rowHeights.len == rows or rowHeights.len == 0, "there must be " &
    "one row height for each row!"
  var widths: seq[Quantity]
  var heights: seq[Quantity]
  if colWidths.len == 0:
    widths = newSeqWith(cols, quant(1.0 / cols.float, ukRelative))
  else:
    widths = fillEmptySizesEvenly(colWidths, pointWidth(view), cols, scale = view.xScale,
                                  ignoreOverflow = ignoreOverflow)
  if rowHeights.len == 0:
    heights = newSeqWith(rows, quant(1.0 / rows.float, ukRelative))
  else:
    heights = fillEmptySizesEvenly(rowHeights, pointHeight(view), rows, scale = view.yScale,
                                   ignoreOverflow = ignoreOverflow)

  var curRowT = c1(0.0)
  for i in 0 ..< rows:
    #doAssert heights[i].unit == ukRelative, "height must be relative!"
    var curColL = c1(0.0)
    for j in 0 ..< cols:
      #doAssert widths[j].unit == ukRelative, "width must be relative!"
      # use widths / heights to create new viewports
      let marginX = margin.toRelative(length = some(pointWidth(view)),
                                      scale = some(view.xScale))
      let marginY = margin.toRelative(length = some(pointHeight(view)),
                                      scale = some(view.yScale))
      let width = sub(widths[j],
                      times(quant(2.0, ukRelative), marginX,
                            length = some(pointWidth(view)),
                            scale = some(view.xScale)),
                      length = some(pointWidth(view)),
                      scale = some(view.xScale))
      let height = sub(heights[i], times(quant(2.0, ukRelative), marginY,
                                         length = some(pointHeight(view)),
                                         scale = some(view.yScale)),
                       length = some(pointHeight(view)),
                       scale = some(view.yScale))
      ## TODO: fix margin. Currently cannot work, since we do not apply the margin
      ## in the `curColL` and `curRowT` vars after each iteration!
      let ch = view.addViewport(
        origin = Coord(x: curColL, #c1(curColL + marginX.val),
                       y: curRowT),#c1(curRowT + marginY.val)),
        width = width,
        height = height,
        xScale = some(view.xScale),
        yScale = some(view.yScale),
        style = some(view.style), # inherit style of parent,
        name = nameTmpl % [view.name, $(i * cols + j)]
      )
      view.children.add ch
      curColL = curColL + view.c1(widths[j].val, akX, widths[j].unit)
    curRowT = curRowT + view.c1(heights[i].val, akY, heights[i].unit)
  ## TODO: update width and height of the parent viewport

proc background*(view: Viewport,
                 style: Option[Style] = none[Style]()) =
  var r = GraphObject(kind: goRect,
                      reOrigin: initCoord(0.0, 0.0),
                      reWidth: quant(1.0, ukRelative),
                      reHeight: quant(1.0, ukRelative))
  if style.isSome:
    r.style = style
  else:
    r.style = some(Style(color: color(0.0, 0.0, 0.0, 0.0),
                         fillColor: grey92))
  view.addObj r

################################################################################
########## DRAWING FUNCTIONS
################################################################################





proc drawLine(img: var BImage, gobj: GraphObject) =
  doAssert gobj.kind == goAxis or gobj.kind == goLine, "object must be a `goAxis` or `goLine`!"
  img.drawLine(gobj.lnStart.point, gobj.lnStop.point,
               gobj.style.get, # if we end up here without a style,
                               # it's a bug!
               rotateAngle = gobj.rotateInView)

proc drawRect(img: var BImage, gobj: GraphObject) =
  doAssert gobj.kind == goRect, "object must be a `goRect`!"
  img.drawRectangle(gobj.reOrigin.point.x, gobj.reOrigin.point.y,
                    # TODO: make sure we HAVE already converted to points!
                    gobj.reWidth.val, gobj.reHeight.val,
                    gobj.style.get, # if we end up here without a style,
                                    # it's a bug!
                    rotate = gobj.rotate,
                    rotateInView = gobj.rotateInView)

proc drawPoint(img: var BImage, gobj: GraphObject) =
  doAssert gobj.kind == goPoint, "object must be a `goPoint`!"
  case gobj.ptMarker
  of mkCircle:
    img.drawCircle(gobj.ptPos.point, gobj.ptSize, lineWidth = 0.0,
                   strokeColor = color(0.0, 0.0, 0.0, 0.0),
                   fillColor = gobj.ptColor,
                   rotateAngle = gobj.rotateInView)
  of mkCross:
    var style = gobj.style.get() # style *has* to exist
    # modify line width to accomodate drawing a cross
    style.lineWidth = gobj.ptSize / 4.0
    style.color = gobj.ptColor
    style.lineType = ltSolid
    style.fillColor = gobj.ptColor
    let
      posX = gobj.ptPos.point.x
      posY = gobj.ptPos.point.y
    img.drawLine((posX - gobj.ptSize / 2.0, posY),
                 (posX + gobj.ptSize / 2.0, posY),
                 style,
                 rotateAngle = gobj.rotateInView)
    img.drawLine((posX, posY - gobj.ptSize / 2.0),
                 (posX, posY + gobj.ptSize / 2.0),
                 style,
                 rotateAngle = gobj.rotateInView)
  else:
    raise newException(Exception, "Not implemented yet!")

proc drawPolyLine(img: var BImage, gobj: GraphObject) =
  doAssert gobj.kind == goPolyLine, "object must be a `goPolyLine`!"
  # TODO: we assume that the coordinates are sorted for now
  img.drawPolyLine(gobj.plPos.mapIt((x: it.x.pos, y: it.y.pos)),
                   gobj.style.get, # there *has* to be a style here
                   rotateAngle = gobj.rotateInView)


proc drawText(img: var BImage, gobj: GraphObject) =
  doAssert(
    (gobj.kind == goText or gobj.kind == goLabel or gobj.kind == goTickLabel),
    "object must be a `goText`, `goLabel` or `goTickLabel`!"
  )
  img.drawText(gobj.txtText, gobj.txtFont, gobj.txtPos.point, gobj.txtAlign,
               gobj.rotate)

proc drawTick(img: var BImage, gobj: GraphObject) =
  ## draw a tick
  doAssert gobj.kind == goTick, "object must be a `goTick`!"
  var style = gobj.style.get() # style *has* to exist
  var length = style.size
  if not gobj.tkMajor:
    # minor ticks use half the width of normal ticks
    style.lineWidth = style.lineWidth / 2.0
    length = length / 2.0

  if gobj.tkSecondary:
    # if secondary, invert the direction of length
    length = -length

  var
    tkStart: float
    tkStop: float
  case gobj.tkAxis
  of akX:
    case gobj.tkKind
    of tkOneSide:
      tkStart = gobj.tkPos.point.y + length
      tkStop = gobj.tkPos.point.y
    of tkBothSides:
      tkStart = gobj.tkPos.point.y + length
      tkStop = gobj.tkPos.point.y - length
    let tkX = gobj.tkPos.point.x
    img.drawLine((tkX, tkStart),
                 (tkX, tkStop),
                 style,
                 rotateAngle = gobj.rotateInView)
  of akY:
    case gobj.tkKind
    of tkOneSide:
      tkStart = gobj.tkPos.point.x
      tkStop = gobj.tkPos.point.x - length
    of tkBothSides:
      tkStart = gobj.tkPos.point.x + length
      tkStop = gobj.tkPos.point.x - length
    let tkY = gobj.tkPos.point.y
    img.drawLine((tkStart, tkY),
                 (tkStop, tkY),
                 style,
                 rotateAngle = gobj.rotateInView)

proc drawGrid(img: var BImage, gobj: GraphObject) =
  ## draws the (major / minor) grid
  doAssert gobj.kind == goGrid, "object must be a `goGrid`!"
  var style = gobj.style.get() # style *has* to exist
  # start with vertical lines
  for x in gobj.gdXPos:
    let
      start = (x.pos, gobj.gdOrigin.y.pos)
      stop = (x.pos, gobj.gdOriginDiag.y.pos)
    img.drawLine(start, stop, style, rotateAngle = gobj.rotateInView)
  for y in gobj.gdYPos:
    let
      start = (gobj.gdOrigin.x.pos, y.pos)
      stop = (gobj.gdOriginDiag.x.pos, y.pos)
    img.drawLine(start, stop, style, rotateAngle = gobj.rotateInView)

proc scale[T: SomeNumber](p: Point, width, height: T): Point =
  result = (p.x * width.float,
            p.y * height.float)

proc toAbsImage(c: Coord1D, img: BImage, axKind: AxisKind): Coord1D {.inline.} =
  case axKind
  of akX: result = c.to(ukPoint, absLength = some(quant(img.width.float, ukPoint)))
  of akY: result = c.to(ukPoint, absLength = some(quant(img.height.float, ukPoint)))

proc toAbsImage(c: Coord, img: BImage): Coord {.inline.} =
  result.x = c.x.toAbsImage(img, akX)
  result.y = c.y.toAbsImage(img, akY)

proc toGlobalCoords(gobj: GraphObject, img: BImage): GraphObject =
  result = gobj
  case gobj.kind
  of goLine, goAxis:
    result.lnStart = result.lnStart.toAbsImage(img)
    result.lnStop = result.lnStop.toAbsImage(img)
  of goRect:
    result.reOrigin = result.reOrigin.toAbsImage(img)
    result.reWidth = result.reWidth.toPoints(some(
      quant(img.width.float, ukPoint))
    )
    result.reHeight = result.reHeight.toPoints(some(
      quant(img.height.float, ukPoint))
    )
  of goPoint:
    result.ptPos = gobj.ptPos.toAbsImage(img)
  of goPolyLine:
    result.plPos = gobj.plPos.mapIt(it.toAbsImage(img))
  of goText, goLabel, goTickLabel:
    result.txtPos = gobj.txtPos.toAbsImage(img)
  of goTick:
    result.tkPos = gobj.tkPos.toAbsImage(img)
  of goGrid:
    result.gdOrigin = gobj.gdOrigin.toAbsImage(img)
    result.gdOriginDiag = gobj.gdOriginDiag.toAbsImage(img)
    let
      qImgWidth = some(quant(img.width.float, ukPoint))
      qImgHeight = some(quant(img.height.float, ukPoint))
    result.gdXPos = gobj.gdXPos.mapIt(it.to(ukPoint, absLength = qImgWidth))
    result.gdYPos = gobj.gdYPos.mapIt(it.to(ukPoint, absLength = qImgHeight))
  of goComposite:
    # composite has nothing to be drawn, only children, which are handled individually
    discard

iterator items*(view: Viewport): Viewport =
  for ch in view.children:
    yield ch

iterator mitems*(view: var Viewport): var Viewport =
  for ch in mitems(view.children):
    yield ch

proc embedInto(gobj: GraphObject, view: Viewport): GraphObject =
  result = gobj
  case gobj.kind
  of goLine, goAxis:
    result.lnStart = result.lnStart.embedInto(view)
    result.lnStop = result.lnStop.embedInto(view)
  of goRect:
    result.reOrigin = gobj.reOrigin.embedInto(view)
    result.reWidth = result.reWidth.embedInto(akX, view)
    result.reHeight = result.reHeight.embedInto(akY, view)
  of goPoint:
    result.ptPos = gobj.ptPos.embedInto(view)
  of goPolyLine:
    result.plPos = gobj.plPos.mapIt(it.embedInto(view))
  of goLabel, goText, goTickLabel:
    result.txtPos = gobj.txtPos.embedInto(view)
  of goTick:
    result.tkPos = gobj.tkPos.embedInto(view)
  of goGrid:
    # assign and convert origin and diagonal origin
    result.gdOrigin = view.origin
    result.gdOriginDiag = Coord(x: view.origin.x + c1(width(view).val),
                                y: view.origin.y + c1(height(view).val))
    result.gdXPos = gobj.gdXPos.mapIt(view.origin.x + it * c1(width(view).val))
    result.gdYPos = gobj.gdYPos.mapIt(view.origin.y + it * c1(height(view).val))
  of goComposite:
    # composite itself contains nothing to draw, only children.
    # children will be drawn and embedded individually
    discard
  #else:
  #  raise newException(Exception, "embedInto not implemented yet for " &
  #    $gobj.kind & "!")

proc getCenter*(view: Viewport): (float, float) =
  ## returns the center position of the given viewport in relative coordinates
  ## NOTE: it is *not* (0.5, 0.5), because the coordinates of the viewport are
  ## described in the coordinate system of the parent!
  let
    centerX = left(view).pos +
      width(view).toRelative(length = some(pointWidth(view))).val / 2.0
    centerY = bottom(view).pos +
      height(view).toRelative(length = some(pointHeight(view))).val / 2.0
  result = (centerX, centerY)

proc parseFilename(fname: string): FiletypeKind =
  let (_, _, ext) = fname.splitFile
  case ext.normalize
  of ".pdf":
    result = fkPdf
  of ".svg":
    result = fkSvg
  of ".png":
    result = fkPng
  else:
    result = fkPdf

proc draw*(img: var BImage, gobj: GraphObject) =
  ## draws the given graph object on the image
  let globalObj = gobj.toGlobalCoords(img)

  case gobj.kind
  of goLine, goAxis:
    img.drawLine(globalObj)
  of goRect:
    img.drawRect(globalObj)
  of goPoint:
    img.drawPoint(globalObj)
  of goPolyLine:
    img.drawPolyLine(globalObj)
  of goLabel, goText, goTickLabel:
    img.drawText(globalObj)
  of goTick:
    img.drawTick(globalObj)
  of goGrid:
    img.drawGrid(globalObj)
  of goComposite:
    # composite itself has nothing to be drawn, only children handled individually
    discard

proc draw(img: var BImage, view: Viewport) =
  ## draws the full viewport including all objects and all
  ## children onto the image
  ## NOTE: children are drawn `after` the parent viewport
  # draw objects
  # before we draw stuff apply potential rotation
  let (centerX, centerY) = getCenter(view)

  proc transformAndDraw(img: var BImage, obj: GraphObject, view: Viewport) =
    ## performs the embedding of the object into the viewport
    ## and draws the resulting object
    var mobj = obj
    # first check if object shall be rotate individually
    if view.rotate.isSome:
      mobj.rotateInView = some((view.rotate.get,
                                 (centerX, centerY).scale(img.width, img.height)))
    img.draw(mobj.embedInto(view))

  for obj in view.objects:
    # transform the object to draw to the global image coordinate system
    # and draw
    img.transformAndDraw(obj, view)
    # draw all children of the object
    for ch in obj.children:
      doAssert obj.kind == goComposite, "Object should be a composite if it " &
        "has children!"
      img.transformAndDraw(ch, view)

  # draw all children viewports
  for chView in view:
    var mchView = chView.embedInto(view)
    # TODO: we have to be careful here, because depending on how the child viewport
    # was created, we may have already set the `wImg`, `hImg` fields in a scaled way!
    #mchView.wImg = quant(view.wImg.val * mchView.width.toRelative(view.wImg).val, ukPoint)
    #mchView.hImg = quant(view.hImg.val * mchView.height.toRelative(view.wImg).val, ukPoint)
    doAssert mchView.wImg == view.wImg
    doAssert mchView.hImg == view.hImg
    if view.rotate.isSome and mchView.rotate.isNone:
      # only propagate if rotation of parent is set but child has no
      # rotation itself
      mchView.rotate = view.rotate
    img.draw(mchView)

when not defined(noCairo):
  proc draw*(view: Viewport, filename: string) =
    ## draws the given viewport and all its children and stores it in the
    ## file `filename`
    let ftype = parseFilename(filename)
    var img = initBImage(filename,
                         width = view.wImg.val.round.int, height = view.hImg.val.round.int,
                         backend = view.backend,
                         ftype = ftype)
    img.draw(view)
    img.destroy()
else:
  proc draw*(view: Viewport, filename: string) =
    static: echo "Compiling draw as a dummy proc"
    echo "WARNING: Compiled with `-d:noCairo`. `draw` does not do anything " &
      "in this compilation mode!"

when isMainModule:

  import seqmath, sequtils
  block:
    #var img = initBImage("testView.pdf",
    #                     width = 600, height = 400,
    #                     backend = bkCairo,
    #                     ftype = fkPdf)
    # create default viewport (== image) of 640x480
    var img = initViewport()

    var view1 = img.addViewport(left = 0.1,
                                bottom = 0.1,
                                width = 0.8,
                                height = 0.8,
                                xScale = some((low: 0.0, high: 2.0 * PI)),
                                yScale = some((low: -1.0, high: 1.0)))
    var view2 = initViewport(left = 0.25,
                             bottom = 0.5,
                             width = 0.75,
                             height = 0.5,
                             xScale = some((low: 0.0, high: 2.0 * PI)),
                             yScale = some((low: -1.0, high: 1.0)))
    let line1 = view1.initAxis(akX)
    let line2 = view1.initAxis(akY)
    let x = linspace(0.0, 6.28, 1_0)
    let y = x.mapIt(sin(it))
    let points = zip(x, y)
    var gobjPoints: seq[GraphObject]
    var gobjErrors: seq[GraphObject]
    for p in points:
      gobjPoints.add initPoint(view2, (x: p.a, y: p.b),
                               marker = mkCross)
      gobjErrors.add initErrorBar(view2, gobjPoints[^1],
                                  errorUp = view2.c1(0.5, akX, ukCentimeter), #initCoord1D(p.a * 0.05, kind = ukData),
                                  errorDown = view2.c1(0.5, akX, ukCentimeter),#initCoord1D(p.a * 0.05, kind = ukData),
                                  axKind = akX,
                                  ebKind = ebLinesT)
      gobjErrors.add initErrorBar(view2, gobjPoints[^1],
                                  errorUp = view2.c1(0.25, akX, ukCentimeter), #initCoord1D(p.a * 0.05, kind = ukData),
                                  errorDown = view2.c1(0.25, akX, ukCentimeter),#initCoord1D(p.a * 0.05, kind = ukData),
                                  axKind = akY,
                                  ebKind = ebLinesT)

    let ptsLine = view1.initPolyLine(pos = points.mapIt((x: it.a, y: it.b)))

    let
      xticks = view1.xticks()
      yticks = view1.yticks()
      xtickLabels = view1.tickLabels(xticks)
      ytickLabels = view1.tickLabels(yticks)

    let rect = view1.initRect(0.3, 0.3, 0.1, 0.2)
    let rect2 = view2.initRect(
      0.0, 0.0, 1.0, 1.0,
      style = some(
        Style(lineWidth: 2.0,
              color: color(1.0, 0.0, 0.0),
              fillColor: color(0.0, 0.0, 0.0, 0.5))
      )
    )

    let cmSquare = view1.initRect(initCoord(0.1, 0.1),
                                  width = quant(1.0, ukCentimeter),
                                  height = quant(1.0, ukCentimeter))

    let inchSquare = view1.initRect(initCoord(0.3, 0.3),
                                  width = quant(1.0, ukInch),
                                  height = quant(1.0, ukInch))

    let xlabel = view1.xlabel("Energy")
    let ylabel = view1.ylabel("Count")

    view1.background()

    let grdlines = view1.initGridLines(some(xticks), some(yticks))
    let grdLnMinor = view1.initGridLines(some(xticks), some(yticks), major = false)

    view1.addObj concat(xticks, yticks, xticklabels, yticklabels, @[line1, line2, xlabel, ylabel, cmSquare, grdLines, grdLnMinor, ptsLine, inchSquare])#, gobjPoints)
    view2.addObj concat(@[rect2], gobjErrors, gobjPoints)
    view1.children.add view2
    img.children.add view1
    img.draw("testView.pdf")

  block:
    var img = initBImage("testEmbed.svg",
                         width = 600, height = 400,
                         backend = bkCairo,
                         ftype = fkSvg)

    var view1 = initViewport(left = 0.0,
                             bottom = 0.0,
                             width = 1.0,
                             height = 1.0)
    var view2 = initViewport(left = 0.5,
                             bottom = 0.5,
                             width = 0.5,
                             height = 0.5,
                             xScale = some((low: 0.0, high: 1.0)),
                             yScale = some((low: 0.0, high: 1.0)))
    let rect = view2.initRect(0.0, 0.0, 1.0, 0.5,
                              style = some(Style(lineWidth: 2.0,
                                                 color: color(1.0, 0.0, 0.0),
                                                 lineType: ltDashed,
                                                 fillColor: color(0.0, 0.0, 0.0, 0.5))))


    let text = view2.initText(initCoord(0.5, 1.05),
                              "Hello",
                              textKind = goText,
                              alignKind = taCenter,
                              font = defaultFont())


    let textRot = view2.initText(initCoord(-0.05, 0.5),
                                 "Counts",
                                 textKind = goText,
                                 alignKind = taCenter,
                                 font = some(defaultFont()),
                                 rotate = some(90.0))
    var textRot2 = textRot
    textRot2.txtAlign = taLeft
    textRot2.rotate = some(70.0)
    textRot2.txtFont.color = color(0.0, 1.0, 0.0)

    var textRot3 = textRot
    textRot3.txtAlign = taRight
    textRot3.rotate = some(70.0)
    textRot3.txtFont.color = color(1.0, 0.0, 0.0)
    var textRot3a = textRot
    textRot3a.txtAlign = taRight
    textRot3a.rotate = some(90.0)
    textRot3a.txtFont.color = color(1.0, 1.0, 0.0)

    var textRot4 = textRot
    textRot4.txtAlign = taLeft
    textRot4.rotate = some(90.0)
    textRot4.txtFont.color = color(0.0, 0.0, 1.0)

    var textRot5 = textRot
    textRot5.txtAlign = taCenter
    textRot5.rotate = some(70.0)
    textRot5.txtFont.color = color(0.0, 1.0, 1.0)
    #view1.rotate = some(30.0)
    view2.addObj @[rect,
                   initPoint(view2, (x: 0.0, y: 0.0)),
                   initPoint(view2, (x: 1.0, y: 0.0)),
                   initPoint(view2, (x: 0.0, y: 1.0)),
                   initPoint(view2, (x: 1.0, y: 1.0)),
                   text, textRot, textRot2, textRot3, textRot4, textRot5, textRot3a]
    view1.children.add view2
    img.draw(view1)
    img.destroy()

  block:
    var img = initViewport(wImg = 800,
                           hImg = 100)
    var axisVp = img.addViewport(left = 0.1,
                                 bottom = 0.1,
                                 width = 0.8,
                                 height = 0.3)
    let line1 = axisVp.initAxis(akX)
    let line2 = axisVp.initAxis(akY)

    let xlabel = axisVp.xlabel("Energy")
    let ylabel = axisVp.ylabel("Count")
    let yx0 = img.initAxis(akY)

    img.objects = @[yx0]
    axisVp.addObj @[line1, line2, xlabel, ylabel]
    img.children.add axisVp
    img.draw("axisCheck.pdf")

  block:
    var img = initViewport()
    var axisVp = img.addViewport(left = 0.1,
                                 bottom = 0.1,
                                 width = 0.8,
                                 height = 0.8)
    var rects: seq[GraphObject]
    const num = 4
    let cols = ggColorHue(num + 1)
    for i in 0 .. num:
      let style = Style(color: cols[i],
                        fillColor: cols[i])
      rects.add axisVp.initRect(initCoord(i.float * 0.1, i.float * 0.1),
                                width = quant(1.0, ukCentimeter),
                                height = quant(1.0, ukCentimeter),
                                color = cols[i],
                                style = some(style))
    axisVp.addObj rects
    img.children.add axisVp
    img.draw("testGGCols.pdf")

  block:
    var img = initViewport()

    let widths = @[quant(1.0, ukCentimeter),
                   quant(0.0, ukRelative),
                   quant(0.0, ukRelative),
                   quant(1.0, ukCentimeter)]
    let heights = @[quant(1.0,ukCentimeter),
                   quant(0.0, ukRelative),
                   quant(0.0, ukRelative),
                   quant(1.0, ukCentimeter)]

    let rect = img.initRect(c(0.0, 0.0),
                            width = quant(1.0, ukCentimeter),
                            height = quant(1.0, ukCentimeter),
                            color = black)

    img.layout(4, 4, colwidths = widths, rowHeights = heights)
    for ch in mitems(img):
      let st = Style(color: color(0.0, 0.0, 1.0, 1.0),
                     lineType: ltSolid,
                     lineWidth: 1.0,
                     fillColor: grey92)
      ch.background(some(st))
      ch.addObj rect

    #img.background()
    img.draw("layoutTest.pdf")
  block:
    var img = initViewport()

    let rect = img.initRect(c(0.0, 0.0),
                            width = quant(1.0, ukCentimeter),
                            height = quant(1.0, ukCentimeter),
                            color = black)

    img.layout(1, 2, colwidths = @[quant(1.0, ukRelative)],
               rowHeights = @[quant(0.25, ukRelative), quant(0.5, ukRelative)])
    for ch in mitems(img):
      let st = Style(color: color(0.0, 0.0, 1.0, 1.0),
                     lineType: ltSolid,
                     lineWidth: 1.0,
                     fillColor: grey92)
      ch.background(some(st))
      ch.addObj rect

    var view1 = img.addViewport(left = 0.1,
                                bottom = 0.1,
                                width = 0.8,
                                height = 0.8,
                                xScale = some((low: 0.0, high: 2.0 * PI)),
                                yScale = some((low: -1.0, high: 1.0)))
    view1.background()
    img.children.add view1
    #img.background()
    img.draw("debug.pdf")

  block:
    var img = initViewport(wImg = 1000,
                           hImg = 400)

    var view1 = img.addViewport(left = 0.1,
                                bottom = 0.1,
                                width = 0.8,
                                height = 0.8,
                                xScale = some((low: 0.0, high: 2.0 * PI)),
                                yScale = some((low: -1.0, high: 1.0)))
    view1.background()
    let line1 = view1.initAxis(akX)
    let line2 = view1.initAxis(akY)
    let xlabel = view1.xlabel("Energy")
    let ylabel = view1.ylabel("Count")
    view1.addObj [line1, line2, xlabel, ylabel]
    img.children.add view1
    img.draw("debugAxes.pdf")

  block testNestedAxes:
    var img = initViewport(wImg = 1000,
                           hImg = 400)

    proc addChild(v: var Viewport, num: int) =
      if num < 0:
        return
      var view1 = v.addViewport(left = 0.1,
                                bottom = 0.1,
                                width = 0.8,
                                height = 0.5,
                                xScale = some((low: 0.0, high: 2.0 * PI)),
                                yScale = some((low: -1.0, high: 1.0)))
      view1.background()
      let line1 = view1.initAxis(akX)
      let line2 = view1.initAxis(akY)
      let xlabel = view1.xlabel("Energy")
      let ylabel = view1.ylabel("Count")
      view1.addObj [line1, line2, xlabel, ylabel]
      view1.addChild(num - 1)
      v.children.add view1
    img.addChild(10)
    img.draw("testNestedAxes.pdf")


## gogLayer
## implements the prototype GoG layer
