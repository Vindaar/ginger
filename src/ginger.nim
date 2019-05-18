import math
import sugar
import chroma
import options
export options

import sequtils
import strformat
from seqmath import linspace

import ginger / [macroUtils, backends, types]
export types, backends, macroUtils

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

# makes use of backends layer
type
  GraphObjectKind* = enum
    goAxis, goText, goLabel, goTick, goTickLabel, goPoint, goLine, goRect,
    goGrid, goPolyLine

  MarkerKind* = enum
    mkCircle, mkCross, mkRotCross, mkStar

  GraphObject* = object
    children*: seq[GraphObject]
    style*: Option[Style]
    rotateInView*: Option[(float, Point)] # rotation of viewport applied to all objs
    rotate*: Option[float] # rotation around center position
    case kind*: GraphObjectKind
    of goAxis:
      axWidth*: float
      axStart*: Coord
      axStop*: Coord
    of goLabel, goText, goTickLabel:
      txtText*: string
      txtFont*: Font
      txtPos*: Coord
      txtAlign*: TextAlignKind
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
    of goPoint:
      ptMarker*: MarkerKind
      ptSize*: float
      ptColor*: Color
      ptPos*: Coord
    of goRect:
      reOrigin*: Coord
      #reBottom*: float
      #reWidth*: float
      #reHeight*: float
      reWidth*: Coord1D
      reHeight*: Coord1D
    else: discard

  # TODO:
  # - write function that applies `Style`!

  Scale* = tuple[low, high: float]

  CoordKind* = enum
    ckRelative, # relative to viewport (0.0, 1.0)
    ckData, # based on xScale, yScale of data
    ckStrWidth,#, # based on width of a string in a given fontsize
    # kinds requiring absolute scale
    ckAbsolute, # absolute to size of image ~= points
    ckCentimeter, # absolute cm based on dpi of 72.27
    ckInch # absolute inch, 2.54 cm = 1 inch

    #ckSquareRelative # relative coords but square. Takes min(width, height)
                     # as def for (0, 1)
    # ...

  Coord1D* = object
    pos*: float
    case kind*: CoordKind
    of ckRelative:
      discard
    of ckAbsolute, ckCentimeter, ckInch:
      # length of scale in points
      # 72.27 points in inch
      # 2.54 cm in inch
      # Option since this is only used to convert it to relative coords.
      length*: Option[float]
    of ckData:
      scale*: Scale
    of ckStrWidth:
      text*: string
      font*: Font

  Coord* = object #tuple[pos: Point, kind: CoordKind]
    kind*: CoordKind # should normally be populated to the coord
                    # of both x and y. In principle we can allow
                    # different coordinates in both axes though!
    x*: Coord1D
    y*: Coord1D
    #case kind: CoordKind
    #of ckRelative:
    #  discard
    #of ckAbsolute:
    #  absWidth: float
    #  absHeight: float
    #of ckData:
    #  xScale: Scale
    #  yScale: Scale
    #of ckCentimeter:
    #  # NOTE: requires a size in either inches or pixel...!
    #  discard
    #of ckStrWidth:
    #  text: string
    #  font: Font

  # - implement coordinate transformations between viewport
  #   and global coordinates
  # - coordinates should be relative coordinates in [0.0, 1.0]
  Viewport* = object
    # parameters describing the embedding into the parent
    # given relative coords
    style*: Style
    xScale*: Scale
    yScale*: Scale
    rotate*: Option[float]
    scale*: Option[float] # scaling factor to scale elements by
    origin*: Coord # "origin" of the viewport, i.e. the location of the
                  # (left, bottom) position in relative coordinates (ckRelative)
    width*: Coord1D # width in relative coordinates
    height*: Coord1D
    objects*: seq[GraphObject] # NOTE: when adding objects manually, be aware
                               # that certain transformations have to be applied
                               # manually beforehand!
    children*: seq[Viewport]
    wImg*: float # absolute width, height in points (pixels)
    hImg*: float
    backend*: BackendKind

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

func `$`(gobj: GraphObject): string =
  # string conversion function of `GraphObject`
  result = &"(GraphObject.kind: {gobj.kind}, "
  case gobj.kind
  of goAxis:
    result &= &"axWidth: {gobj.axWidth}, axStart: {gobj.axStart}, "
    result &= &"axStop: {gobj.axStop}"
  of goLabel, goText, goTickLabel:
    result &= &"txtText: {gobj.txtText}, txtPos: {gobj.txtPos}, "
    result &= &"txtAlign: {gobj.txtAlign}, txtRotate: {gobj.txtRotate}, "
    result &= &"txtFont: {gobj.txtFont}"
  of goGrid:
    result &= &"gdXPos: {gobj.gdXPos}, gdYPos: {gobj.gdYPos}"
    result &= &"gdOrigin: {gobj.gdOrigin}, gdOriginDiag: {gobj.gdOriginDiag}"
  of goTick:
    result &= &"tkPos: {gobj.tkPos}, tkMajor: {gobj.tkMajor}, tkAxis: {gobj.tkAxis}"
  of goPoint:
    result &= &"ptPos: {gobj.ptPos}, ptMarker: {gobj.ptMarker}, "
    result &= &"ptSize: {gobj.ptSize}, ptColor: {gobj.ptColor}"
  of goPolyLine:
    result &= &"plPos: {gobj.plPos}"
  of goRect:
    result &= &"reOrigin: {gobj.reOrigin}, reWidth: {gobj.reWidth}, "
    result &= &"reHeight: {gobj.reHeight}"
  else:
    result &= &"<no conversion for {gobj.kind}"
  result &= &", style: {gobj.style}, rotate: {gobj.rotate}, "
  result &= &"rotateInView: {gobj.rotateInView}"
  result &= &", children: "
  if gobj.children.len == 0:
    result &= "@[])"
  else:
    result &= "\n"
  for ch in gobj.children:
    result = &"\t {ch}\n"
  result &= ")"

template cmToInch(x: float): float = x / 2.54
template inchToAbs(x: float): float = x * 72.27
template absToInch(x: float): float = x / 72.27
template inchToCm(x: float): float = x * 2.54

func toRelative*(p: Coord1D,
                 length: Option[float] = none[float]()): Coord1D =
  ## converts the given coordinate to a relative coordinate
  case p.kind
  of ckRelative:
    result = p
  of ckAbsolute, ckCentimeter, ckInch:
    if p.length.isSome or length.isSome:
      # either get p.length or use length
      let ln = eitherOrRaise(p.length, length)
      var newPos: float
      case p.kind
      of ckAbsolute:
        newPos = p.pos / ln
      of ckCentimeter:
        newPos = p.pos.cmToInch.inchToAbs / ln
      of ckInch:
        newPos = p.pos.inchToAbs / ln
      else: raise newException(Exception, "CoordKind is invalid!")
      result = Coord1D(pos: newPos,
                       kind: ckRelative)
    else:
      raise newException(ValueError, "Cannot convert `" & $p.kind & "` into " &
        " relative coordinate if no length scale given!")
  of ckData:
    # TODO: this REQUIRES that the sclae of the Coord is up to date!
    # After potential change of data scale of plot, this will fail!
    result = Coord1D(pos: (p.pos - p.scale.low) / (p.scale.high - p.scale.low),
                     kind: ckRelative)
  of ckStrWidth:
    # can either use cairo's internals, e.g. get the extent of the string in a
    # given font, or assuming a font size in dots calculate from DPI?
    raise newException(Exception,
                       "Conversion from StrWidth to relative currently not supported!")

func toPoints*(p: Coord1D,
               length: Option[float] = none[float]()): Coord1D =
  ## converts the given coordinate to point based absolute values
  case p.kind
  of ckRelative:
    if length.isSome:
      result = Coord1D(pos: p.pos * length.get(),
                       length: length,
                       kind: ckAbsolute)
    else:
      raise newException(Exception, "Cannot convert relative coordinate to " &
        "absolute points without a length scale!")
  of ckAbsolute, ckCentimeter, ckInch:
    var newPos: float
    case p.kind
    of ckAbsolute:
      newPos = p.pos
    of ckCentimeter:
      newPos = p.pos.cmToInch.inchToAbs
    of ckInch:
      newPos = p.pos.inchToAbs
    else: raise newException(Exception, "CoordKind is invalid!")
    result = Coord1D(pos: newPos,
                     length: p.length,
                     kind: ckAbsolute)
  of ckData:
    result = result.toRelative.toPoints(length = length)
  of ckStrWidth:
    # can either use cairo's internals, e.g. get the extent of the string in a
    # given font, or assuming a font size in dots calculate from DPI?
    raise newException(Exception,
                       "Conversion from StrWidth to relative currently not supported!")

func toRelative*(p: Coord): Coord =
  ## converts the given coordinate to a relative coordinate
  result = Coord(x: p.x.toRelative,
                 y: p.y.toRelative,
                 kind: ckRelative)

proc `==`*(c1, c2: Coord1D): bool =
  ## TODO: after certain conversion we may end up with unequal values
  ## due to floating point errors. Handle?
  # check for equality of absolute value coordinate kinds first
  if c1.kind in ckAbsolute .. ckInch and
     c2.kind in ckAbsolute .. ckInch:
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

proc `+`(c1, c2: Coord1D): Coord1D =
  ## adds two Coord1D by converting to relative coordinates.
  ## Note that this may be a lossy conversion, e.g. if one is given
  ## in `ckAbsolute` (length is lost)
  result = Coord1D(pos: c1.toRelative.pos + c2.toRelative.pos,
                   kind: ckRelative)

proc `-`(c1, c2: Coord1D): Coord1D =
  ## subtracts two Coord1D by converting to relative coordinates.
  ## Note that this may be a lossy conversion, e.g. if one is given
  ## in `ckAbsolute` (length is lost)
  result = Coord1D(pos: c1.toRelative.pos - c2.toRelative.pos,
                   kind: ckRelative)

proc `*`(c1, c2: Coord1D): Coord1D =
  ## multiplies two Coord1D by converting to relative coordinates.
  ## Note that this may be a lossy conversion, e.g. if one is given
  ## in `ckAbsolute` (length is lost)
  result = Coord1D(pos: c1.toRelative.pos * c2.toRelative.pos,
                   kind: ckRelative)

proc to*(p: Coord1D, ckKind: CoordKind,
         absLength = none[float](),
         datScale = none[Scale](),
         strText = none[string](), strFont = none[Font]()): Coord1D =
  ## converts the given 1D Coordinate position in a certain coordinate
  ## system to the same position in a target coordinate system
  ## NOTE: this procedure is a potentially lossy conversion!
  # first check whether it's only a unit conversion between absolute values
  # in this case the conversion is loss free
  if ckKind in ckAbsolute .. ckInch and
     p.kind in ckAbsolute .. ckInch:
    case ckKind
    of ckAbsolute:
      result = p.toPoints
    of ckCentimeter:
      let newPos = p.toPoints.pos.absToInch.inchToCm
      result = Coord1D(pos: newPos,
                       length: p.length,
                       kind: ckCentimeter)
    of ckInch:
      let newPos = p.toPoints.pos.absToInch
      result = Coord1D(pos: newPos,
                       length: p.length,
                       kind: ckInch)
    else: raise newException(Exception, "CoordKing is invalid!")
  else:
    # first convert any point to a relative point, from which we can
    # calculate any other position easiest
    var pRel: Coord1D
    if absLength.isSome:
      pRel = p.toRelative(absLength)
    else:
      pRel = p.toRelative
    case ckKind
    of ckRelative: result = p # nothing to do
    of ckAbsolute:
      doAssert absLength.isSome, "Conversion to absolute requires a length scale!"
      result = Coord1D(pos: pRel.pos * absLength.get(),
                       length: absLength,
                       kind: ckAbsolute)
    of ckInch:
      doAssert absLength.isSome, "Conversion to inches requires an absolute length scale!"
      # assumes absLength is size in points!
      const dpi = 72.27
      result = Coord1D(pos: pRel.pos * absLength.get() / dpi,
                       length: absLength,
                       kind: ckInch)
    of ckCentimeter:
      doAssert absLength.isSome, "Conversion to inches requires an absolute length scale!"
      # assumes absLength is size in points!
      const dpi = 72.27
      const inch = 2.54
      result = Coord1D(pos: pRel.pos * absLength.get() / dpi * inch,
                       length: absLength,
                       kind: ckCentimeter)
    of ckData:
      doAssert datScale.isSome, "Conversion to data requires a scale!"
      let scale = datScale.get()
      result = Coord1D(pos: (scale.high - scale.low) * pRel.pos + scale.low,
                       scale: scale,
                       kind: ckData)
    of ckStrWidth:
      doAssert strText.isSome, "Conversion to string width requires an string!"
      doAssert strFont.isSome, "Conversion to string width requires a Font!"
      raise newException(Exception, "Conversion to string width not yet implemented!")

proc to*(p: Coord, ckKind: CoordKind,
         absWidth = none[float](), absHeight = none[float](),
         datXScale = none[Scale](), datYScale = none[Scale](),
         strText = none[string](), strFont = none[Font]()): Coord =
  ## converts the given Coordinate position in a certain coordinate
  ## system to the same position in a target coordinate system
  # first convert any point to a relative point, from which we can
  # calculate any other position easiest
  case ckKind
  of ckRelative: result = p.toRelative # nothing to do
  of ckAbsolute:
    doAssert absWidth.isSome, "Conversion to absolute requires a width!"
    doAssert absHeight.isSome, "Conversion to absolute requires a height!"
    result = Coord(x: p.x.to(ckAbsolute, absLength = absWidth),
                   y: p.y.to(ckAbsolute, absLength = absHeight),
                   kind: ckAbsolute)
  of ckInch:
    doAssert absWidth.isSome, "Conversion to inches requires a width!"
    doAssert absHeight.isSome, "Conversion to inches requires a height!"
    result = Coord(x: p.x.to(ckInch, absLength = absWidth),
                   y: p.y.to(ckInch, absLength = absHeight),
                   kind: ckInch)
  of ckCentimeter:
    doAssert absWidth.isSome, "Conversion to cm requires a width!"
    doAssert absHeight.isSome, "Conversion to cm requires a height!"
    result = Coord(x: p.x.to(ckCentimeter, absLength = absWidth),
                   y: p.y.to(ckCentimeter, absLength = absHeight),
                   kind: ckCentimeter)
  of ckData:
    doAssert datXScale.isSome, "Conversion to data requires an X scale!"
    doAssert datYScale.isSome, "Conversion to data requires a Y scale!"
    let
      xScale = datXScale.get()
      yScale = datYScale.get()
    result = Coord(x: p.x.to(ckData, datScale = datXScale),
                   y: p.y.to(ckData, datScale = datYScale),
                   kind: ckData)
  of ckStrWidth:
    doAssert strText.isSome, "Conversion to string width requires an string!"
    doAssert strFont.isSome, "Conversion to string width requires a Font!"
    raise newException(Exception, "Conversion to string width not yet implemented!")
  # case ckKind
  # of ckRelative: result = p # nothing to do
  # of ckAbsolute:
  #   doAssert absWidth.isSome, "Conversion to absolute requires a width!"
  #   doAssert absHeight.isSome, "Conversion to absolute requires a height!"
  #   result = Coord(x: pRel.x * absWidth.get(),
  #                  y: pRel.y * absHeight.get(),
  #                  absWidth: absWidth.get(),
  #                  absHeight: absHeight.get(),
  #                  kind: ckAbsolute)
  # of ckData:
  #   doAssert datXScale.isSome, "Conversion to data requires an X scale!"
  #   doAssert datYScale.isSome, "Conversion to data requires a Y scale!"
  #   let
  #     xScale = datXScale.get()
  #     yScale = datYScale.get()
  #   result = Coord(x: (xScale.high - xScale.low) * pRel.x + xScale.low,
  #                  y: (yScale.high - yScale.low) * pRel.y + yScale.low,
  #                  xScale: xScale,
  #                  yScale: yScale,
  #                  kind: ckData)
  # of ckCentimeter:
  #   raise newException(Exception, "Conversion to cm not yet implemented!")
  # of ckStrWidth:
  #   doAssert strText.isSome, "Conversion to string width requires an string!"
  #   doAssert strFont.isSome, "Conversion to string width requires a Font!"
  #  raise newException(Exception, "Conversion to string width not yet implemented!")

func initCoord1D*(at: float, kind: CoordKind = ckRelative): Coord1D =
  ## returns a Coord1D at coordinate `at` of kind `kind`
  result = Coord1D(pos: at, kind: kind)

func initCoord*(x, y: float, kind: CoordKind = ckRelative): Coord =
  ## returns a coordinate at coordinates x, y of kind `kind`
  result = Coord(x: initCoord1D(x, kind = kind),
                 y: initCoord1D(y, kind = kind),
                 kind: kind)

func patchCoord(c: Coord1D, length: float): Coord1D =
  ## patches the given coordinate in case it requries an absolute
  ## scale (ckAbsolute .. ckInch) to have a length field
  result = c
  if result.kind in ckAbsolute .. ckInch and not result.length.isSome:
    result.length = some(length)

func patchCoord(c: Coord, view: Viewport): Coord =
  ## patches the coordinate if it contains an absolute scale to have
  ## width / height inherited from its viewport
  result = Coord(x: c.x.patchCoord(view.wImg),
                 y: c.y.patchCoord(view.hImg),
                 kind: c.kind)

proc `[]`(view: Viewport, idx: int): Viewport =
  ## returns the `idx` child of `view`
  if view.children.len > idx:
    result = view.children[idx]
  else:
    raise newException(IndexError, "`idx` is invalid for " & $view.children.len &
      " children viewports!")

proc `[]=`(view: var Viewport, idx: int, viewToSet: Viewport) =
  ## override the `idx` child of `view` with `viewToSet`
  if view.children.len > idx:
    view.children[idx] = viewToSet
  else:
    raise newException(IndexError, "`idx` is invalid for " & $view.children.len &
      " children viewports!")

proc len(view: Viewport): int = view.children.len
proc high(view: Viewport): int = view.len - 1

proc left(view: Viewport): float =
  ## returns the left (x) position of the `Viewport` in `ckRelative`
  result = view.origin.x.toRelative.pos

proc bottom(view: Viewport): float =
  ## returns the bottom (y) position of the `Viewport` in `ckRelative`
  result = view.origin.y.toRelative.pos

proc width(view: Viewport): float =
  ## returns the width of the `Viewport` in `ckRelative`
  result = view.width.toRelative.pos

proc height(view: Viewport): float =
  ## returns the height of the `Viewport` in `ckRelative`
  result = view.height.toRelative.pos

func updateScale(view: Viewport, c: var Coord1D, axKind: AxisKind) =
  ## update the scale coordinate of the 1D coordinate `c` in place
  if c.kind == ckData:
    case axKind
    of akX:
      c.scale = view.xScale
    of akY:
      c.scale = view.yScale

func updateScale(view: Viewport, c: Coord1D, axKind: AxisKind): Coord1D =
  ## update the scale coordinate of the 1D coordinate `c` in place
  result = c
  view.updateScale(result, axKind)

func updateScale(view: Viewport, c: var Coord) =
  ## update the scale coordinate of the coordinate `c` in place
  view.updateScale(c.x, akX)
  view.updateScale(c.y, akY)

func updateScale(view: Viewport, c: Coord): Coord =
  ## same as above, but returns a mutated copy
  result = c
  view.updateScale(result)

func updateDataScale(view: Viewport, obj: var GraphObject) =
  ## updates the data scale associated to the `obj`
  case obj.kind
  of goAxis:
    view.updateScale(obj.axStart)
    view.updateScale(obj.axStop)
  of goLabel, goText, goTickLabel:
    view.updateScale(obj.txtPos)
  of goGrid:
    obj.gdXPos.applyIt(view.updateScale(it, akX))
    obj.gdYPos.applyIt(view.updateScale(it, akY))
  of goTick:
    view.updateScale(obj.tkPos)
  of goPoint:
    view.updateScale(obj.ptPos)
  of goRect:
    view.updateScale(obj.reOrigin)
    view.updateScale(obj.reWidth, akX)
    view.updateScale(obj.reHeight, akY)
  else:
    raise newException(Exception, "updating of goLine not yet implemented!")

func updateDataScale(view: Viewport,
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

func addObj(view: var Viewport, obj: GraphObject) =
  ## adds the given `obj` to the viewport's objects and makes
  ## sure it inherits all properties, e.g. Style and data scales
  var mobj = obj
  # check if user assigned a style to overwrite viewport style
  if not obj.style.isSome:
    mobj.style = some(view.style)
  # (potentially) update data scale
  view.updateDataScale(mobj)
  view.objects.add mobj

func addObj(view: var Viewport, objs: varargs[GraphObject]) =
  ## adds the `objs` to the viewport's objects and makes
  ## sure they inherit all properties, e.g. Style and data scales
  for obj in objs:
    view.addObj obj

proc convertToKind(c: Coord1D, toKind: Coord1D): Coord1D =
  ## converts the coordinate `c` to the kind of `toKind`
  case toKind.kind
  of ckRelative:
    result = c.toRelative
  of ckAbsolute:
    if toKind.length.isSome:
      result = c.to(ckAbsolute,
                    absLength = toKind.length)
    else:
      raise newException(ValueError, "Conversion to `ckAbsolute` requires a length!")
  of ckInch:
    if toKind.length.isSome:
      result = c.to(ckInch,
                    absLength = toKind.length)
    else:
      raise newException(ValueError, "Conversion to `ckInch` requires a length!")
  of ckCentimeter:
    if toKind.length.isSome:
      result = c.to(ckCentimeter,
                    absLength = toKind.length)
    else:
      raise newException(ValueError, "Conversion to `ckCentimeter` requires a length!")
  of ckData:
    result = c.to(ckData,
                  datScale = some(toKind.scale))
  else:
    raise newException(Exception, "convertToKind not implemented for " & $toKind.kind)

#proc translate(c: Coord1D, byCoord: Coord1D): Coord1D =
#  ## translates the coordinate `c` by `byCoord`
#  let pos = c.toRelative.pos + byCoord.toRelative.pos
#  result = Coord1D(pos: pos, kind: ckRelative)
#  result = result.convertToKind(c)

#proc translate(c: Coord, byCoord: Coord): Coord =
#  result = Coord(x: c.x.translate(byCoord.x),
#                 y: c.y.translate(byCoord.y),
#                 kind: c.kind)

proc embedInto(c: Coord, view: Viewport): Coord =
  ## embeds the coordinate `c` into `Viewport`
  result = Coord(x: Coord1D(pos: left(view) + width(view) * c.x.toRelative.pos,
                            kind: ckRelative),
                 y: Coord1D(pos: bottom(view) + height(view) * c.y.toRelative.pos,
                            kind: ckRelative),
                 kind: ckRelative)

proc embedInto(view: Viewport, into: Viewport): Viewport =
  ## embeds the given `view` into the `into` Viewport by embedding
  ## the (left, bottom) coordinates and scaling the width / height
  result = view
  result.origin = result.origin.embedInto(into)
  result.width = result.width * into.width#Coord1D(pos: result.width.toRelative.pos * into.width.toRelative.pos,
                                          #        kind: ckRelative)
  result.height = result.height * into.height

proc point(c: Coord): Point =
  ## converts the given coordinate to `ckRelative` and returns the position as a
  ## `Point`
  #result = (x: c.x.toRelative.pos, y: c.y.toRelative.pos)
  result = (x: c.x.pos, y: c.y.pos)





################################################################################
############ INIT FUNCTIONS
################################################################################






proc initViewport*(origin: Coord,
                   width, height: Coord1D,
                   style = none[Style](),
                   xScale = none[Scale](),
                   yScale = none[Scale](),
                   rotate = none[float](),
                   scale = none[float](),
                   wImg = 640.0,
                   hImg = 480.0,
                   backend = bkCairo): Viewport =
  ## initializes a `Viewport` with `origin` in any coordinate system
  ## with 1D coordinates providing width and height
  ## Uses Coord1D to allow to define sizes in arbitrary units
  result = Viewport(origin: origin,
                    width: width,
                    height: height,
                    rotate: rotate,
                    scale: scale,
                    wImg: wImg,
                    hImg: hImg,
                    backend: backend)
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
                   wImg = 640.0,
                   hImg = 480.0,
                   backend = bkCairo): Viewport =
  ## convenience init function for Viewport using relative coordinates
  let origin = Coord(x: Coord1D(pos: left, kind: ckRelative),
                     y: Coord1D(pos: bottom, kind: ckRelative))
  let
    widthCoord = Coord1D(pos: width, kind: ckRelative)
    heightCoord = Coord1D(pos: height, kind: ckRelative)
  result = initViewport(origin, widthCoord, heightCoord,
                        style, xScale, yScale, rotate, scale,
                        wImg, hImg, backend)

proc addViewport*(view: var Viewport,
                  origin: Coord,
                  width, height: Coord1D,
                  style = none[Style](),
                  xScale = none[Scale](),
                  yScale = none[Scale](),
                  rotate = none[float](),
                  scale = none[float]()): Viewport =
  ## add a new viewport with the given settings to the `view`
  ## TODO: do not return viewchild???
  var viewChild = initViewport(origin.patchCoord(view),
                               width.patchCoord(view.wImg),
                               height.patchCoord(view.hImg),
                               style, xScale, yScale, rotate, scale,
                               backend = view.backend)
  # override width and height
  viewChild.wImg = view.wImg * width.toRelative.pos
  viewChild.hImg = view.hImg * height.toRelative.pos
  # TODO: this is not useful, since all objects have value semantics, i.e. if we change it
  # the change is not reflected, since we work on a copy.
  view.children.add viewChild
  result = viewChild

proc addViewport*(view: var Viewport,
                  left = 0.0, bottom = 0.0, width = 1.0, height = 1.0,
                  style = none[Style](),
                  xScale = none[Scale](),
                  yScale = none[Scale](),
                  rotate = none[float](),
                  scale = none[float]()): Viewport =
  ## add a new viewport with the given settings to the `view`, set at relative
  ## coordinates (left, bottom), (width, height)
  ## TODO: Do not return viewchild???
  let origin = Coord(x: Coord1D(pos: left, kind: ckRelative),
                     y: Coord1D(pos: bottom, kind: ckRelative))
  let
    widthCoord = Coord1D(pos: width, kind: ckRelative)
    heightCoord = Coord1D(pos: height, kind: ckRelative)
  result = view.addViewport(origin, widthCoord, heightCoord, style, xScale,
                            yScale, rotate, scale)

proc initAxis(view: Viewport,
              axKind: AxisKind,
              width = 1.0,
              color = color(0.0, 0.0, 0.0)): GraphObject =
  var axis = GraphObject(kind: goAxis,
                         axWidth: width,
                         axStart: initCoord(0.0, 1.0),
                         axStop: initCoord(1.0, 1.0),
                         style: some(Style(color: color,
                                           lineWidth: width)))
  case axKind
  of akX:
    result = axis
  of akY:
    result = replace(axis):
      axStart = initCoord(0.0, 0.0)
      axStop = initCoord(0.0, 1.0)

proc xaxis(view: Viewport,
           width = 1.0,
           color = color(0.0, 0.0, 0.0)): GraphObject =
  result = view.initAxis(akX, width, color)

proc yaxis(view: Viewport,
           width = 1.0,
           color = color(0.0, 0.0, 0.0)): GraphObject =
  result = view.initAxis(akY, width, color)

proc initRect(view: Viewport,
              origin: Coord,
              width, height: Coord1D,
              color = color(0.0, 0.0, 0.0),
              style = none[Style]()): GraphObject =
  result = GraphObject(kind: goRect,
                       reOrigin: origin.patchCoord(view),
                       reWidth: width.patchCoord(view.wImg),
                       reHeight: height.patchCoord(view.hImg))
  if style.isSome:
    result.style = style
  else:
    result.style = some(Style(lineWidth: 0.0,
                              color: color(0.0, 0.0, 0.0, 0.0),
                              size: 0.0,
                              lineType: ltSolid,
                              fillColor: color))

proc initRect(view: Viewport,
              left, bottom, width, height: float,
              color = color(0.0, 0.0, 0.0),
              style = none[Style]()): GraphObject =
  let origin = Coord(x: Coord1D(pos: left, kind: ckRelative),
                     y: Coord1D(pos: bottom, kind: ckRelative))
  let
    widthCoord = Coord1D(pos: width, kind: ckRelative)
    heightCoord = Coord1D(pos: height, kind: ckRelative)
  result = view.initRect(origin = origin,
                         width = widthCoord,
                         height = heightCoord,
                         color = color,
                         style = style)

proc initText(view: Viewport,
              origin: Coord,
              text: string,
              alignKind: TextAlignKind,
              font: Font,
              rotate = none[float]()): GraphObject =
  result = GraphObject(kind: goText,
                       txtText: text,
                       txtFont: font,
                       txtAlign: alignKind,
                       txtPos: origin.patchCoord(view))
  if rotate.isSome:
    result.rotate = some(rotate.get())

proc scaleTo(p: Point, view: Viewport): Point =
  ## scales the point from data coordinates to viewport coordinates
  discard

proc initPoint(view: Viewport,
               pos: Point,
               size = 3.0,
               marker: MarkerKind = mkCircle,
               color = color(0.0, 0.0, 0.0)): GraphObject =
  result = GraphObject(kind: goPoint,
                       ptMarker: marker,
                       ptSize: size,
                       ptColor: color,
                       ptPos: Coord(x: Coord1D(pos: pos.x, scale: view.xScale, kind: ckData),
                                    y: Coord1D(pos: pos.y, scale: view.yScale, kind: ckData)))
                       #ptPos: pos.scaleTo(view))

proc initAxisLabel(view: Viewport,
                   label: string,
                   axKind: AxisKind,
                   margin: Coord1D,
                   font: Option[Font] = none[Font]()): GraphObject =
  ## margin is positive value!
  result = GraphObject(kind: goText,
                       txtText: label,
                       txtAlign: taCenter)
  if font.isSome:
    result.txtFont = font.get()
  else:
    result.txtFont = Font(family: "sans-serif", size: 12.0, color: color(0.0, 0.0, 0.0))
  case axKind
  of akX:
    # TODO: fix positions based on absolute unit (e.g. cm) instead of
    # relatives?
    let yPos = initCoord1D(1.0) + Coord1D(pos: margin.pos,
                                          length: some(view.hImg),
                                          kind: ckCentimeter)
    result.txtPos = Coord(x: initCoord1D(0.5),
                          y: ypos)
  of akY:
    let xPos = Coord1D(pos: -margin.pos,
                       length: some(view.wImg),
                       kind: ckCentimeter)
    result.txtPos = Coord(x: xPos,
                          y: initCoord1D(0.5))
    result.rotate = some(-90.0)

proc xlabel(view: Viewport,
            label: string,
            font = Font(family: "sans-serif", size: 12.0, color: color(0.0, 0.0, 0.0)),
            margin = 1.25): GraphObject =
  result = view.initAxisLabel(label = label,
                              axKind = akX,
                              margin = initCoord1D(margin, ckCentimeter),
                              font = some(font))

proc ylabel(view: Viewport,
            label: string,
            font = Font(family: "sans-serif", size: 12.0, color: color(0.0, 0.0, 0.0)),
            margin = 1.25): GraphObject =
  result = view.initAxisLabel(label = label,
                              axKind = akY,
                              margin = initCoord1D(margin, ckCentimeter),
                              font = some(font))

proc initTickLabel(view: Viewport,
                   tick: GraphObject,
                   font: Font = Font(family: "sans-serif", size: 8.0, color: color(0.0, 0.0, 0.0)),
                   rotate = none[float]()): GraphObject =
  doAssert tick.kind == goTick, "object must be a `goTick` to create a `goTickLabel`!"
  var label: GraphObject
  var text: string
  var origin: Coord
  let loc = tick.tkPos
  case tick.tkAxis
  of akX:
    let xCoord = Coord1D(pos: loc.x.pos, kind: ckData, scale: loc.x.scale)
    let yCoord = Coord1D(pos: loc.y.scale.high, kind: ckData, scale: loc.y.scale)
    origin = Coord(x: xCoord,
                   y: yCoord + Coord1D(pos: 0.5, kind: ckCentimeter, length: some(view.hImg)),
                   kind: ckData)
    text = &"{loc.x.pos:.2f}"
  of akY:
    let xCoord = Coord1D(pos: loc.x.scale.low, kind: ckData, scale: loc.x.scale)
    let yCoord = Coord1D(pos: loc.y.pos, kind: ckData, scale: loc.y.scale)
    origin = Coord(x: xCoord - Coord1D(pos: 0.5, kind: ckCentimeter, length: some(view.wImg)),
                   y: yCoord,
                   kind: ckData)
    text = &"{loc.y.pos:.2f}"
  result = view.initText(origin, text, taCenter, font, rotate)

proc tickLabels(view: Viewport, ticks: seq[GraphObject],
                font: Font = Font(family: "sans-serif", size: 8.0, color: color(0.0, 0.0, 0.0))): seq[GraphObject] =
  ## returns all tick labels for the given ticks
  result = ticks.mapIt(view.initTickLabel(it, font))

proc initTick(view: Viewport,
              axKind: AxisKind,
              major: bool,
              at: Coord,
              style: Option[Style] = none[Style]()): GraphObject =
  result = GraphObject(kind: goTick,
                       tkPos: at.patchCoord(view),
                       tkMajor: major,
                       tkAxis: axKind)
  if style.isSome:
    result.style = style
  else:
    result.style = some(Style(lineWidth: 1.0, # width of tick
                              color: color(0.0, 0.0, 0.0),
                              size: 5.0, # total length of tick
                              lineType: ltSolid))

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

proc calcTickLocations(scale: Scale, numTicks: int): (Scale, float, int) =
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

proc initTicks(view: var Viewport,
               axKind: AxisKind,
               numTicks: int = 0,
               tickLocs: seq[Coord] = @[],
               major = true,
               style: Option[Style] = none[Style]()): seq[GraphObject] =
  # check whether there
  if numTicks == 0 and tickLocs.len == 0:
    raise newException(ValueError, "Either need a number of ticks or tick " &
      "locations if auto tick locations not used!")
  if numTicks == 0 and tickLocs.len > 0:
    for loc in tickLocs:
      result.add initTick(view, axKind = axKind, major = major, at = loc,
                          style = style)
  elif numTicks > 0:
    var scale: Scale
    if axKind == akX:
      scale = view.xScale
    else:
      scale = view.yScale

    let (newScale, newWidth, newNumTicks) = calcTickLocations(scale, numTicks)
    var autoTickLocs: seq[Coord]
    if axKind == akX:
      autoTickLocs = linspace(newScale.low, newScale.high, newNumTicks + 1).mapIt(
          Coord(x: Coord1D(pos: it, kind: ckData, scale: newScale),
                y: Coord1D(pos: view.yScale.high, kind: ckData, scale: view.yScale),
                kind: ckData)
        )
    else:
      autoTickLocs = linspace(newScale.low, newScale.high, newNumTicks + 1).mapIt(
        Coord(x: Coord1D(pos: view.xScale.low, kind: ckData, scale: view.xScale),
              y: Coord1D(pos: it, kind: ckData, scale: newScale),
              kind: ckData)
      )
    result = view.initTicks(axKind, tickLocs = autoTickLocs,
                            major = major, style = style)
    # finally update the scale associated to the view
    case axKind
    of akX:
      view.xScale = newScale
    of akY:
      view.yScale = newScale

    # and update the scales of all objects owned by the viewport
    view.updateDataScale(view.objects)

proc xticks(view: var Viewport,
            numTicks: int = 10,
            tickLocs: seq[Coord] = @[],
            major = true,
            style: Option[Style] = none[Style]()): seq[GraphObject] =
  result = view.initTicks(akX,
                          numTicks = numTicks,
                          tickLocs = tickLocs,
                          major = true,
                          style = style)

proc yticks(view: var Viewport,
            numTicks: int = 10,
            tickLocs: seq[Coord] = @[],
            major = true,
            style: Option[Style] = none[Style]()): seq[GraphObject] =
  result = view.initTicks(akY,
                          numTicks = numTicks,
                          tickLocs = tickLocs,
                          major = true,
                          style = style)

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
  let cdiv2 = Coord1D(pos: 2.0, kind: ckData, scale: scale)
  for i in 0 ..< ticks.high: # ignore last tick
    doAssert ticks[i].kind == goTick, "Elements for grid lines must be `goTick`!"
    debugecho "Current tick ", ticks[i]
    # calculate position between both ticks
    case axKind
    of akX:
      let midPos = (ticks[i].tkPos.x + ticks[i+1].tkPos.x) / cdiv2
      result[i] = midPos
    of akY:
      let midPos = (ticks[i].tkPos.y + ticks[i+1].tkPos.y) / cdiv2
      result[i] = midPos
    debugecho "Resulting pos ", result[i].pos

proc initGridLines(view: Viewport,
                   xticks: Option[seq[GraphObject]] = none[seq[GraphObject]](),
                   yticks: Option[seq[GraphObject]] = none[seq[GraphObject]](),
                   major = true,
                   style: Option[Style] = none[Style]()): GraphObject =
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

  if major:
    # take tick positions directly
    if xticks.isSome:
      result.gdXPos = xticks.unsafeGet.mapIt(it.tkPos.x)
    if yticks.isSome:
      result.gdYPos = yticks.unsafeGet.mapIt(it.tkPos.y)
  else:
    # if log scale, no minor
    #if not logScale:
    # if normal scale, minor lines at (x_i + x_{i+1})/2
    if xticks.isSome:
      let ticks = xticks.get()
      result.gdXPos = calcMinorTicks(ticks, akX)
      echo "Positions are ", result.gdXPos
    if yticks.isSome:
      let ticks = yticks.get()
      result.gdYPos = calcMinorTicks(ticks, akY)

################################################################################
########## DRAWING FUNCTIONS
################################################################################





proc drawAxis(img: BImage, gobj: GraphObject) =
  doAssert gobj.kind == goAxis, "object must be a `goAxis`!"
  img.drawLine(gobj.axStart.point, gobj.axStop.point,
               gobj.style.get, # if we end up here without a style,
                               # it's a bug!
               rotateAngle = gobj.rotateInView)

proc drawRect(img: BImage, gobj: GraphObject) =
  doAssert gobj.kind == goRect, "object must be a `goRect`!"
  img.drawRectangle(gobj.reOrigin.point.x, gobj.reOrigin.point.y,
                    gobj.reWidth.pos, gobj.reHeight.pos,
                    gobj.style.get, # if we end up here without a style,
                                    # it's a bug!
                    gobj.rotateInView)

proc drawPoint(img: BImage, gobj: GraphObject) =
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

proc drawText(img: BImage, gobj: GraphObject) =
  doAssert(
    (gobj.kind == goText or gobj.kind == goLabel),
    "object must be a `goText` or `goLabel`!"
  )
  img.drawText(gobj.txtText, gobj.txtFont, gobj.txtPos.point, gobj.txtAlign,
               gobj.rotate)

proc drawTick(img: BImage, gobj: GraphObject) =
  ## draw a tick
  doAssert gobj.kind == goTick, "object must be a `goTick`!"
  var style = gobj.style.get() # style *has* to exist
  var length = style.size
  if not gobj.tkMajor:
    # minor ticks use half the width of normal ticks
    style.lineWidth = style.lineWidth / 2.0
    length = length / 2.0

  case gobj.tkAxis
  of akX:
    let tkStart = gobj.tkPos.point.y + length
    let tkStop = gobj.tkPos.point.y - length
    let tkX = gobj.tkPos.point.x
    img.drawLine((tkX, tkStart),
                 (tkX, tkStop),
                 style,
                 rotateAngle = gobj.rotateInView)
  of akY:
    let tkStart = gobj.tkPos.point.x + length
    let tkStop = gobj.tkPos.point.x - length
    let tkY = gobj.tkPos.point.y
    img.drawLine((tkStart, tkY),
                 (tkStop, tkY),
                 style,
                 rotateAngle = gobj.rotateInView)

proc drawGrid(img: BImage, gobj: GraphObject) =
  ## draws the (major / minor) grid
  doAssert gobj.kind == goGrid, "object must be a `goGrid`!"
  var style = gobj.style.get() # style *has* to exist
  # start with vertical lines
  for x in gobj.gdXPos:
    echo "Drawing at ", x.pos
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

proc toGlobalCoords(gobj: GraphObject, img: BImage): GraphObject =
  result = gobj
  case gobj.kind
  of goAxis:
    result.axStart = result.axStart.to(ckAbsolute,
                                       absWidth = some(img.width.float),
                                       absHeight = some(img.height.float))
    result.axStop = result.axStop.to(ckAbsolute,
                                     absWidth = some(img.width.float),
                                     absHeight = some(img.height.float))
  of goRect:
    result.reOrigin = result.reOrigin.to(ckAbsolute,
                                         absWidth = some(img.width.float),
                                         absHeight = some(img.height.float))
    result.reWidth = Coord1D(pos: result.reWidth.toRelative.pos * img.width.float,
                             kind: ckRelative)
    result.reHeight = Coord1D(pos: result.reHeight.toRelative.pos * img.height.float,
                            kind: ckRelative)

  of goPoint:
    #result.ptPos = gobj.ptPos.toRelative #Coord(x: Coord1D(pos: gobj.ptPos.x, scale:
    result.ptPos = gobj.ptPos.to(ckAbsolute,
                                 absWidth = some(img.width.float),
                                 absHeight = some(img.height.float))
  of goText, goLabel:
    result.txtPos = gobj.txtPos.to(ckAbsolute,
                                   absWidth = some(img.width.float),
                                   absHeight = some(img.height.float))
  of goTick:
    result.tkPos = gobj.tkPos.to(ckAbsolute,
                                 absWidth = some(img.width.float),
                                 absHeight = some(img.height.float))
  of goGrid:
    result.gdOrigin = gobj.gdOrigin.toAbsImage(img)
    result.gdOriginDiag = gobj.gdOriginDiag.toAbsImage(img)
    result.gdXPos = gobj.gdXPos.mapIt(it.to(ckAbsolute,
                                            absLength = some(img.width.float)))
    result.gdYPos = gobj.gdYPos.mapIt(it.to(ckAbsolute,
                                            absLength = some(img.height.float)))

  else:
    raise newException(Exception, "Not yet implemented!")

proc draw*(img: BImage, gobj: GraphObject) =
  ## draws the given graph object on the image
  let globalObj = gobj.toGlobalCoords(img)

  case gobj.kind
  of goAxis:
    img.drawAxis(globalObj)
  of goRect:
    img.drawRect(globalObj)
  #of goLabel:
  #  img.drawLabel(gobj)
  of goPoint:
    img.drawPoint(globalObj)
  of goLabel, goText:
    img.drawText(globalObj)
  of goTick:
    img.drawTick(globalObj)
  of goGrid:
    img.drawGrid(globalObj)
  #of goLine:
  #  img.drawLine(gobj)
  else:
    raise newException(Exception, "Not implemented yet!")

iterator items*(view: Viewport): Viewport =
  for ch in view.children:
    yield ch

iterator mitems*(view: var Viewport): Viewport =
  for ch in mitems(view.children):
    yield ch

#proc translate(p: Point, view: Viewport): Point =
#  result = (left(view) + p.x * (width(view)),
#            bottom(view) + p.y * (height(view)))

proc transform(gobj: GraphObject, view: Viewport): GraphObject =
  result = gobj
  case gobj.kind
  of goAxis:
    result.axStart = result.axStart.embedInto(view)
    result.axStop = result.axStop.embedInto(view)
  of goRect:
    result.reOrigin = gobj.reOrigin.embedInto(view)
    result.reWidth = result.reWidth * view.width
    result.reHeight = result.reHeight * view.height
  of goPoint:
    result.ptPos = gobj.ptPos.embedInto(view)
  of goLabel, goText:
    result.txtPos = gobj.txtPos.embedInto(view)
  of goTick:
    result.tkPos = gobj.tkPos.embedInto(view)
  of goGrid:
    # assign and convert origin and diagonal origin
    result.gdOrigin = view.origin
    result.gdOriginDiag = Coord(x: view.origin.x + view.width,
                                y: view.origin.y + view.height,
                                kind: ckRelative)
    result.gdXPos = gobj.gdXPos.mapIt(view.origin.x + it * view.width)
    result.gdYPos = gobj.gdYPos.mapIt(view.origin.y + it * view.height)
  else:
    raise newException(Exception, "transform not implemented yet!")

proc draw(img: BImage, view: Viewport) =
  ## draws the full viewport including all objects and all
  ## children onto the image
  ## NOTE: children are drawn `after` the parent viewport
  # draw objects
  # before we draw stuff apply potential rotation
  let
    centerX = left(view) + width(view) / 2.0
    centerY = bottom(view) + height(view) / 2.0
  for obj in view.objects:
    # transform the object to draw to the global image coordinate system
    # and draw
    var mobj = obj
    # first check if object shall be rotate individually
    if view.rotate.isSome:
      mobj.rotateInView = some((view.rotate.get,
                               (centerX, centerY).scale(img.width, img.height)))
    img.draw(mobj.transform(view))

  # draw all children viewports
  for chView in view:
    var mchView = chView.embedInto(view)
    mchView.rotate = view.rotate
    img.draw(mchView)

proc draw*(view: Viewport, filename: string, ftype = fkPdf) =
  ## draws the given viewport and all its children and stores it in the
  ## file `filename`
  echo view.backend
  var img = initBImage(filename,
                       width = view.wImg.round.int, height = view.hImg.round.int,
                       backend = view.backend,
                       ftype = ftype)
  img.draw(view)
  img.destroy()

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
    let x = linspace(0.0, 6.28, 1_000)
    let y = x.mapIt(sin(it))
    let points = zip(x, y)
    var gobjPoints: seq[GraphObject]
    for p in points:
      gobjPoints.add initPoint(view2, (x: p.a, y: p.b),
                               marker = mkCross)

    let
      xticks = view1.xticks()
      yticks = view1.yticks()
      xtickLabels = view1.tickLabels(xticks)
      ytickLabels = view1.tickLabels(yticks)
    for x in yticks:
      echo x.tkPos
    #if true:
    #  quit()

    #img.drawRectangle(200.0, 100.0, 150.0, 100.0)
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
                                  width = Coord1D(pos: 1, kind: ckCentimeter),
                                  height = Coord1D(pos: 1, kind: ckCentimeter))

    let inchSquare = view1.initRect(initCoord(0.3, 0.3),
                                  width = Coord1D(pos: 1, kind: ckInch),
                                  height = Coord1D(pos: 1, kind: ckInch))

    let xlabel = view1.xlabel("Energy")
    let ylabel = view1.ylabel("Count")

    view1.addObj concat(gobjPoints, xticks, yticks, xticklabels, yticklabels, @[line1, line2, rect, xlabel, ylabel, cmSquare, inchSquare])
    let grdlines = view1.initGridLines(some(xticks), some(yticks))
    let grdLnMinor = view1.initGridLines(some(xticks), some(yticks), major = false)

    view2.addObj concat(@[rect2], gobjPoints)
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
                              alignKind = taCenter,
                              font = Font(family: "serif",
                                   size: 12.0,
                                   color: color(0.0, 0.0, 0.0)))


    let textRot = view2.initText(initCoord(-0.05, 0.5),
                                 "Counts",
                                 alignKind = taCenter,
                                 font = Font(family: "serif",
                                             size: 12.0,
                                             color: color(0.0, 0.0, 0.0)),
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




## gogLayer
## implements the prototype GoG layer
