import math
import sugar
import chroma
import options
export options

import ginger / [macroUtils, backends, types]
export types, backends, macroUtils

# TODO: think about renaming `Coord1D` to someting like Unit?

## backendLayer
## implements the prototype backend layer
# requires
# done in backends.nim / backendsCairo.nim

## baseLayer
## implements the prototype base layer

# makes use of backends layer
type
  GraphObjectKind* = enum
    goAxis, goLabel, goTick, goPoint, goLine, goRect

  MarkerKind* = enum
    mkCircle, mkCross, mkRotCross, mkStar

  GraphObject* = object
    children*: seq[GraphObject]
    style*: Style
    rotate*: Option[(float, Point)]
    case kind*: GraphObjectKind
    of goAxis:
      axWidth*: float
      axStart*: Point
      axStop*: Point
    of goLabel:
      lbText*: string
      lbFont*: Font
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
    ckAbsolute, # absolute to size of image
    ckData, # based on xScale, yScale of data
    ckCentimeter # absolute cm based on dpi of 72.27
    ckStrWidth # based on width of a string in a given fontsize
    # ...

  Coord1D* = object
    pos*: float
    case kind*: CoordKind
    of ckRelative:
      discard
    of ckAbsolute:
      length*: float
    of ckData:
      scale*: Scale
    of ckCentimeter:
      # NOTE: requires a size in either inches or pixel...!
      discard
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
    width*: Coord1D
    height*: Coord1D
    objects*: seq[GraphObject]
    children*: seq[Viewport]

func toRelative*(p: Coord1D): Coord1D =
  ## converts the given coordinate to a relative coordinate
  case p.kind
  of ckRelative:
    result = p
  of ckAbsolute:
    result = Coord1D(pos: p.pos / p.length,
                     kind: ckRelative)
  of ckData:
    result = Coord1D(pos: (p.pos - p.scale.low) / (p.scale.high - p.scale.low),
                     kind: ckRelative)
  of ckCentimeter:
    const dpi = 72.27
    const inch = 2.54
    #result = Coord(x:
    raise newException(Exception,
                       "Conversion from cm to relative currently not supported!")
  of ckStrWidth:
    # can either use cairo's internals, e.g. get the extent of the string in a
    # given font, or assuming a font size in dots calculate from DPI?
    raise newException(Exception,
                       "Conversion from StrWidth to relative currently not supported!")


func toRelative*(p: Coord): Coord =
  ## converts the given coordinate to a relative coordinate
  result = Coord(x: p.x.toRelative,
                 y: p.x.toRelative,
                 kind: ckRelative)
  #case p.kind
  #of ckRelative:
  #  result = p
  #of ckAbsolute:
  #  result = Coord(x: p.x / p.absWidth,
  #                 y: p.y / p.absHeight,
  #                 kind: ckRelative)
  #of ckData:
  #  result = Coord(x: (p.x - p.xScale.low) / (p.xScale.high - p.xScale.low),
  #                 y: (p.y - p.yScale.low) / (p.yScale.high - p.yScale.low),
  #                 kind: ckRelative)
  #of ckCentimeter:
  #  const dpi = 72.27
  #  const inch = 2.54
  #  #result = Coord(x:
  #  raise newException(Exception,
  #                     "Conversion from cm to relative currently not supported!")
  #of ckStrWidth:
  #  # can either use cairo's internals, e.g. get the extent of the string in a
  #  # given font, or assuming a font size in dots calculate from DPI?
  #  raise newException(Exception,
  #                     "Conversion from StrWidth to relative currently not supported!")

proc `==`*(c1, c2: Coord): bool =
  let
    c1Rel = c1.toRelative
    c2Rel = c2.toRelative
  result = if c1Rel.x.pos == c2Rel.x.pos and
              c1Rel.y.pos == c2Rel.y.pos:
             true
           else:
             false

proc to*(p: Coord1D, ckKind: CoordKind,
         absLength = none[float](),
         datScale = none[Scale](),
         strText = none[string](), strFont = none[Font]()): Coord1D =
  ## converts the given 1D Coordinate position in a certain coordinate
  ## system to the same position in a target coordinate system
  ## NOTE: this procedure is a potentially lossy conversion!
  # first convert any point to a relative point, from which we can
  # calculate any other position easiest
  let pRel = p.toRelative
  case ckKind
  of ckRelative: result = p # nothing to do
  of ckAbsolute:
    doAssert absLength.isSome, "Conversion to absolute requires a length scale!"
    result = Coord1D(pos: pRel.pos * absLength.get(),
                     length: absLength.get(),
                     kind: ckAbsolute)
  of ckData:
    doAssert datScale.isSome, "Conversion to data requires a scale!"
    let scale = datScale.get()
    result = Coord1D(pos: (scale.high - scale.low) * pRel.pos + scale.low,
                     scale: scale,
                     kind: ckData)
  of ckCentimeter:
    raise newException(Exception, "Conversion to cm not yet implemented!")
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
  of ckData:
    doAssert datXScale.isSome, "Conversion to data requires an X scale!"
    doAssert datYScale.isSome, "Conversion to data requires a Y scale!"
    let
      xScale = datXScale.get()
      yScale = datYScale.get()
    result = Coord(x: p.x.to(ckData, datScale = datXScale),
                   y: p.y.to(ckData, datScale = datYScale),
                   kind: ckData)
  of ckCentimeter:
    raise newException(Exception, "Conversion to cm not yet implemented!")
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

proc initViewport*(origin: Coord,
                   width, height: Coord1D,
                   style = none[Style](),
                   xScale = none[Scale](),
                   yScale = none[Scale](),
                   rotate = none[float](),
                   scale = none[float]()): Viewport =
  ## initializes a `Viewport` with `origin` in any coordinate system
  ## with 1D coordinates providing width and height
  ## Uses Coord1D to allow to define sizes in arbitrary units
  result = Viewport(origin: origin,
                    width: width,
                    height: height,
                    rotate: rotate,
                    scale: scale)
  if style.isSome:
    result.style = style.get()
  if xScale.isSome:
    result.xScale = xScale.get()
  if yScale.isSome:
    result.yScale = yScale.get()

proc initViewport*(left, bottom, width, height: float,
                   style = none[Style](),
                   xScale = none[Scale](),
                   yScale = none[Scale](),
                   rotate = none[float](),
                   scale = none[float]()): Viewport =
  ## convenience init function for Viewport using relative coordinates
  let origin = Coord(x: Coord1D(pos: left, kind: ckRelative),
                     y: Coord1D(pos: bottom, kind: ckRelative))
  let
    widthCoord = Coord1D(pos: width, kind: ckRelative)
    heightCoord = Coord1D(pos: height, kind: ckRelative)
  result = initViewport(origin, widthCoord, heightCoord,
                        style, xScale, yScale, rotate, scale)

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

proc convertToKind(c: Coord1D, toKind: Coord1D): Coord1D =
  ## converts the coordinate `c` to the kind of `toKind`
  case toKind.kind
  of ckRelative:
    result = c.toRelative
  of ckAbsolute:
    result = c.to(ckAbsolute,
                  absLength = some(toKind.length))
  of ckData:
    result = c.to(ckData,
                  datScale = some(toKind.scale))
  else:
    raise newException(Exception, "convertToKind not implemented for " & $toKind.kind)

proc translate(c: Coord1D, byCoord: Coord1D): Coord1D =
  ## translates the coordinate `c` by `byCoord`
  let pos = c.toRelative.pos + byCoord.toRelative.pos
  result = Coord1D(pos: pos, kind: ckRelative)
  result = result.convertToKind(c)

proc translate(c: Coord, byCoord: Coord): Coord =
  result = Coord(x: c.x.translate(byCoord.x),
                 y: c.y.translate(byCoord.y),
                 kind: c.kind)

proc embedInto(c: Coord, view: Viewport): Coord =
  ## embeds the coordinate `c` into `Viewport`
  result = Coord(x: Coord1D(pos: left(view) + c.x.toRelative.pos * width(view),
                            kind: ckRelative),
                 y: Coord1D(pos: bottom(view) + c.y.toRelative.pos * height(view),
                            kind: ckRelative),
                 kind: ckRelative)
  #result = result.convertToKind(c)

proc embedInto(view: Viewport, into: Viewport): Viewport =
  ## embeds the given `view` into the `into` Viewport by embedding
  ## the (left, bottom) coordinates and scaling the width / height
  result = view
  result.origin = result.origin.embedInto(into)
  result.width = Coord1D(pos: result.width.toRelative.pos * into.width.toRelative.pos,
                         kind: ckRelative)
  result.height = Coord1D(pos: result.height.toRelative.pos * into.height.toRelative.pos,
                          kind: ckRelative)

proc point(c: Coord): Point =
  ## converts the given coordinate to `ckRelative` and returns the position as a
  ## `Point`
  #result = (x: c.x.toRelative.pos, y: c.y.toRelative.pos)
  result = (x: c.x.pos, y: c.y.pos)

proc initAxis(view: Viewport,
              kind: AxisKind,
              width = 1.0,
              color = color(0.0, 0.0, 0.0)): GraphObject =
  var axis = GraphObject(kind: goAxis,
                         axWidth: width,
                         axStart: (0.0, 1.0),
                         axStop: (1.0, 1.0),
                         style: Style(color: color,
                                      lineWidth: width))
  case kind
  of akX:
    result = axis
  of akY:
    result = replace(axis):
      axStart = (0.0, 0.0)
      axStop = (0.0, 1.0)

proc initRect(view: Viewport,
              origin: Coord,
              width, height: Coord1D,
              color = color(0.0, 0.0, 0.0),
              style = none[Style]()): GraphObject =
  result = GraphObject(kind: goRect,
                       reOrigin: origin,
                       reWidth: width,
                       reHeight: height)
  if style.isSome:
    result.style = style.get()
  else:
    result.style = Style(lineWidth: 0.0,
                         color: color(0.0, 0.0, 0.0, 0.0),
                         size: 0.0,
                         fillColor: color)

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
  #                     GraphObject(kind: goRect,
  #                     reLeft: left,
  #                     reBottom: bottom,
  #                     reWidth: width,
  #                     reHeight: height,
  #                     style: Style(fillColor: color))

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

proc drawAxis(img: BImage, gobj: GraphObject) =
  doAssert gobj.kind == goAxis, "object must be a `goAxis`!"
  echo "Drawing to ", gobj.axStart, " to ", gobj.axStop
  img.drawLine(gobj.axStart, gobj.axStop,
               gobj.style.lineWidth,
               gobj.style.color,
               gobj.rotate)

proc drawRect(img: BImage, gobj: GraphObject) =
  doAssert gobj.kind == goRect, "object must be a `goRect`!"
  img.drawRectangle(gobj.reOrigin.point.x, gobj.reOrigin.point.y,
                    gobj.reWidth.pos, gobj.reHeight.pos,
                    gobj.style,
                    gobj.rotate)

proc drawPoint(img: BImage, gobj: GraphObject) =
  doAssert gobj.kind == goPoint, "object must be a `goPoint`!"
  case gobj.ptMarker
  of mkCircle:
    #echo "Pos at ", gobj.ptPos.point
    img.drawCircle(gobj.ptPos.point, gobj.ptSize, lineWidth = 0.0,
                   strokeColor = color(0.0, 0.0, 0.0, 0.0),
                   fillColor = gobj.ptColor,
                   rotateAngle = gobj.rotate)
  of mkCross:
    img.drawLine((gobj.ptPos.point.x - gobj.ptSize / 2.0, gobj.ptPos.point.y),
                 (gobj.ptPos.point.x + gobj.ptSize / 2.0, gobj.ptPos.point.y),
                 gobj.ptSize / 4.0,
                 gobj.ptColor,
                 rotateAngle = gobj.rotate)
    img.drawLine((gobj.ptPos.point.x, gobj.ptPos.point.y - gobj.ptSize / 2.0),
                 (gobj.ptPos.point.x, gobj.ptPos.point.y + gobj.ptSize / 2.0),
                 gobj.ptSize / 4.0,
                 gobj.ptColor,
                 rotateAngle = gobj.rotate)
  else:
    raise newException(Exception, "Not implemented yet!")

proc scale[T: SomeNumber](p: Point, width, height: T): Point =
  result = (p.x * width.float,
            p.y * height.float)

proc toGlobalCoords(gobj: GraphObject, img: BImage): GraphObject =
  result = gobj
  case gobj.kind
  of goAxis:
    result.axStart = result.axStart.scale(img.width, img.height)
    result.axStop = result.axStop.scale(img.width, img.height)
  of goRect:
    #let nPos = (result.reLeft, result.reBottom).scale(img.width, img.height)
    #result.reLeft = nPos[0]
    #result.reBottom = nPos[1]
    #result.reWidth = result.reWidth * img.width.float
    #result.reHeight = result.reHeight * img.height.float
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
    #echo "Point at ", result.ptPos
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

proc translate(p: Point, view: Viewport): Point =
  result = (left(view) + p.x * (width(view)),
            bottom(view) + p.y * (height(view)))

proc transform(gobj: GraphObject, view: Viewport): GraphObject =
  result = gobj
  case gobj.kind
  of goAxis:
    result.axStart = result.axStart.translate(view)
    result.axStop = result.axStop.translate(view)
  of goRect:
    result.reOrigin = gobj.reOrigin.embedInto(view)
    result.reWidth = Coord1D(pos: result.reWidth.toRelative.pos * view.width.toRelative.pos,
                             kind: ckRelative)
    result.reHeight = Coord1D(pos: result.reHeight.toRelative.pos * view.height.toRelative.pos,
                            kind: ckRelative)
  of goPoint:
    #let pos = result.ptPos.translate(view)
    result.ptPos = gobj.ptPos.embedInto(view)
    #discard
    #let coord = result.ptPos.to(ckAbsolute,
    #absWidth = some(width(view)),
    #                                absHeight = some(height(view)))
    #result.ptPos = coord
    #echo "PtPos ", result.ptPos
    #result.ptPos = Coord(x: Coord1D(pos: pos.x, kind: ckRelative),
    #                     y: Coord1D(pos: pos.y, kind: ckRelative),
    #                     kind: ckRelative)
  else:
    raise newException(Exception, "transform not implemented yet!")

proc draw*(img: BImage, view: Viewport) =
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
    if view.rotate.isSome:
      mobj.rotate = some((view.rotate.get,
                          (centerX, centerY).scale(img.width, img.height)))
    img.draw(mobj.transform(view))

  # draw all children viewports
  for chView in view:
    var mchView = chView.embedInto(view)
    echo "\n\n\n"
    echo mchView.origin
    echo mchView.width
    mchView.rotate = view.rotate
    img.draw(mchView)

when isMainModule:

  import seqmath, sequtils


  block:
    var img = initBImage("testView.pdf",
                         width = 600, height = 400,
                         backend = bkCairo,
                         ftype = fkPdf)

    var view1 = initViewport(left = 0.1,
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
    let x = linspace(0.0, 6.28, 1_000_000)
    let y = x.mapIt(sin(it))
    let points = zip(x, y)
    var gobjPoints: seq[GraphObject]
    for p in points:
      gobjPoints.add initPoint(view2, (x: p.a, y: p.b),
                               marker = mkCross)


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

    #view1.rotate = some(30.0)
    view1.objects = concat(@[line1, line2, rect])
    view2.objects = concat(@[rect2], gobjPoints)
    view1.children.add view2
    img.draw(view1)
    #for i in 0 ..< 3:
    #  view1.rotate = some(i.float * 10.0)
    #  img.draw(view1)
    img.destroy()


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
                                                 fillColor: color(0.0, 0.0, 0.0, 0.5))))
    view2.objects = @[rect,
                      initPoint(view2, (x: 0.0, y: 0.0)),
                      initPoint(view2, (x: 1.0, y: 0.0)),
                      initPoint(view2, (x: 0.0, y: 1.0)),
                      initPoint(view2, (x: 1.0, y: 1.0))]
    view1.children.add view2
    img.draw(view1)
    img.destroy()


## gogLayer
## implements the prototype GoG layer
