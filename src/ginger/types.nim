from std / random import Rand
import chroma
import options

## CT variables adjustable via command line arguments (or nim.cfg / config.nims) to (de-)activate
## different backends
const useCairo* {.booldefine.} = true
const useTikZ* {.booldefine.}  = true
const usePixie* {.booldefine.} = false

type
  BackendKind* = enum
    bkNone, bkDummy, bkCairo, bkVega, bkTikZ, bkPixie
  FiletypeKind* = enum
    fkSvg, fkPng, fkPdf, fkVega, fkTeX

  Point* = tuple[x, y: float]
  IPoint* = tuple[x, y: int]

  AxisKind* = enum
    akX, akY

  TeXOptions* = object
    useTeX*: bool                # arguments only used if true
    texTemplate*: Option[string] # a custom user TeX template
    standalone*: bool            # if true output to a standalone TeX document
    onlyTikZ*: bool              # if true write ``only`` TikZ commands to file
    caption*: Option[string]     # optional caption for the figure
    label*: Option[string]       # optional label for the figure
    placement*: string           # placement option for the figure env ("htbp", ...)

  BImage*[T] = object
    fname*: string
    width*: int
    height*: int
    ftype*: FileTypeKind
    backend*: T
    rnd*: Rand ## Random number generator to produce filenames if `dataAsBitmap` or raster used

  DummyBackend* = object

  LineType* = enum
    ltNone, ltSolid, ltDashed, ltDotted, ltDotDash, ltLongDash, ltTwoDash

  ErrorBarKind* = enum
    ebLines, # simple lines extending the error
    ebLinesT # lines with an orthogonal line at ends, like a `T`

  TextAlignKind* = enum
    taLeft, taCenter, taRight

  FontSlant* = enum
    fsNormal, fsItalic, fsOblique

  Font* = object
    family*: string # serif, sans-serif...
    size*: float
    bold*: bool
    slant*: FontSlant
    color*: Color
    alignKind*: TextAlignKind

  MarkerKind* = enum
    mkCircle, mkCross, mkTriangle, mkRhombus, mkRectangle, mkRotCross, mkUpsideDownTriangle,
    mkEmptyCircle, mkEmptyRectangle, mkEmptyRhombus

  Gradient* = object
    colors*: seq[Color]
    rotation*: float

  Style* = object
    color*: Color
    size*: float
    lineType*: LineType
    lineWidth*: float
    fillColor*: Color
    marker*: MarkerKind
    errorBarKind*: ErrorBarKind
    gradient*: Option[Gradient] # overrides `color`
    font*: Font

  # Following are the graphing related types mainly used in `ginger.nim`
  GraphObjectKind* = enum
    goAxis, # a plot axis
    goText, # general text
    goLabel, # an axis label
    goTick, # an axis tick
    goTickLabel, # a tick label (i.e. the number or text)
    goPoint, # a general point
    goManyPoints, # used if many points of same style
    goLine, # a general line
    goRect, # a general rectangle
    goGrid, # the plot grid (lines along the ticks)
    goPolyLine, # a line connecting several points
    goRaster, # pixel based raster data
    goComposite # an object consisting of several other GraphObjects

  CompositeKind* = enum
    cmpErrorBar # an error bar consisting of potentially several lines

  TickKind* = enum
    tkOneSide, # only outside the plot
    tkBothSides # inside and outside the plot

  GraphObject* = ref GraphObjectObj
  GraphObjectObj* = object
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
      gdOrigin*: Coord     # Coordinate of origin of plot viewport
      gdOriginDiag*: Coord # and corner diagonal along viewport
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
    of goManyPoints:
      ptsMarker*: MarkerKind
      ptsSize*: float
      ptsColor*: Color
      ptsPos*: seq[Coord] # seq coord for now, but may become a seq[Point]
      ## If we add a `drawCircles` in the future, this could be useful
      # ptsRelPos*: seq[Point] # stores the final positions in global space after embedding & converting
    of goPolyLine:
      plPos*: seq[Coord]
    of goRect:
      reOrigin*: Coord
      reWidth*: Quantity
      reHeight*: Quantity
    of goRaster: ## XXX: add option to somehow allow ggplotnim to store any plot as raster if too many
                 ## objects are contained in it. Maybe add a filename argument that will instead be
                 ## used? In ggplotnim could check if df.len > N, then produce PNG of only the
                 ## plot viewport and then hand that as a raster?
      # raw pixel raster object, used for high pixel density heatmaps etc.
      rstOrigin*: Coord
      rstPixWidth*: Quantity
      rstPixHeight*: Quantity
      # The number of same colored blocks along each direction
      # e.g. heatmap of 20x20 elements will be drawn to rstPixWidth * rstPixHeight
      # pixel bitmap (e.g. 500x500 pixels). Need information to calculate size of
      # each block
      rstBlockX*: int
      rstBlockY*: int
      # should the callback take pixels and return pixels?
      rstDrawCb*: proc(): seq[uint32] # callback which fills the pix width / height
                                      # with raw RGB pixels
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

  # TODO: should axis be a global field after all?
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
      fType*: FileTypeKind # need to render the text on the correct file type backend!
      backend*: BackendKind # string based values are only defined given a backend
      includeBearing*: bool
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
    fType*: FileTypeKind
    dataAsBitmap*: bool ## If true will write the data viewport that contains the actual ggplotnim
                        ## data as a bitmap. Mainly makes sense for vector graphic targets


when useCairo and not defined(noCairo):
  import cairo
  # Note: this can be defined here, as we define all Cairo types locally if `noCairo` is defined
  # as dummies
  type
    CairoBackend* = object
      cCanvas*: PSurface
      ctx*: PContext
      created*: bool # if surface was created
    # helper object to store text extent information
    TextExtent* = TTextExtents
else:
  # define for other backends
  type
    TTextExtents* = object
      x_bearing*: float64
      y_bearing*: float64
      width*: float64
      height*: float64
      x_advance*: float64
      y_advance*: float64
    TextExtent* = TTextExtents

when usePixie:
  import pixie
  type
    PixieBackend* = object
      pxContext*: pixie.Context

when useTikZ:
  # TikZ backend type does not involve any external dependencies by itself (backend does though!)
  type
    TikZBackend* = object
      data*: string # stores the TikZ commands as a string to be inserted into a LaTeX template
      header*: string # Additional TeX code to be inserted after the header of `\usepackage` calls
      bodyHeader*: string # Additional TeX code to be inserted at beginning of `\begin{document}`
      options*: TeXOptions
      lastColor*: string # stores the last used color to avoid redefining same color

from std / sequtils import mapIt
proc clone*(g: GraphObject): GraphObject =
  ## Clones the given graph object
  result = GraphObject(name: g.name,
                       children: g.children.mapIt(it.clone()),
                       rotateInView: g.rotateInView,
                       rotate: g.rotate,
                       kind: g.kind)
  case g.kind
  of goLine, goAxis:
    result.lnStart = g.lnStart
    result.lnStop = g.lnStop
  of goLabel, goText, goTickLabel:
    result.txtText = g.txtText
    result.txtFont = g.txtFont
    result.txtAlign = g.txtAlign
    result.txtRotate = g.txtRotate
  of goGrid:
    result.gdOrigin = g.gdOrigin
    result.gdOriginDiag = g.gdOriginDiag
    result.gdXPos = g.gdXPos
    result.gdYPos = g.gdYpos
  of goTick:
    result.tkMajor = g.tkMajor
    result.tkPos = g.tkPos
    result.tkAxis = g.tkAxis
    result.tkKind = g.tkKind
    result.tkSecondary = g.tkSecondary
  of goPoint:
    result.ptMarker = g.ptMarker
    result.ptSize = g.ptSize
    result.ptColor = g.ptColor
    result.ptPos = g.ptPos
  of goManyPoints:
    result.ptsMarker = g.ptsMarker
    result.ptsSize = g.ptsSize
    result.ptsColor = g.ptsColor
    result.ptsPos = g.ptsPos
  of goPolyLine:
    result.plPos = g.plPos
  of goRect:
    result.reOrigin = g.reOrigin
    result.reWidth = g.reWidth
    result.reHeight = g.reHeight
  of goRaster:
    result.rstOrigin = g.rstOrigin
    result.rstPixWidth = g.rstPixWidth
    result.rstPixHeight = g.rstPixHeight
    result.rstBlockX = g.rstBlockX
    result.rstBlockY = g.rstBlockY
    result.rstDrawCb = g.rstDrawCb
  of goComposite:
    result.cmpKind = g.cmpKind

proc clone*(v: Viewport): Viewport =
  ## Clones the given Viewport
  result = new Viewport
  result.name = v.name
  result.parent = v.parent
  result.style = v.style
  result.xScale = v.xScale
  result.yScale = v.yScale
  result.rotate = v.rotate
  result.scale = v.scale
  result.origin = v.origin
  result.width = v.width
  result.height = v.height
  result.objects = newSeq[GraphObject](v.objects.len)
  for i, o in v.objects:
    result.objects[i] = o.clone()
  result.wView = v.wView
  result.hView = v.hView
  result.wImg = v.wImg
  result.hImg = v.hImg
  result.backend = v.backend
  result.fType = v.fType
  result.children = newSeq[Viewport](v.children.len)
  for i, o in v.children:
    result.children[i] = o.clone()
