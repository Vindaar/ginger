import chroma
import options

when not defined(noCairo):
  import cairo
else:
  type
    TSurface = object
      discard
    PSurface = ptr TSurface

    TTextExtents = object
      x_bearing*: float64
      y_bearing*: float64
      width*: float64
      height*: float64
      x_advance*: float64
      y_advance*: float64

    TPattern = object
      discard
    PPattern = ptr TPattern

    TContext = object
      discard
    PContext = ptr TContext

type
  BackendKind* = enum
    bkCairo, bkVega, bkTikZ
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

  BImage* = object
    fname*: string
    width*: int
    height*: int
    ftype*: FileTypeKind
    case backend*: BackendKind
    of bkCairo:
      cCanvas*: PSurface
      ctx*: PContext
      created*: bool # if surface was created
    of bkTikZ:
      data*: string # stores the TikZ commands as a string to be inserted into a LaTeX template
      options*: TexOptions
    of bkVega: discard

  LineType* = enum
    ltNone, ltSolid, ltDashed, ltDotted, ltDotDash, ltLongDash, ltTwoDash

  ErrorBarKind* = enum
    ebLines, # simple lines extending the error
    ebLinesT # lines with an orthogonal line at ends, like a `T`

  TextAlignKind* = enum
    taLeft, taCenter, taRight

  FontSlant* = enum
    fsNormal, fsItalic, fsOblique

  #FontFamily = enum
  #  ffSerif, ffSans
  # helper object to store text extent information
  TextExtent* = TTextExtents

  Font* = object
    family*: string # serif, sans-serif...
    size*: float
    bold*: bool
    slant*: FontSlant
    color*: Color
    alignKind*: TextAlignKind

  MarkerKind* = enum
    mkCircle, mkCross, mkRotCross, mkStar

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
