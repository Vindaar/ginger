import chroma

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

type
  BackendKind* = enum
    bkCairo, bkVega
  FiletypeKind* = enum
    fkSvg, fkPng, fkPdf, fkVega

  Point* = tuple[x, y: float]
  IPoint* = tuple[x, y: int]

  AxisKind* = enum
    akX, akY

  BImage* = object
    fname*: string
    width*: int
    height*: int
    case backend*: BackendKind
    of bkCairo:
      cCanvas*: PSurface
      ftype*: FileTypeKind
    of bkVega:
      discard

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

  MarkerKind* = enum
    mkCircle, mkCross, mkRotCross, mkStar

  Style* = object
    color*: Color
    size*: float
    lineType*: LineType
    lineWidth*: float
    fillColor*: Color
    marker*: MarkerKind
    errorBarKind*: ErrorBarKind
