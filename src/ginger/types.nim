import cairo
import chroma

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

  TextAlignKind* = enum
    taLeft, taCenter, taRight

  FontSlant* = enum
    fsNormal, fsItalic, fsOblique

  #FontFamily = enum
  #  ffSerif, ffSans

  Font* = object
    family*: string # serif, sans-serif...
    size*: float
    bold*: bool
    slant*: FontSlant
    color*: Color

  Style* = object
    color*: Color
    size*: float
    lineType*: LineType
    lineWidth*: float
    fillColor*: Color
