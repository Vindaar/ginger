import cairo
import chroma
import math
import types
import options

template withSurface*(surface: PSurface, name: untyped, actions: untyped): untyped =
  var `name` {.inject.} = create(surface)
  actions
  `name`.destroy()

template withSurface*(surface: PSurface, actions: untyped): untyped =
  surface.withSurface(ctx):
    actions

template withCairo(img: BImage, name: untyped, actions: untyped): untyped =
  var `name` {.inject.} = image_surface_create(FORMAT_ARGB32, img.width, img.height)
  `name`.withSurface:
    actions
  `name`.destroy()

template withCairo(img: BImage, actions: untyped): untyped =
  img.withCairo(surface):
    actions

template rotate(ctx: PContext, angle: float, around: Point): untyped =
  ctx.translate(around[0], around[1])
  ctx.rotate(angle * PI / 180.0)
  ctx.translate(-around[0], -around[1])

proc drawLine*(img: BImage, start, stop: Point,
               lineWidth: float = 2.0,
               color: Color = color(0.0, 1.0, 0.0),
               rotateAngle: Option[(float, Point)] = none[(float, Point)]()) =
  img.cCanvas.withSurface:
    if rotateAngle.isSome:
      let rotAngTup = rotateAngle.get
      ctx.rotate(rotAngTup[0], rotAngTup[1])
    ctx.set_source_rgba(color.r, color.g, color.b, color.a)
    ctx.set_line_width(lineWidth)
    ctx.move_to(start.x, start.y)
    ctx.line_to(stop.x, stop.y)
    ctx.stroke()

proc drawCircle*(img: BImage, center: Point, radius: float,
                 lineWidth: float,
                 strokeColor = color(0.0, 0.0, 0.0),
                 fillColor = color(0.0, 0.0, 0.0, 0.0),
                 rotateAngle: Option[(float, Point)] = none[(float, Point)]()) =
  img.cCanvas.withSurface:
    if rotateAngle.isSome:
      let rotAngTup = rotateAngle.get
      ctx.rotate(rotAngTup[0], rotAngTup[1])
    ctx.set_line_width(lineWidth)
    ctx.set_source_rgba(strokeColor.r, strokeColor.g, strokeColor.b, strokeColor.a)
    ctx.arc(center.x, center.y, radius, 0.0, 2 * PI)
    ctx.stroke_preserve()
    ctx.set_source_rgba(fillColor.r, fillColor.g, fillColor.b, fillColor.a)
    ctx.fill()

func toCairoFontSlant(font: Font): TFontSlant =
  case font.slant
  of fsNormal:
    result = FONT_SLANT_NORMAL
  of fsItalic:
    result = FONT_SLANT_ITALIC
  of fsOblique:
    result = FONT_SLANT_OBLIQUE

func toCairoFontWeight(font: Font): TFontWeight =
  case font.bold
  of true:
    result = FONT_WEIGHT_BOLD
  of false:
    result = FONT_WEIGHT_NORMAL

proc drawText*(img: BImage, text: string, font: Font, at: Point,
               rotateAngle: Option[(float, Point)] = none[(float, Point)]()) =
  # NOTE: with text_extents we can center the text too, see:
  # https://www.cairographics.org/samples/text_align_center/
  img.cCanvas.withSurface:
    if rotateAngle.isSome:
      let rotAngTup = rotateAngle.get
      ctx.rotate(rotAngTup[0], rotAngTup[1])
    ctx.select_font_face(font.family, font.toCairoFontSlant, font.toCairoFontWeight)
    ctx.set_font_size(font.size)
    ctx.set_source_rgba(font.color.r, font.color.g, font.color.b, font.color.a)
    ctx.move_to(at.x, at.y)
    ctx.show_text(text)

proc drawRectangle*(img: BImage, left, bottom, width, height: float,
                    style: Style,
                    rotateAngle: Option[(float, Point),] = none[(float, Point)]()) =
  ## draws a rectangle on the image
  img.cCanvas.withSurface:
    if rotateAngle.isSome:
      let rotAngTup = rotateAngle.get
      ctx.rotate(rotAngTup[0], rotAngTup[1])
    ctx.rectangle(left, bottom, width, height)
    ctx.set_line_width(style.lineWidth)
    ctx.set_source_rgba(style.color.r, style.color.g, style.color.b, style.color.a)
    ctx.stroke_preserve()
    ctx.set_source_rgba(style.fillColor.r, style.fillColor.g, style.fillColor.b, style.fillColor.a)
    ctx.fill()

proc initBImage*(filename: string,
                 width, height: int,
                 backend: BackendKind,
                 fType: FiletypeKind): BImage =
  case backend
  of bkCairo:
    var surface: PSurface
    case fType:
    of fkPng:
      surface = image_surface_create(FORMAT_ARGB32, width.int32, height.int32)
    of fkSvg:
      surface = svg_surface_create(filename, width.float, height.float)
    of fkPdf:
      surface = pdf_surface_create(filename, width.float, height.float)
    else:
      raise newException(Exception, "Unsupported fType " & $fType & " in `initBImage")
    result = BImage(fname: filename,
                    backend: bkCairo,
                    cCanvas: surface,
                    width: width,
                    height: height,
                    fType: fType)
  of bkVega:
    discard

#proc save(img: BImage) =
#  case img.kind
#  of bkCairo:
#    let err = img.cCanvas.write_to_png(




when isMainModule:
  # raw Cairo code
  var surface = image_surface_create(FORMAT_ARGB32, 600, 400)
  var ctx = create(surface)
  ctx.select_font_face("serif", FONT_SLANT_NORMAL, FONT_WEIGHT_BOLD)
  ctx.set_font_size(32.0)
  ctx.set_source_rgb(0.0, 0.0, 1.0)
  ctx.move_to(100.0, 300.0)
  ctx.show_text("Hello from Nim!")

  ctx.destroy()
  discard surface.write_to_png("testHello.png")
  surface.destroy()
