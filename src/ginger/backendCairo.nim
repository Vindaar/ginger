import cairo
import chroma
import math
import types
import options

template withSurface*(img: var BImage[CairoBackend], name: untyped, actions: untyped): untyped =
  if not img.backend.created:
    img.backend.ctx = create(img.backend.cCanvas)
    img.backend.created = true
  template ctx(): untyped = img.backend.ctx
  # save current context
  ctx.save()
  actions
  # restore last context, so that next call starts from default
  ctx.restore()
  when false:
    # this is equivalent code, which creates a new context for each
    # call.
    var `name` {.inject.} = create(img.cCanvas)
    actions
    `name`.destroy()

template withSurface*(img: var BImage[CairoBackend], actions: untyped): untyped =
  img.withSurface(ctx):
    actions

when false:
  template withCairo(img: BImage[CairoBackend], name: untyped, actions: untyped): untyped =
    var `name` {.inject.} = image_surface_create(FORMAT_ARGB32, img.width, img.height)
    `name`.withSurface:
      actions
    `name`.destroy()

  template withCairo(img: BImage[CairoBackend], actions: untyped): untyped =
    img.withCairo(surface):
      actions

template rotate(ctx: PContext, angle: float, around: Point): untyped =
  ctx.translate(around[0], around[1])
  ctx.rotate(angle * PI / 180.0)
  ctx.translate(-around[0], -around[1])

func getLineStyle(lineType: LineType, lineWidth: float): seq[float] =
  template dash: untyped = lineWidth * 4.0
  template dashSpace: untyped = lineWidth * 5.0
  template dot: untyped = lineWidth / 2.0
  template dotSpace: untyped = lineWidth * 2.0
  template longDash: untyped = lineWidth * 8.0

  case lineType
  of ltDashed:
    result = @[dash(), dashSpace()]
  of ltDotted:
    result = @[dot(), dotSpace()]
  of ltDotDash:
    result = @[dot(), dotSpace(), dash(), dotSpace()]
  of ltLongDash:
    result = @[longDash(), dashSpace()]
  of ltTwoDash:
    result = @[dash(), dotSpace() * 2.0, longDash(), dotSpace() * 2.0]
  else: discard

func setLineStyle(ctx: PContext, lineType: LineType, lineWidth: float) =
  case lineType
  of ltDashed .. ltTwoDash:
    let lineStyle = getLineStyle(lineType, lineWidth)
    ctx.set_dash(lineStyle, lineStyle.len.float)
    ctx.set_line_cap(LINE_CAP_ROUND)
  of ltNone:
    # achieve no line, by setting line width to 0
    ctx.set_line_width(0.0)
  else: discard

proc drawLine*(img: var BImage[CairoBackend], start, stop: Point,
               style: Style,
               rotateAngle: Option[(float, Point)] = none[(float, Point)]()) =
  img.withSurface:
    if rotateAngle.isSome:
      let rotAngTup = rotateAngle.get
      ctx.rotate(rotAngTup[0], rotAngTup[1])
    ctx.set_source_rgba(style.color.r, style.color.g, style.color.b, style.color.a)
    ctx.setLineStyle(style.lineType, style.lineWidth)
    ctx.set_line_width(style.lineWidth)
    ctx.move_to(start.x, start.y)
    ctx.line_to(stop.x, stop.y)
    ctx.stroke()

proc drawPolyLine*(img: var BImage[CairoBackend], points: seq[Point],
                   style: Style,
                   rotateAngle: Option[(float, Point)] = none[(float, Point)]()) =
  img.withSurface:
    if rotateAngle.isSome:
      let rotAngTup = rotateAngle.get
      ctx.rotate(rotAngTup[0], rotAngTup[1])
    ctx.set_source_rgba(style.color.r, style.color.g, style.color.b, style.color.a)
    ctx.setLineStyle(style.lineType, style.lineWidth)
    ctx.set_line_width(style.lineWidth)
    let p0 = points[0]
    ctx.move_to(p0.x, p0.y)
    for i in 1 .. points.high:
      ctx.line_to(points[i].x, points[i].y)
    # now stroke the path we created
    ctx.stroke_preserve()
    #ctx.close_path()
    # and fill the created path if desired
    ctx.set_source_rgba(style.fillColor.r, style.fillColor.g, style.fillColor.b,
                        style.fillColor.a)
    ctx.fill()

proc drawCircle*(img: var BImage[CairoBackend], center: Point, radius: float,
                 lineWidth: float,
                 strokeColor = color(0.0, 0.0, 0.0),
                 fillColor = color(0.0, 0.0, 0.0, 0.0),
                 rotateAngle: Option[(float, Point)] = none[(float, Point)]()) =
  img.withSurface:
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

proc getTextExtent*(ctx: PContext, text: string): TextExtent =
  ## convenience wrapper around `text_extents` so user does not have to
  ## bother with pointers
  ctx.text_extents(text, addr result)

from std / os import getTempDir, `/`
proc getTextExtent*(_: typedesc[CairoBackend], fType: FileTypeKind, text: string, font: Font): TextExtent =
  ## creates a temporary cairo surface and evaluates the given string `text`
  ## under the given `font` for the text extent.
  # create small surface as user space to evaluate on
  let width = text.len.float * font.size * 2.0
  let height = font.size * 2.0
  var surface: PSurface
  # Create the correct surface for the file type. This is important to get precise text size
  # information for that backend!
  case fType
  of fkPng: surface = image_surface_create(FORMAT_ARGB32, width.int32, height.int32)
  of fkSvg: surface = svg_surface_create(getTempDir() / "text_extent_surface.svg", width.float, height.float)
  of fkPdf: surface = pdf_surface_create(getTempDir() / "text_extent_surface.pdf", width.float, height.float)
  else:
    raise newException(ValueError, "Invalid file type for the Cairo backend: " & $fType)
  var ctx = create(surface)
  ctx.select_font_face(font.family, font.toCairoFontSlant, font.toCairoFontWeight)
  ctx.set_font_size(font.size)
  ctx.set_source_rgba(font.color.r, font.color.g, font.color.b, font.color.a)
  result = ctx.getTextExtent(text)
  ctx.destroy()
  surface.destroy()

proc drawText*(img: var BImage[CairoBackend], text: string, font: Font, at: Point,
               alignKind: TextAlignKind = taLeft,
               rotate: Option[float] = none[float](),
               rotateInView: Option[(float, Point)] = none[(float, Point)]()) =
  # NOTE: with text_extents we can center the text too, see:
  # https://www.cairographics.org/samples/text_align_center/
  img.withSurface:
    if rotateInView.isSome:
      let rotAngTup = rotateInView.get
      ctx.rotate(rotAngTup[0], rotAngTup[1])
    ctx.select_font_face(font.family, font.toCairoFontSlant, font.toCairoFontWeight)
    ctx.set_font_size(font.size)
    ctx.set_source_rgba(font.color.r, font.color.g, font.color.b, font.color.a)
    var
      x = at.x
      y = at.y

    let extents = ctx.getTextExtent(text)
    # potentially rotate around specific point (location depends on where we align)
    if rotate.isSome:
      let
        rotAtX = at.x
        rotAtY = at.y# - (extents.height / 2.0 + extents.y_bearing)
      ctx.rotate(rotate.get(), (rotAtX, rotAtY))
    case alignKind
    of taLeft:
      x = at.x - extents.x_bearing
      y = at.y - (extents.height / 2.0 + extents.y_bearing)
    of taCenter:
      x = at.x - (extents.width / 2.0 + extents.x_bearing)
      y = at.y - (extents.height / 2.0 + extents.y_bearing)
    of taRight:
      x = at.x - (extents.width + extents.x_bearing)
      y = at.y - (extents.height / 2.0 + extents.y_bearing)

    ctx.move_to(x, y)
    ctx.show_text(text)

proc createGradient(gradient: Gradient,
                    left, bottom, width, height: float): PPattern =
  let middle = bottom + height / 2.0
  let right = left + width
  let center = left + width / 2.0

  #result = pattern_create_linear(left, middle, left + width, middle)
  result = pattern_create_linear(center, bottom + height, center, bottom)
  let stepSize = width / gradient.colors.len.float
  let numColors = gradient.colors.len.float
  for i, c in gradient.colors:
    result.add_color_stop_rgb(i.float / numColors, c.r, c.g, c.b)

proc drawRectangle*(img: var BImage[CairoBackend], left, bottom, width, height: float,
                    style: Style,
                    rotate: Option[float] = none[float](),
                    rotateInView: Option[(float, Point),] = none[(float, Point)]()) =
  ## draws a rectangle on the image
  img.withSurface:
    if rotateInView.isSome:
      # possible rotation of viewport
      let rotAngTup = rotateInView.get
      ctx.rotate(rotAngTup[0], rotAngTup[1])
    if rotate.isSome:
      # possible rotation desired for rectangle
      let
        rotAtX = left
        rotAtY = bottom
      ctx.rotate(rotate.get(), (rotAtX, rotAtY))
    ctx.rectangle(left, bottom, width, height)
    ctx.set_line_width(style.lineWidth)
    ctx.setLineStyle(style.lineType, style.lineWidth)
    ctx.set_source_rgba(style.color.r, style.color.g, style.color.b, style.color.a)
    ctx.stroke_preserve()
    if style.gradient.isSome:
      var pat = createGradient(style.gradient.unsafeGet,
                               left = left,
                               bottom = bottom,
                               width = width,
                               height = height)
      ctx.set_source(pat)
      destroy(pat)
    else:
      ctx.set_source_rgba(style.fillColor.r, style.fillColor.g, style.fillColor.b, style.fillColor.a)
    ctx.fill()

proc drawRaster*(img: var BImage[CairoBackend], left, bottom, width, height: float,
                 numX, numY: int,
                 drawCb: proc(): seq[uint32],
                 rotate: Option[float] = none[float](),
                 rotateInView: Option[(float, Point),] = none[(float, Point)]()) =
  ## draws a rectangle on the image
  img.withSurface:
    if rotateInView.isSome:
      # possible rotation of viewport
      let rotAngTup = rotateInView.get
      ctx.rotate(rotAngTup[0], rotAngTup[1])
    if rotate.isSome:
      # possible rotation desired for rectangle
      let
        rotAtX = left
        rotAtY = bottom
      ctx.rotate(rotate.get(), (rotAtX, rotAtY))

    let
      width = abs(width)
      height = abs(height)
    # we *truncate* the width / height here. Better one pixel short than too much
    let wImg = width.floor.int32
    let hImg = height.floor.int32
    var pngSurface = imageSurfaceCreate(FORMAT_ARGB32, wImg, hImg)

    pngSurface.flush()
    # get the raw data of the surface and draw with the callback
    var data = cast[ptr UncheckedArray[uint32]](getData(pngSurface))
    let toDraw = drawCb()
    # each tile must be drawn with `blockSize` pixels
    let blockSizeX = width / numX.float
    let blockSizeY = height / numY.float
    for y in 0 ..< hImg:
      for x in 0 ..< wImg:
        var tX = (x.float / blockSizeX).floor.int
        var tY = (y.float / blockSizeY).floor.int
        var cVal = toDraw[tY * numX + tX]
        let α = 0xFF'u32 and (cVal shr 24)
        if α != 0xFF'u32 and cVal > 0'u32: # If non-trival alpha & a color, need to pre-multiply all colors!
          # get individual channels
          let r = 0x00FF'u32 and (cVal shr 16)
          let g = 0x0000FF'u32 and (cVal shr 8)
          let b = 0x000000FF'u32 and cVal
          # pre-multiply and reassemble
          cVal = (α shl 24) or
                 (r * α div 255 shl 16) or
                 (g * α div 255 shl 8) or
                 (b * α div 255)
        data[y * wImg + x] = cVal
    pngSurface.markDirty()
    # apply the new surface to the image surface
    ctx.set_source(pngSurface, left, bottom)
    ctx.paint()
    pngSurface.destroy()

proc insertRaster*(img: var BImage[CairoBackend], tmpName: string, left, bottom, width, height: float) =
  raise newException(Defect, "`insertRaster` not implemenetd for Cairo backend yet.")

proc initBImage*(_: typedesc[CairoBackend],
                 filename: string,
                 width, height: int,
                 fType: FiletypeKind,
                 texOptions = TeXOptions()
                ): BImage[CairoBackend] =
  var surface: PSurface
  case fType:
  of fkPng:
    surface = image_surface_create(FORMAT_ARGB32, width.int32, height.int32)
  of fkSvg:
    surface = svg_surface_create(filename, width.float, height.float)
  of fkPdf:
    surface = pdf_surface_create(filename, width.float, height.float)
  else:
    raise newException(Exception, "Unsupported fType " & $fType & " in `initBImage[CairoBackend]")
  let backend = CairoBackend(cCanvas: surface,
                             created: false)

  result = BImage[CairoBackend](fname: filename,
                                backend: backend,
                                width: width,
                                height: height,
                                fType: fType)


proc destroy*(img: var BImage[CairoBackend]) =
  case img.fType
  of fkPng:
    let err = cairo.write_to_png(img.backend.cCanvas, img.fname)
    if err != cairo.StatusSuccess:
      echo "WARNING: `write_to_png` returned status code: ", err
  else: discard # not needed for SVG, PDF
  if img.backend.created:
    cairo.destroy(img.backend.ctx)
  cairo.destroy(img.backend.cCanvas)

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
