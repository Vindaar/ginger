import chroma
import math
import types
import options

import os, strformat
# use `latexdsl_nochecks` instead of regular to avoid checking all enum fields.
# The LaTeX code written here is "write once, compile many", no need to recheck
# everything. If we allow the user to hand their own LaTeX snippets, those
# can be checked separately on the user's side.
import latexdsl_nochecks
from strutils import `%`, join, contains

#[
Maybe we have to collect all colors in a table or seq and create a custom 'preamble' that
contains color definitons?
]#

proc toTikZCoord(img: BImage[TikZBackend], p: Point, isLength: static bool = false): Point =
  let ratio = img.height.float / img.width.float
  when isLength:
    result = (x: p.x / img.width.float, y: p.y / img.height.float)
  else:
    result = (x: p.x / img.width.float, y: (img.height.float - p.y) / img.height.float)
  result.y = result.y * ratio

func toStr(img: BImage[TikZBackend], p: Point, isLength: static bool = false): string =
  block:
    let pst = img.toTikZCoord(p, isLength = isLength)
    var tmp = &"({pst.x:.4f}\\textwidth, {pst.y:.4f}\\textwidth)"
    tmp

func toStrDirect(p: Point, isLength: static bool = false): string =
  block:
    var tmp = &"({p.x:.4f}\\textwidth, {p.y:.4f}\\textwidth)"
    tmp

proc toStr(alignKind: TextAlignKind): string =
  ## convert the text align kind to the correct TikZ notation.
  ##
  ## In TikZ the the node is placed `right`, `left` (etc.) of the coordinate, wherease
  ## in cairo it is aligned by the `left` / `right` edge. Thus, the inversion.
  case alignKind
  of taLeft: result = "right"
  of taCenter: result = "" # default
  of taRight: result = "left"

proc toAnchorStr(alignKind: TextAlignKind): string =
  ## convert the text align kind to the correct TikZ notation for an anchor.
  case alignKind
  of taLeft: result = "west"
  of taCenter: result = "" # default
  of taRight: result = "east"

proc nodeProperties(img: BImage[TikZBackend], at: Point, alignKind: TextAlignKind, rotate: Option[float],
                    font: Font,
                    alignLeft = false,
                    useAlignOverAnchor = false): string =
  ## If `useAlignOverAnchor` is true, we do not use `anchor=`, but rather `align=`. This is
  ## the case if text is put that contains manual line breaks. Those are not supported with `anchor`
  ## for whatever reason.
  var res = newSeq[string]()
  let ak = alignKind.toStr
  if ak.len > 0:
    res.add ak # this is placement of the node relative to coordinate. Required if `align` is used.
  if rotate.isSome:
    res.add "rotate = " & $(-rotate.get) # rotation is opposite of cairo
  let fs = font.size
  let fontSize = latex:
    font = \fontsize{$(fs)}{$(fs * 1.2)}\selectfont
  res.add fontSize
  if useAlignOverAnchor:
    # if we use align, left and right are same as in ginger
    if alignKind == taLeft: res.add "align=left"
    elif alignKind == taRight: res.add "align=right"
  else:
    let anchor = alignKind.toAnchorStr
    if anchor.len > 0:
      res.add &"anchor={anchor}"
  result = res.join(", ")
  if result.len > 0:
    result = "[" & result & "]"

template latexAdd(body: untyped): untyped {.dirty.} =
  block:
    let toAdd = latex:
      body
    img.backend.data.add toAdd

proc defColor(name: string, c: Color): string =
  let color = &"{c.r}, {c.g}, {c.b}"
  result = latex:
    \definecolor{`name`}{rgb}{`color`}

func addColorIfNew(img: var BImage[TikZBackend], color: string) =
  if color != img.backend.lastColor:
    latexAdd:
      `color`
    img.backend.lastColor = color

proc colorStr(style: Style): string =
  result.add defColor("drawColor", style.color)
  result.add defColor("fillColor", style.fillColor)

func getLineStyle(lineType: LineType, lineWidth: float): string =
  template dash: untyped = lineWidth * 4.0
  template dashSpace: untyped = lineWidth * 5.0
  template dot: untyped = lineWidth / 2.0
  template dotSpace: untyped = lineWidth * 2.0
  template longDash: untyped = lineWidth * 8.0

  case lineType
  of ltDashed:
    result = latex:
      dash pattern = on $(dash())pt off $(dashSpace())pt
  of ltDotted:
    result = latex:
      dash pattern = on $(dot())pt off $(dotSpace())pt
  of ltDotDash:
    result = latex:
      dash pattern = on $(dot())pt off $(dotSpace()) on $(dash())pt off $(dotSpace())
  of ltLongDash:
    result = latex:
      dash pattern = on $(longDash())pt off $(dashSpace())
  of ltTwoDash:
    result = latex:
      dash pattern = on $(dash())pt off $(dotSpace() * 2.0) on $(longDash())pt off $(dotSpace() * 2.0)
  else: discard

proc lineStyle(style: Style, drawColor = "drawColor", fillColor = "fillColor"): string =
  let fillOp = &"fill opacity = {style.fillColor.a}"
  let colorOp = &"draw opacity = {style.color.a}"
  let lineDash = getLineStyle(style.lineType, style.lineWidth)
  result = &"[color = {drawColor}, fill = {fillColor}, {colorOp}, {fillOp}, line width = {style.lineWidth}pt"
  if lineDash.len > 0:
    result.add ", " & lineDash & "]"
  else:
    result.add "]"

proc drawLine*(img: var BImage[TikZBackend], start, stop: Point,
               style: Style,
               rotateAngle: Option[(float, Point)] = none[(float, Point)]()) =
  let p0 = img.toStr(start)
  let p1 = img.toStr(stop)
  let color = style.colorStr
  let lineSt = style.lineStyle
  img.addColorIfNew(color)
  latexAdd:
    \draw `lineSt` `p0` -- `p1` ";"

proc drawPolyLine*(img: var BImage[TikZBackend], points: seq[Point],
                   style: Style,
                   rotateAngle: Option[(float, Point)] = none[(float, Point)]()) =
  let lineSt = style.lineStyle
  let color = style.colorStr
  img.addColorIfNew(color)
  latexAdd:
    \draw `lineSt`
  for i, p in points:
    let pStr = img.toStr(p)
    if i == points.high:
      latexAdd:
        `pStr`
    else:
      latexAdd:
        `pStr` " -- "
  img.backend.data.add ";"

proc drawCircle*(img: var BImage[TikZBackend], center: Point, radius: float,
                 lineWidth: float,
                 strokeColor = color(0.0, 0.0, 0.0),
                 fillColor = color(0.0, 0.0, 0.0, 0.0),
                 rotateAngle: Option[(float, Point)] = none[(float, Point)]()) =
  let p = img.toStr(center)
  let radius = $(radius / img.width.float) & "\\textwidth"# / img.width.float
  let style = Style(lineWidth: lineWidth,
                    color: strokeColor,
                    fillColor: fillColor)
  let color = style.colorStr
  img.addColorIfNew(color)
  let lineSt = style.lineStyle
  latexAdd:
    \draw `lineSt` `p` circle [radius = `radius`] ";"

proc getTextExtent*(_: typedesc[TikZBackend], text: string, font: Font): TextExtent =
  ## XXX: HACK
  let ptY = font.size
  let ptX = ptY * 0.5 ## TODO: ideally we need the correct font height to width ratio!
  result = TextExtent(
    x_bearing: 0.0,
    y_bearing: 0.0,
    width: ptX * text.len.float,
    height: ptY)
  result.x_advance = result.width
  result.y_advance = result.height

proc drawText*(img: var BImage[TikZBackend], text: string, font: Font, at: Point,
               alignKind: TextAlignKind = taLeft,
               rotate: Option[float] = none[float](),
               rotateInView: Option[(float, Point)] = none[(float, Point)]()) =
  let alignLeft = if r"\\" in text: true else: false # for manual line breaks, need left alignment
  let useAlignOverAnchor = r"\\" in text
  let alignStr = img.nodeProperties(at, alignKind, rotate, font,
                                    alignLeft = alignLeft,
                                    useAlignOverAnchor = useAlignOverAnchor)
  var textStr: string
  if font.bold:
    textStr = latex:
      \textbf{`text`}
  else:
    textStr = latex:
      `text`
  latexAdd:
    \node `alignStr` at $(img.toStr(at)) {`textStr`} ";"

proc drawRectangle*(img: var BImage[TikZBackend], left, bottom, width, height: float,
                    style: Style,
                    rotate: Option[float] = none[float](),
                    rotateInView: Option[(float, Point),] = none[(float, Point)]()) =
  let atPt = img.toTikZCoord((x: left, y: bottom))
  let sizePt = img.toTikZCoord((x: left + width, y: bottom + height), isLength = false)
  if style.gradient.isSome:
    let gradient = style.gradient.get
    let heightRel = height / img.width.float
    let sliceHeight = heightRel / (gradient.colors.len.float - 1)
    var curBottom = atPt.y - heightRel
    for i, c in gradient.colors:
      let n = "color" & $i
      let atStr = (x: atPt.x, y: curBottom).toStrDirect # left bottom coords
      let sizeStr = (
        x: sizePt.x,
        y: curBottom + sliceHeight + 1e-3               # add some ε for overlap
      ).toStrDirect                                     # end coords
      let curColor = defColor(n, c)
      let curLineStyle = style.lineStyle(fillColor = n)
      latexAdd:
        `curColor`
        \draw `curLineStyle` `atStr` rectangle `sizeStr` ";"
      curBottom += sliceHeight
  else:
    let color = style.colorStr
    let lineSt = style.lineStyle
    let sizeStr = sizePt.toStrDirect
    let atStr = atPt.toStrDirect
    img.addColorIfNew(color)
    latexAdd:
      \draw `lineSt` `atStr` rectangle `sizeStr` ";"

when defined(useCairo) and not defined(noCairo):
  from backendCairo import initBImage, drawRaster, destroy
  proc drawRaster*(img: var BImage[TikZBackend], left, bottom, width, height: float,
                   numX, numY: int,
                   drawCb: proc(): seq[uint32],
                   rotate: Option[float] = none[float](),
                   rotateInView: Option[(float, Point),] = none[(float, Point)]()) =
    ## draw raster by using Cairo to draw the actual raster and store it as a png. That we
    ## include here
    let tmpName = getTempDir() & "raster_ggplotnim_tikz_tmp_store.png"
    # create a Cairo backend image
    var imgC = initBImage(CairoBackend,
                          tmpName,
                          width = width.toInt(), height = height.toInt(),
                          ftype = fkPng,
                          texOptions = TeXOptions())
    imgC.drawRaster(0, 0, width, height, numX, numY, drawCB, rotate, rotateInView)
    imgC.destroy()
    # embedding a picture using a node places ``center`` of picture at `atStr` coord
    let atStr = img.toStr((x: left + width / 2.0, y: bottom + height / 2.0))
    let w = width / img.width.float
    latexAdd:
      \node at `atStr` {\includegraphics[width = `w`\textwidth]{`tmpName`}}";"
else:
  proc drawRaster*(img: var BImage[TikZBackend], left, bottom, width, height: float,
                   numX, numY: int,
                   drawCb: proc(): seq[uint32],
                   rotate: Option[float] = none[float](),
                   rotateInView: Option[(float, Point),] = none[(float, Point)]()) =
    {.warning: "`drawRaster` is not supported as the code was compiled without `-d:useCairo`.".}

proc getStandaloneTmpl(): string =
  result = latex:
    \documentclass[tikz,border = "2mm"]{standalone}
    \usepackage[utf8]{inputenc}
    \usepackage[margin="2.5cm"]{geometry}
    \usepackage{unicode-math} # for unicode support in math environments
    \usepackage{amsmath}
    \usepackage{siunitx}
    \usepackage{tikz}
    document:
      tikzpicture:
        "$#"

proc getOnlyTikZTmpl(texOptions: TeXOptions): string =
  if texOptions.caption.isSome and texOptions.label.isSome:
    let plc = texOptions.placement
    let cap = texOptions.caption.get
    let lab = texOptions.label.get
    result = latex:
      figure[`plc`]:
        \centering
        tikzpicture:
          "$#"
        \label{`lab`}
        \caption{`cap`}
  elif texOptions.caption.isSome:
    let plc = texOptions.placement
    let cap = texOptions.caption.get
    result = latex:
      figure[`plc`]:
        \centering
        tikzpicture:
          "$#"
        \caption{`cap`}
  elif texOptions.label.isSome:
    let plc = texOptions.placement
    let lab = texOptions.label.get
    result = latex:
      figure[`plc`]:
        \centering
        tikzpicture:
          "$#"
        \label{`lab`}
  else:
    result = latex:
      tikzpicture:
        "$#"

#proc getOnlyTikZTmpl(): string =
#  result = latex:
#    figure:
#      \centering
#      tikzpicture:
#        "$#"
#      \caption{test caption}
#      \label{test_label}

proc getArticleTmpl(): string =
  result = latex:
    \documentclass[a4paper]{article}
    \usepackage[utf8]{inputenc}
    \usepackage[margin="2.5cm"]{geometry}
    \usepackage{unicode-math} # for unicode support in math environments
    \usepackage{amsmath}
    \usepackage{siunitx}
    \usepackage{tikz}
    document:
      center:
        tikzpicture:
          "$#"

proc writeTeXFile*(img: BImage[TikZBackend]) =
  var tmpl: string
  if img.backend.options.texTemplate.isNone:
    if img.backend.options.onlyTikZ:
      tmpl = getOnlyTikZTmpl(img.backend.options)
    elif img.backend.options.standalone:
      tmpl = getStandaloneTmpl()
    else:
      tmpl = getArticleTmpl()
  else:
    tmpl = img.backend.options.texTemplate.get
  var f = open(img.fname, fmWrite)
  f.write(tmpl % img.backend.data)
  f.close()

proc initBImage*(_: typedesc[TikZBackend],
                 filename: string,
                 width, height: int,
                 fType: FiletypeKind,
                 texOptions: TexOptions): BImage[TikZBackend] =
  let fname = if ftype == fkPdf: filename.replace(".pdf", ".tex")
              else: filename
  let backend = TikZBackend(options: texOptions)
  result = BImage[TikZBackend](fname: fname,
                               backend: backend,
                               width: width, height: height,
                               fType: fType)

import shell
proc destroy*(img: var BImage[TikZBackend]) =
  # write to file
  backendTikZ.writeTeXFile(img)
  # possibly compile
  # get the path for the output file
  let path = img.fname.parentDir
  case img.fType
  of fkTeX: discard # nothing to do
  of fkPdf:
    # compile using terminal
    # 1. check if xelatex in PATH
    when defined(linux) or defined(macosx):
      let checkCmd = "command -v"
    elif defined(windows):
      let checkCmd = "WHERE"
    else:
      {.error: "Unsupported platform for PDF generation. Please open an issue.".}

    var generated = false
    template checkAndRun(cmd: untyped): untyped =
      var (res, err) = shellVerbose:
        ($checkCmd) ($cmd)
      if err == 0:
        (res, err) = shellVerbose:
          ($cmd) "-output-directory" ($path) ($img.fname)
        if err == 0:
          # successfully generated
          generated = true
        else:
          raise newException(IOError, "Could not generate PDF from TeX file `" & $img.fname &
            & "` using TeX compiler: `" & $cmd & "`. Output was: " &
            res)
    checkAndRun("xelatex")
    if generated: return # success, no need to try `pdflatex`
    checkAndRun("pdflatex") # currently broken, as we import `unicode-math`
    if not generated:
      raise newException(IOError, "Could not generate a PDF from TeX file " &
        $img.fname & " as neither `xelatex` nor `pdflatex` was found in PATH")
  else: doAssert false
