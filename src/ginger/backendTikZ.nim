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
from strutils import `%`, join, contains, replace, strip, splitLines



#[
Maybe we have to collect all colors in a table or seq and create a custom 'preamble' that
contains color definitons?
]#

proc toTikZCoord(img: BImage[TikZBackend], p: Point, isLength: static bool = false): Point =
  let ratio = img.height.float / img.width.float
  when isLength:
    result = p
  else:
    result = (x: p.x, y: img.height.float - p.y)

func toStr(img: BImage[TikZBackend], p: Point, isLength: static bool = false): string =
  block:
    let pst = img.toTikZCoord(p, isLength = isLength)
    var tmp = &"({pst.x:.4f}bp, {pst.y:.4f}bp)"
    tmp

func toStrDirect(p: Point, isLength: static bool = false): string =
  block:
    var tmp = &"({p.x:.4f}bp, {p.y:.4f}bp)"
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

proc applyStyle(text: string, font: Font): string =
  ## Applies the correct style to the given text, depending on the font.
  if font.bold:
    result = latex:
      \textbf{`text`}
  elif font.family == "monospace":
    # replace spaces by `\ ` to get explicit spaces where spaces are found to
    # get correct size for text with spaces.
    let text = text.replace(" ", r"\ ")
    result = latex:
      \texttt{`text`}
  else:
    result = text

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
      dash pattern = on $(dash())bp off $(dashSpace())bp
  of ltDotted:
    result = latex:
      dash pattern = on $(dot())bp off $(dotSpace())bp
  of ltDotDash:
    result = latex:
      dash pattern = on $(dot())bp off $(dotSpace()) on $(dash())bp off $(dotSpace())
  of ltLongDash:
    result = latex:
      dash pattern = on $(longDash())bp off $(dashSpace())
  of ltTwoDash:
    result = latex:
      dash pattern = on $(dash())bp off $(dotSpace() * 2.0) on $(longDash())bp off $(dotSpace() * 2.0)
  else: discard

proc lineStyle(style: Style, drawColor = "drawColor", fillColor = "fillColor"): string =
  let fillOp = &"fill opacity = {style.fillColor.a}"
  let colorOp = &"draw opacity = {style.color.a}"
  let lineDash = getLineStyle(style.lineType, style.lineWidth)
  result = &"[color = {drawColor}, fill = {fillColor}, {colorOp}, {fillOp}, line width = {style.lineWidth}bp"
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


from std/os import getTempDir, `/`
import std/[tables, strscans]
var cacheTab = initTable[(string, Font), (float, float, float)]()
import latexdsl / tex_daemon
var Daemon: TeXDaemon

proc checkSize*(td: TeXDaemon, style, arg: string): (float, float, float) =
  ## Checks the of the given argument if formatted by LaTeX
  const sizeCommands = """
\typeout{\the\wd\mybox} % Width
\typeout{\the\ht\mybox} % Height
\typeout{\the\dp\mybox} % Depth
"""
  let boxArg = """$#
\sbox{\mybox}{$#}
""" % [style, arg]
  td.process(boxArg)
  # after processing write the size commands and read back the data
  var
    w: float
    h: float
    d: float
    matched = false
    idx = 0
  for cmd in sizeCommands.strip.splitLines():
    td.write(cmd)
    var res = td.read()
    while "pt" notin res: ## If there is some more data in the stream before, make all read
      res = td.read()
    case idx
    of 0: (matched, w) = res.strip().scanTuple("$fpt")
    of 1: (matched, h) = res.strip().scanTuple("$fpt")
    of 2: (matched, d) = res.strip().scanTuple("$fpt")
    else: doAssert false, "Why at index : " & $idx
    doAssert matched, "Did not match '$fpt', input was: " & $res
    inc idx
  result = (w, h, d)

proc getExtents(text: string, font: Font): (float, float, float) =
  const setup = """
\documentclass[draft]{article}

\usepackage{unicode-math}
\usepackage{amsmath}
\usepackage{siunitx}
\usepackage{tikz}

\newbox\mybox
% \newlength\mywidth
% \newlength\myheight
% \newlength\mydepth

\begin{document}
"""
  if not Daemon.isReady: ## If not already set up, do so now
    Daemon = initTeXDaemon()
    # process the setup to be ready to return sizes
    Daemon.process(setup)

  let textStr = applyStyle(text, font)
  if (textStr, font) notin cacheTab:
    let fs = font.size
    let fontSize = latex:
      \fontsize{$(fs)}{$(fs * 1.2)}\selectfont
    var (w, h, d) = Daemon.checkSize(fontSize, textStr)
    ## Convert the `pt` values we get to `bp` values (72.27 dpi for pt, 72.0 for bp)
    let r = 72.27 / 72.0
    (w, h, d) = (w * r, h * r, d * r)
    cacheTab[(textStr, font)] = (w, h, d)
    result = (w, h, d)
  else:
    result = cacheTab[(textStr, font)]

proc getTextExtent*(_: typedesc[TikZBackend], fType: FileTypeKind, text: string, font: Font): TextExtent =
  ## Uses a TeX compiler in the background to determine the real sizes of the given text and font.
  let (w, h, d) = getExtents(text, font)
  result = TextExtent(
    x_bearing: 0.0,
    y_bearing: -d, ## XXX: not sure if this should be on bearing
    width: w,
    height: h,
    x_advance: w,
    y_advance: h)

proc drawText*(img: var BImage[TikZBackend], text: string, font: Font, at: Point,
               alignKind: TextAlignKind = taLeft,
               rotate: Option[float] = none[float](),
               rotateInView: Option[(float, Point)] = none[(float, Point)]()) =
  let alignLeft = if r"\\" in text: true else: false # for manual line breaks, need left alignment
  let useAlignOverAnchor = r"\\" in text
  let alignStr = img.nodeProperties(at, alignKind, rotate, font,
                                    alignLeft = alignLeft,
                                    useAlignOverAnchor = useAlignOverAnchor)
  let textStr = applyStyle(text, font)
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

proc drawBackground*(img: var BImage[TikZBackend], style: Style) =
  ## TikZ specific helper to draw the background. This is simply because LaTeX may extend the
  ## size of the page over our initial restriction if an element overflows. Therefore
  ## use use `xcolor` to set the page color explicitly.
  let colorDef = defColor("backgroundColor", style.fillColor)
  let color = "backgroundColor"
  let toAdd = latex:
    `colorDef`
    \pagecolor{`color`}
  img.backend.bodyHeader = toAdd

when useCairo and not defined(noCairo):
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

proc getStandaloneTmpl(img: BImage[TikZBackend]): string =
  let w = $(img.width.float ) & "bp"
  let h = $(img.height.float) & "bp"
  result = latex:
    \documentclass[tikz,border="0mm"]{standalone}
    \usepackage[utf8]{inputenc}
    \usepackage{unicode-math} # for unicode support in math environments
    \usepackage{amsmath}
    \usepackage{siunitx}
    \usepackage{tikz}
    "$#"
    document:
      "$#"
      tikzpicture["every node/.style={outer sep=0pt, inner sep=0pt}"]:
        \path["use as bounding box"] (0,0) rectangle (`w`, `h`) ";"
        "$#"

proc getOnlyTikZTmpl(img: BImage[TikZBackend], texOptions: TeXOptions): string =
  let w = $(img.width.float ) & "bp"
  let h = $(img.height.float) & "bp"
  if texOptions.caption.isSome and texOptions.label.isSome:
    let plc = texOptions.placement
    let cap = texOptions.caption.get
    let lab = texOptions.label.get
    result = latex:
      figure[`plc`]:
        \centering
        tikzpicture["every node/.style={outer sep=0pt, inner sep=0pt}"]:
          \path["use as bounding box"] (0,0) rectangle (`w`, `h`) ";"
          "$#"
        \label{`lab`}
        \caption{`cap`}
  elif texOptions.caption.isSome:
    let plc = texOptions.placement
    let cap = texOptions.caption.get
    result = latex:
      figure[`plc`]:
        \centering
        tikzpicture["every node/.style={outer sep=0pt, inner sep=0pt}"]:
          \path["use as bounding box"] (0,0) rectangle (`w`, `h`) ";"
          "$#"
        \caption{`cap`}
  elif texOptions.label.isSome:
    let plc = texOptions.placement
    let lab = texOptions.label.get
    result = latex:
      figure[`plc`]:
        \centering
        tikzpicture["every node/.style={outer sep=0pt, inner sep=0pt}"]:
          \path["use as bounding box"] (0,0) rectangle (`w`, `h`) ";"
          "$#"
        \label{`lab`}
  else:
    result = latex:
      tikzpicture["every node/.style={outer sep=0pt, inner sep=0pt}"]:
        \path["use as bounding box"] (0,0) rectangle (`w`, `h`) ";"
        "$#"

proc getArticleTmpl(img: BImage[TikZBackend]): string =
  let w = $(img.width.float ) & "bp"
  let h = $(img.height.float) & "bp"
  result = latex:
    \documentclass[a4paper]{article}
    \usepackage[utf8]{inputenc}
    \usepackage[margin="2.5cm"]{geometry}
    \usepackage{unicode-math} # for unicode support in math environments
    \usepackage{amsmath}
    \usepackage{siunitx}
    \usepackage{tikz}
    "$#"
    document:
      "$#"
      center:
        tikzpicture["every node/.style={outer sep=0pt, inner sep=0pt}"]:
          \path["use as bounding box"] (0,0) rectangle (`w`, `h`) ";"
          "$#"

proc genTeXFile*(img: BImage[TikZBackend]): string =
  var tmpl: string
  if img.backend.options.texTemplate.isNone:
    if img.backend.options.onlyTikZ:
      # Does not support `header` or `bodyHeader`
      tmpl = img.getOnlyTikZTmpl(img.backend.options) % img.backend.data
    elif img.backend.options.standalone:
      tmpl = img.getStandaloneTmpl() % [img.backend.header, img.backend.bodyHeader, img.backend.data]
    else:
      tmpl = img.getArticleTmpl() % [img.backend.header, img.backend.bodyHeader, img.backend.data]
  else:
    tmpl = img.backend.options.texTemplate.get % [img.backend.header, img.backend.bodyHeader, img.backend.data]
  result = tmpl

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

const QuietTikZ {.booldefine.} = false
proc destroy*(img: var BImage[TikZBackend]) =
  # write to file
  let body = backendTikZ.genTeXFile(img)
  # possibly compile
  # get the path for the output file
  let path = img.fname.parentDir
  case img.fType
  of fkTeX: writeFile(img.fname, body) # only write the TeX file, do not compile
  of fkPdf:
    compile(img.fname, body, path = path, fullBody = true, verbose = not QuietTikZ)
  else: doAssert false

  # close the TeXDaemon
  Daemon.close()
  Daemon.isReady = false
