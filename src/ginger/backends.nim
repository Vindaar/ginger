import chroma, shell
import types
import options
from os import getTempDir
from strutils import replace

export types
export chroma


when not defined(noCairo):
  # import nothing into scope, so that we avoid overload ambiguity
  from cairo import nil
  from backendCairo import nil
  from backendTikZ import nil

  proc drawLine*(img: var BImage, start, stop: Point,
                 style: Style,
                 rotateAngle: Option[(float, Point)] = none[(float, Point)]()) =
    case img.backend
    of bkCairo: backendCairo.drawLine(img, start, stop, style, rotateAngle)
    of bkTikZ: backendTikz.drawLine(img, start, stop, style, rotateAngle)
    else: discard

  proc drawPolyLine*(img: var BImage, points: seq[Point],
                     style: Style,
                     rotateAngle: Option[(float, Point)] = none[(float, Point)]()) =
    case img.backend
    of bkCairo: backendCairo.drawPolyLine(img, points, style, rotateAngle)
    of bkTikZ: backendTikz.drawPolyLine(img, points, style, rotateAngle)
    else: discard

  proc drawCircle*(img: var BImage, center: Point, radius: float,
                   lineWidth: float,
                   strokeColor = color(0.0, 0.0, 0.0),
                   fillColor = color(0.0, 0.0, 0.0, 0.0),
                   rotateAngle: Option[(float, Point)] = none[(float, Point)]()) =
    case img.backend
    of bkCairo:
      backendCairo.drawCircle(
        img, center, radius, lineWidth, strokeColor, fillColor, rotateAngle
      )
    of bkTikz:
      backendTikz.drawCircle(
        img, center, radius, lineWidth, strokeColor, fillColor, rotateAngle
      )
    else: discard

  proc getTextExtent*(backend: BackendKind, text: string, font: Font): TextExtent =
    case backend
    of bkCairo: result = backendCairo.getTextExtent(text, font)
    of bkTikZ: result = backendTikZ.getTextExtent(text, font)
    else: discard

  proc drawText*(img: var BImage, text: string, font: Font, at: Point,
                 alignKind: TextAlignKind = taLeft,
                 rotate: Option[float] = none[float](),
                 rotateInView: Option[(float, Point)] = none[(float, Point)]()) =
    case img.backend
    of bkCairo:
      backendCairo.drawText(
        img, text, font, at, alignKind, rotate, rotateInView
      )
    of bkTikz:
      backendTikz.drawText(
        img, text, font, at, alignKind, rotate, rotateInView
      )
    else: discard

  proc drawRectangle*(img: var BImage, left, bottom, width, height: float,
                      style: Style,
                      rotate: Option[float] = none[float](),
                      rotateInView: Option[(float, Point),] = none[(float, Point)]()) =
    case img.backend
    of bkCairo:
      backendCairo.drawRectangle(
        img, left, bottom, width, height, style, rotate, rotateInView
      )
    of bkTikz:
      backendTikz.drawRectangle(
        img, left, bottom, width, height, style, rotate, rotateInView
      )
    else: discard

  # forward declarations to use them in `drawRaster` for `TikZ`
  proc initBImage*(filename: string,
                   width, height: int,
                   fType: FiletypeKind,
                   texOptions: TeXOptions): BImage
  proc destroy*(img: var BImage)
  proc drawRaster*(img: var BImage, left, bottom, width, height: float,
                   numX, numY: int,
                   drawCb: proc(): seq[uint32],
                   rotate: Option[float] = none[float](),
                   rotateInView: Option[(float, Point),] = none[(float, Point)]()) =
    case img.backend
    of bkCairo:
      backendCairo.drawRaster(
        img, left, bottom, width, height, numX, numY, drawCB, rotate, rotateInView
      )
    of bkTikz:
      let tmpName = getTempDir() & "raster_ggplotnim_tikz_tmp_store.png"
      var imgC = initBImage(tmpName,
                            width = width.int, height = height.int,
                            ftype = fkPng,
                            texOptions = TeXOptions())
      imgC.drawRaster(0, 0, width, height, numX, numY, drawCB, rotate, rotateInView)
      imgC.destroy()
      backendTikz.drawRaster(
        img, tmpName, left, bottom, width, height, numX, numY, drawCB, rotate, rotateInView
      )
    else: discard

  proc initBImage*(filename: string,
                   width, height: int,
                   fType: FiletypeKind,
                   texOptions = TeXOptions()): BImage =
    let backend = if ftype == fkTeX or (ftype == fkPdf and texOptions.useTeX): bkTikZ
                  else: bkCairo
    case backend
    of bkCairo:
      result = backendCairo.initBImage(
        filename, width, height, fType
      )
    of bkTikz:
      let fname = if ftype == fkPdf: filename.replace(".pdf", ".tex")
                  else: filename
      result = backendTikz.initBImage(
        fname, width, height, fType, texOptions
      )
    else: discard

  proc destroy*(img: var BImage) =
    case img.backend
    of bkCairo:
      case img.fType
      of fkPng:
        let err = cairo.write_to_png(img.cCanvas, img.fname)
        echo err, " output of write_to_png"
      else: discard # not needed for SVG, PDF
      if img.created:
        cairo.destroy(img.ctx)
      cairo.destroy(img.cCanvas)
    of bkTikZ:
      # write to file
      backendTikZ.writeTeXFile(img)
      # possibly compile
      case img.fType
      of fkTeX: discard # nothing to do
      of fkPdf:
        # compile using terminal

        # 1. check if xelatex in PATH
        when defined(linux) or defined(macos):
          let checkCmd = "command -v"
        elif defined(windows):
          let checkCmd = "WHERE"
        else:
          raise newException(Exception, "Unsupported platform for PDF generation. Please open an issue.")

        var generated = false
        template checkAndRun(cmd: untyped): untyped =
          var (res, err) = shellVerbose:
            ($checkCmd) ($cmd)
          if err == 0:
            (res, err) = shellVerbose:
              ($cmd) ($img.fname)
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
    of bkVega:
      discard
else:
  proc destroy*(img: var BImage) =
    echo "Nothing to destroy when compiled without backend."

  import backendDummy
  export backendDummy

when isMainModule:
  # backend layer cairo code
  var img = initBImage("test.svg",
                       backend = bkCairo,
                       width = 600, height = 400,
                       ftype = fkSvg)
  img.drawLine((0.0, 0.0), (150.0, 140.0))
  img.drawCircle((200.0, 300.0), 2.0, lineWidth = 1.0,
                 strokeColor = color(0.0, 0.0, 0.0, 0.0),
                 fillColor = color(0.9, 0.9, 0.9))
  img.drawText("Hello!", Font(family: "serif", size: 12.0, color: color(1.0, 0.0, 0.0)), (0.0, 200.0))
  img.destroy()
