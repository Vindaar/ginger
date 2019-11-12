import cairo
import chroma
import types

export types
export chroma
#export cairo

import backendCairo
export backendCairo

proc destroy*(img: var BImage) =
  case img.backend
  of bkCairo:
    case img.fType
    of fkPng:
      let err = img.cCanvas.write_to_png(img.fname)
      echo err, " output of write_to_png"
    else: discard # not needed for SVG, PDF
    img.cCanvas.destroy()
  of bkVega:
    discard


when isMainModule:
  # backend layer cairo code
  var img = initBImage("test.svg",
                       width = 600, height = 400,
                       backend = bkCairo,
                       ftype = fkSvg)
  img.drawLine((0.0, 0.0), (150.0, 140.0))
  img.drawCircle((200.0, 300.0), 2.0, lineWidth = 1.0,
                 strokeColor = color(0.0, 0.0, 0.0, 0.0),
                 fillColor = color(0.9, 0.9, 0.9))
  img.drawText("Hello!", Font(family: "serif", size: 12.0, color: color(1.0, 0.0, 0.0)), (0.0, 200.0))
  img.destroy()
