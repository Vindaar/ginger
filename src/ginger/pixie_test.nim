import chroma
import types
import options

import pixie
import backends

when isMainModule:
  var 
    style = Style(
      color: color(0.0, 0.0, 0.0, 1.0),
      size: 2.0,
      lineType: ltSolid,
      lineWidth: 3.0,
      fillColor: color(0.0, 0.0, 0.0, 1.0),
      marker: mkCircle,
      errorBarKind: ebLines,
      gradient: none[Gradient](),
      font: types.Font(
        family: "foo",
        size: 16.0,
        bold: false,
        slant: fsNormal,
        color: color(0.0, 0.0, 0.0, 0.0),
        alignKind: taCenter
      )
    )
    # backend Pixie tests
    img = initBImage("test.png",
                         width = 600, height = 400,
                         bkPixie,
                         ftype = fkPng)
    rotation = option((45.0, (1.0, 5.0)))
  img.drawLine((0.0, 0.0), (150.0, 140.0), style)
  img.drawRectangle(300.0, 175.0, 100.0, 100.0, style)
  # Now with rotation
  img.drawRectangle(100, 0, 100.0, 100.0, style, none[float](), rotation)
  img.pxContext.image.writeFile(img.fname)
