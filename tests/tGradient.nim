import ginger, sequtils, options
import ggplotnim / colormaps / viridisRaw

let viridis = ViridisRaw.mapIt(color(it[0], it[1], it[2]))

var img = initViewport()
var rect = img.initRect(#c(0.0, 0.0),
                        c(0.5, 0.5),
                        #width = quant(1.0, ukRelative),#ukCentimeter),
                        #height = quant(1.0, ukRelative), #Centimeter),
                        width = quant(11.0, ukCentimeter),
                        height = quant(5.0, ukCentimeter),
                        gradient = some(Gradient(colors: viridis)))
echo rect
let st = Style(color: color(0.0, 0.0, 1.0, 1.0),
               lineType: ltSolid,
               lineWidth: 1.0,
               fillColor: grey92)
var rect2 = rect
rect2.rotate = some(30.0)

img.background(some(st))
img.addObj @[rect, rect2]

#img.background()
img.draw("tGradient.pdf")
