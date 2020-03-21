
import ginger

var view = initViewport()
view.layout(3, 3, colwidths = @[quant(2.5, ukCentimeter),
                                quant(0.0, ukRelative),
                                quant(5.0, ukCentimeter)],
            rowheights = @[quant(1.25, ukCentimeter),
                           quant(0.0, ukRelative),
                           quant(2.0, ukCentimeter)])
block:
  var mv = view
  let colors = ggColorHue(15)
  var idx = 0
  for ch in mitems(mv.children):
    ch.drawBoundary(some(colors[idx]), writeNumber = some(idx))
    inc idx

  var lg = mv[5]
  lg.layout(2, 2,
            colWidths = @[quant(1.0, ukCentimeter), # for space to plot
                          quant(0.0, ukRelative)], # for legend. incl header
            rowHeights = @[quant(1.0, ukCentimeter), # for header
                           quant(4.5, ukCentimeter)]) # for act. legend
  for ch in mitems(lg.children):
    ch.drawBoundary(some(colors[idx]), writeNumber = some(idx))
    inc idx

  var legView = lg[3]
  legView.layout(3, 1, colWidths = @[quant(1.0, ukCentimeter),
                                     quant(0.5, ukCentimeter),
                                     quant(0.0, ukRelative)])
  for ch in mitems(legView.children):
    ch.drawBoundary(some(colors[idx]), writeNumber = some(idx))
    inc idx
  lg[3] = legView
  mv[5] = lg
  mv.draw("boundsCont.pdf")
block:
  let colors = ggColorHue(15)
  var idx = 0
  #for ch in mitems(view.children):
  #  ch.drawBoundary(some(colors[idx]), writeNumber = some(idx))
  #  inc idx

  var lg = view[5]
  lg.layout(2, 2,
              colWidths = @[quant(1.0, ukCentimeter), # for space to plot
                            quant(0.0, ukRelative)], # for legend. incl header
              rowHeights = @[quant(1.0, ukCentimeter), # for header
                             quant(0.0, ukRelative)]) # for act. legend
  for ch in mitems(lg.children):
    ch.drawBoundary(some(colors[idx]), writeNumber = some(idx))
    inc idx
  view[5] = lg
  view.draw("boundsDiscr.pdf")
