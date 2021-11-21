import ginger

#[
This is a super dump test to see the alignment of two text snippets
that are at the same position. The output should show `1` left of `0`.

I'm still not sure if the result should be closer to another or not...

And obviously this is not a proper test, as I'm a bit too lazy to turn this somehow
into a proper test, as ideally it should take the LaTeX compilation into account.
]#

var view = initViewport(backend = bkTikZ)
let txt = view.initText(initCoord(0.5, 0.5),
                        "0",
                        goText,
                        alignKind = taLeft)
let txt2 = view.initText(initCoord(0.5, 0.5),
                         "1",
                         goText,
                         alignKind = taRight)

view.addObj txt, txt2
view.draw("/tmp/txt.pdf", TexOptions(useTex: true))
