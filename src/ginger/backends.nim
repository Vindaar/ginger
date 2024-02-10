import chroma
import types
import options
from os import getTempDir, splitFile
from strutils import replace, normalize
from math import round

export types
export chroma

when defined(noCairo):
  {.warning: "The `noCairo` option is deprecated. Instead backends must be activated on demand " &
    "using `-d:use<Backend>` options (typically defined in a `nim.cfg` or `config.nims` file).".}

# `use*` variables are defined in `types.nim`
when useCairo and not defined(noCairo): # noCairo for backward compat
  import backendCairo
  export backendCairo
when useTikZ:
  import backendTikZ
  export backendTikZ
when usePixie:
  import backendPixie
  export backendPixie
# backend dummy is always available
import backendDummy
export backendDummy

proc parseFilename*(fname: string): FiletypeKind =
  let (_, _, ext) = fname.splitFile
  case ext.normalize
  of ".pdf":
    result = fkPdf
  of ".svg":
    result = fkSvg
  of ".png":
    result = fkPng
  of ".tex":
    result = fkTeX
  else:
    result = fkPdf

proc toBackend*(fType: FiletypeKind, texOptions: TexOptions): BackendKind =
  ## TODO: generalize the `texOptions` to variant object for possible other backends
  case fType
  of fkSvg: result = bkCairo
  of fkPng: result = bkCairo
  of fkTeX: result = bkTikZ
  of fkPdf:
    # depends on `texOptions`
    result = if texOptions.useTeX or texOptions.texTemplate.isSome:
               bkTikZ
             else:
               bkCairo # extend for Pixie
  of fkVega: doAssert false # not supported
  of fkDummy: result = bkDummy
  of fkNone:
    raise newException(ValueError, "File type fkNone cannot be converted to a backend.")

proc getTextExtent*(backend: BackendKind, fType: FileTypeKind, text: string, font: Font): TextExtent =
  template useBackend(backend: untyped): untyped =
    when declared(backend):
      result = backend.getTextExtent(fType, text, font)
    else:
      doAssert false, "The binary was compiled without the option to use the `" & $astToStr(backend) &
        "`. Please compile with `-d:use<Backend>` {Cairo, TikZ, Pixie} to activate it."

  case backend
  of bkCairo:         useBackend(CairoBackend)
  of bkTikZ:          useBackend(TikZBackend)
  of bkPixie:         useBackend(PixieBackend)
  of bkNone, bkDummy: useBackend(DummyBackend)
  else: discard

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
