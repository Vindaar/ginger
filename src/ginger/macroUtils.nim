import macros

proc findReplace(cImpl: NimNode, assgn: NimNode): NimNode =
  ## iterates through `cImpl`, looks for the assignment in `assgn`
  ## and if found replaces the value within. If not found appends
  ## the given statement to the NimNode
  result = copy(cImpl)
  for i in 1 ..< cImpl.len:
    let curNode = cImpl[i]
    case curNode.kind
    of nnkExprColonExpr:
      # check if field is `assgn` field
      if eqIdent(curNode[0].strVal, assgn[0].strVal):
        # replace this nodes value
        result[i][1] = assgn[1]
        # return now
        return result
    else:
      let msg = "No, shouldn't be here: " & $curNode.kind & " with repr " & curNode.treeRepr
      error(msg)
  # if we end up here we didn't find the identifier
  let newExpr = nnkExprColonExpr.newTree(
    assgn[0],
    assgn[1]
  )
  result.add newExpr

macro replace*(c: typed, x: untyped): untyped =
  ## mini dsl to replace specific fields of a given object
  ## with values given in body
  ##
  ## Example:
  ## .. code-block:
  ##   type
  ##     MyObj = object
  ##       a: int
  ##       b: seq[int]
  ##       c: bool
  ##       d: float
  ##   let obj = MyObj(a: 5,
  ##                   b: @[1, 2, 3]
  ##                   c: false)
  ##   let nObj = replace(obj):
  ##     c = true
  ##     d = 5.5
  ## # will be rewritten to
  ##   let nObj = MyObj(a: 5,
  ##                   b: @[1, 2, 3]
  ##                   c: true,
  ##                   d: 5.5)
  const oldAst = compileOption("oldast")
  when oldAst:
    var cImpl = c.getImpl
  else:
    # in new AST `getImpl` returns symbol for which it was called
    # too
    var cImpl = c.getImpl[2]
  # now just replace the correct cImpl fields
  # by the given fields of the user
  for ch in x:
    cImpl = findReplace(cImpl, ch)
  result = cImpl
  echo result.repr
