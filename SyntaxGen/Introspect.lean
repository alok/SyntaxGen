/-
  SyntaxGen.Introspect: Generate examples from ParserDescr by introspecting the environment.

  This module can generate examples from any syntax rule defined with `syntax`,
  by looking up the ParserDescr constant and traversing it.
-/
import Lean
import SyntaxGen.Basic

namespace SyntaxGen.Introspect

open Lean Parser Meta Elab Command Term

/-! ## ParserDescr Introspection -/

/-- Try to evaluate a ParserDescr from a constant in the environment -/
unsafe def evalParserDescrUnsafe (env : Environment) (constName : Name) : Option ParserDescr :=
  let opts := {}
  match env.evalConst ParserDescr opts constName with
  | .ok descr => some descr
  | .error _ => none

/-- Safe wrapper that runs in IO -/
@[implemented_by evalParserDescrUnsafe]
opaque evalParserDescr (env : Environment) (constName : Name) : Option ParserDescr

/-! ## ParserDescr-based Generation -/

/-- Generate syntax from a ParserDescr -/
partial def genFromParserDescr (descr : ParserDescr) : GenM Syntax := do
  if ← isMaxDepth then return ← genIdent
  match descr with
  | .const n => genFromConst n
  | .unary n d => genUnary n d
  | .binary n d1 d2 => genBinary n d1 d2
  | .node _k _p d =>
      withDepth (genFromParserDescr d)
  | .nodeWithAntiquot _ _k d =>
      withDepth (genFromParserDescr d)
  | .sepBy d sep _ _ =>
      genSepBy d sep false
  | .sepBy1 d sep _ _ =>
      genSepBy d sep true
  | .cat catName _ =>
      withCat catName (genFromCat catName)
  | .parser n =>
      genFromConst n
  | .trailingNode .. => genIdent  -- Fallback for trailing parsers
  | .symbol s => return mkAtom s
  | .nonReservedSymbol s _ => return mkAtom s
  | .unicodeSymbol _uni ascii _ => return mkAtom ascii
where
  genFromConst (n : Name) : GenM Syntax := do
    match n with
    | `ident | `rawIdent => genIdent
    | `numLit | `num => genNumLit
    | `strLit | `str => genStrLit
    | `hole => return mkAtom "_"
    | `Lean.Parser.Term.hole => return mkAtom "_"
    | _ => genFromCat n

  genUnary (name : Name) (inner : ParserDescr) : GenM Syntax := do
    match name with
    | `optional =>
        if ← randBool 50 then
          withDepth (genFromParserDescr inner)
        else
          return Syntax.missing
    | `many =>
        let count ← randBound 3
        genMany inner count
    | `many1 =>
        let count ← randBound 2
        genMany inner (count + 1)
    | `group | `ppGroup | `ppIndent | `withPosition | `atomic =>
        withDepth (genFromParserDescr inner)
    | _ =>
        withDepth (genFromParserDescr inner)

  genBinary (name : Name) (d1 d2 : ParserDescr) : GenM Syntax := do
    match name with
    | `andthen =>
        let s1 ← withDepth (genFromParserDescr d1)
        let s2 ← withDepth (genFromParserDescr d2)
        return combine s1 s2
    | `orelse =>
        if ← randBool 50 then
          withDepth (genFromParserDescr d1)
        else
          withDepth (genFromParserDescr d2)
    | _ =>
        withDepth (genFromParserDescr d1)

  genMany (descr : ParserDescr) (count : Nat) : GenM Syntax := do
    if count == 0 then return Syntax.node .none `null #[]
    let mut results : Array Syntax := #[]
    for _ in [:count] do
      let s ← withDepth (genFromParserDescr descr)
      results := results.push s
    return Syntax.node .none `null results

  genSepBy (descr : ParserDescr) (sep : String) (atLeastOne : Bool) : GenM Syntax := do
    let minCount := if atLeastOne then 1 else 0
    let count ← randBound 3
    let count := count + minCount
    if count == 0 then return Syntax.node .none `null #[]
    let mut results : Array Syntax := #[]
    for i in [:count] do
      if i > 0 then results := results.push (mkAtom sep)
      let s ← withDepth (genFromParserDescr descr)
      results := results.push s
    return Syntax.node .none `null results

  combine (s1 s2 : Syntax) : Syntax :=
    let args1 := match s1 with
      | .node _ `null args => args
      | .missing => #[]
      | s => #[s]
    let args2 := match s2 with
      | .node _ `null args => args
      | .missing => #[]
      | s => #[s]
    Syntax.node .none `null (args1 ++ args2)

/-! ## Command for introspecting syntax -/

/-- Generate examples from a specific parser constant -/
elab "#syntaxgen_from" parserName:Lean.Parser.ident count:(num)? : command => do
  let name := parserName.getId
  let env ← getEnv
  let cnt := match count with
    | some n => getNat? n.raw |>.getD 10
    | none => 10

  -- Try to evaluate the parser description
  let some descr := evalParserDescr env name
    | throwError "Could not find ParserDescr for `{name}`. Make sure it's a valid parser constant."

  let mut results : Array Syntax := #[]
  let mut seed : Nat := 42

  for _ in [:cnt * 2] do
    if results.size >= cnt then break
    let config : GenConfig := { seed := seed }
    seed := seed + 7919
    match runGen config (genFromParserDescr descr) with
    | some s => results := results.push s
    | none => pure ()

  if results.isEmpty then
    logWarning s!"Could not generate examples from `{name}`."
  else
    logInfo m!"Generated {results.size} examples from `{name}`:"
    for ex in results do
      logInfo m!"  • {syntaxToString ex}"

/-- List all registered syntax node kinds for a category -/
elab "#syntaxgen_list" catName:Lean.Parser.ident : command => do
  let name := catName.getId
  let env ← getEnv
  let state := parserExtension.getState env
  let some cat := state.categories.find? name
    | throwError "Unknown syntax category `{name}`"

  let kinds := cat.kinds.toList.map Prod.fst
  logInfo m!"Syntax node kinds for `{name}` ({kinds.length} total):"
  for kind in kinds.take 30 do
    logInfo m!"  • {kind}"
  if kinds.length > 30 then
    logInfo m!"  ... and {kinds.length - 30} more"

end SyntaxGen.Introspect
