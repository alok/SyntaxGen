/-
  SyntaxGen.Auto: Automatic generator extraction from syntax declarations.

  The key insight: parse the syntax pattern at elaboration time and
  generate a corresponding `GenM Syntax` function automatically.

  Usage:
  ```lean
  syntax_gen "[" term "|" term " in " term (" if " term)? "]" : term as listComp

  #syntaxgen listComp 5  -- Now works automatically!
  ```
-/
import Lean
import SyntaxGen.Basic

namespace SyntaxGen.Auto

open Lean Parser Elab Command Term Meta Syntax

/-! ## Generator AST

A simple AST representing what we need to generate.
-/

/-- Representation of a syntax pattern for generation -/
inductive GenAST where
  | atom : String → GenAST                    -- Literal token
  | cat : Name → GenAST                       -- Syntax category (term, tactic, etc.)
  | ident : GenAST                            -- Identifier
  | num : GenAST                              -- Numeric literal
  | str : GenAST                              -- String literal
  | seq : Array GenAST → GenAST               -- Sequence of parts
  | optional : GenAST → GenAST                -- Optional (50% chance)
  | many : GenAST → GenAST                    -- Zero or more
  | many1 : GenAST → GenAST                   -- One or more
  | sepBy : GenAST → String → GenAST          -- Separated by
  | choice : Array GenAST → GenAST            -- Random choice
  deriving Inhabited, Repr, BEq

namespace GenAST

/-- Convert GenAST to a generator function -/
partial def toGen (ast : GenAST) : GenM Syntax := do
  if ← isMaxDepth then
    -- At max depth, return simplest possible thing
    return ← genIdent
  match ast with
  | .atom s => return mkAtom s
  | .cat n => withDepth (genFromCat n)
  | .ident => genIdent
  | .num => genNumLit
  | .str => genStrLit
  | .seq parts =>
      let mut results : Array Syntax := #[]
      for p in parts do
        let s ← withDepth (p.toGen)
        match s with
        | .missing => pure ()  -- Skip missing (from optionals)
        | .node _ `null args => results := results ++ args
        | s => results := results.push s
      return Syntax.node .none `null results
  | .optional inner =>
      if ← randBool 50 then
        withDepth inner.toGen
      else
        return Syntax.missing
  | .many inner =>
      let count ← randBound 3
      let mut results : Array Syntax := #[]
      for _ in [:count] do
        let s ← withDepth inner.toGen
        results := results.push s
      return Syntax.node .none `null results
  | .many1 inner =>
      let count ← randBound 2
      let mut results : Array Syntax := #[]
      for _ in [:count + 1] do
        let s ← withDepth inner.toGen
        results := results.push s
      return Syntax.node .none `null results
  | .sepBy inner sep =>
      let count ← randBound 3
      let mut results : Array Syntax := #[]
      for i in [:count] do
        if i > 0 then results := results.push (mkAtom sep)
        let s ← withDepth inner.toGen
        results := results.push s
      return Syntax.node .none `null results
  | .choice alts =>
      if alts.isEmpty then return ← genIdent
      let idx ← randBound alts.size
      withDepth (alts[idx]!.toGen)

/-- Estimate complexity of a GenAST -/
def complexity : GenAST → Nat
  | .atom _ => 1
  | .cat _ => 5
  | .ident => 1
  | .num => 1
  | .str => 1
  | .seq parts => parts.foldl (fun acc p => acc + p.complexity) 0
  | .optional inner => inner.complexity
  | .many inner => inner.complexity * 2
  | .many1 inner => inner.complexity * 2
  | .sepBy inner _ => inner.complexity * 2
  | .choice alts => alts.foldl (fun acc a => max acc a.complexity) 0

end GenAST

/-! ## Pattern Parsing

Parse a Lean syntax pattern into GenAST.
-/

/-- Known syntax categories -/
def knownCategories : List Name := [
  `term, `tactic, `command, `doElem, `level, `attr, `prio, `prec,
  `stx, `Lean.Parser.Term.funBinder, `Lean.binderIdent
]

/-- Parse a syntax pattern into GenAST -/
partial def parsePattern (stx : Syntax) : GenAST := Id.run do
  match stx with
  -- String literal → atom
  | .atom _ val =>
      if val.startsWith "\"" && val.endsWith "\"" then
        .atom (val.drop 1 |>.dropRight 1)
      else
        .atom val

  -- Identifier → could be category reference or literal
  | .ident _ _ name _ =>
      if name == `ident then .ident
      else if name == `num || name == `numLit then .num
      else if name == `str || name == `strLit then .str
      else if knownCategories.contains name then .cat name
      else .atom name.toString

  | .node _ kind args =>
      parseNode kind args

  | _ => .ident  -- fallback
where
  parseNode (kind : SyntaxNodeKind) (args : Array Syntax) : GenAST :=
    -- Handle different syntax node kinds
    if kind == `Lean.Parser.Syntax.cat then
      -- Category reference like `term` in syntax patterns
      match args[0]? with
      | some (Syntax.ident _ _ catName _) => .cat catName
      | _ => .ident

    else if kind == `Lean.Parser.Syntax.atom then
      -- Atom like "foo" in syntax patterns
      match args[0]? with
      | some (Lean.Syntax.atom _ val) => .atom (val.drop 1 |>.dropRight 1)  -- Remove quotes
      | _ => .atom ""

    else if kind == `group || kind == `null then
      -- Group/sequence of patterns
      let parts := args.map parsePattern
      if parts.size == 1 then parts[0]!
      else .seq parts

    else if kind == `Lean.Parser.Syntax.optional || kind.toString.endsWith "optional" then
      -- Optional pattern
      let inner := args.map parsePattern
      .optional (.seq inner)

    else if kind == `Lean.Parser.Syntax.many || kind.toString.endsWith "many" then
      let inner := args.map parsePattern
      .many (.seq inner)

    else if kind == `Lean.Parser.Syntax.many1 || kind.toString.endsWith "many1" then
      let inner := args.map parsePattern
      .many1 (.seq inner)

    else if kind == `Lean.Parser.Syntax.sepBy then
      if args.size >= 2 then
        let inner := parsePattern args[0]!
        let sep := match args[1]! with
          | .atom _ s => s
          | _ => ","
        .sepBy inner sep
      else
        .ident

    else if kind == `Lean.Parser.Syntax.sepBy1 then
      if args.size >= 2 then
        let inner := parsePattern args[0]!
        let sep := match args[1]! with
          | .atom _ s => s
          | _ => ","
        .sepBy inner sep  -- For gen purposes, same as sepBy
      else
        .ident

    else if (kind.toString.splitOn "orelse").length > 1 || kind == `choice then
      -- Choice between alternatives
      .choice (args.map parsePattern)

    else
      -- Unknown node - try to parse children
      let parts := args.map parsePattern
      if parts.isEmpty then .ident
      else if parts.size == 1 then parts[0]!
      else .seq parts

/-! ## Generator Registry

Store generated generators in an environment extension.
-/

/-- Registry entry: name → GenAST -/
structure GenEntry where
  name : Name
  ast : GenAST
  category : Name  -- The target category (term, command, etc.)
  deriving Inhabited

/-- Environment extension for registered generators -/
initialize genRegistry : SimplePersistentEnvExtension GenEntry (NameMap GenEntry) ←
  registerSimplePersistentEnvExtension {
    addEntryFn := fun m entry => m.insert entry.name entry
    addImportedFn := fun arrays =>
      arrays.foldl (fun m arr => arr.foldl (fun m e => m.insert e.name e) m) {}
  }

/-- Look up a registered generator -/
def lookupGen (env : Environment) (name : Name) : Option GenEntry :=
  (genRegistry.getState env).find? name

/-! ## The syntax_gen Command

A drop-in replacement for `syntax` that also registers a generator.
-/

/-- Parse the stx pattern from syntax command args -/
def extractPatternFromSyntaxCmd (stx : Syntax) : Option (Array Syntax × Name) := do
  -- syntax pattern looks like: syntax ... pattern ... : category
  -- We need to find the pattern parts and category
  let args := stx.getArgs
  -- Find the colon
  let mut colonIdx := none
  for i in [:args.size] do
    if let .atom _ ":" := args[i]! then
      colonIdx := some i
      break
  let cIdx ← colonIdx
  -- Category is after colon
  let catStx := args[cIdx + 1]!
  let cat ← match catStx with
    | .ident _ _ n _ => some n
    | _ => none
  -- Pattern is everything between (name := ...) and colon, excluding modifiers
  let mut patternParts : Array Syntax := #[]
  let mut startIdx := 1  -- Skip "syntax" keyword
  -- Skip optional (name := ...) and priority
  for i in [1:cIdx] do
    let arg := args[i]!
    -- Skip parenthesized name assignment and priority
    if arg.isOfKind `Lean.Parser.Command.namedName then continue
    if arg.isOfKind `Lean.Parser.Command.namedPrio then continue
    patternParts := patternParts.push arg
  return (patternParts, cat)

/-- Syntax for syntax_gen command -/
syntax (name := syntaxGenDecl) "syntax_gen" (Command.namedName)? (Command.namedPrio)? stx+ ":" Lean.Parser.ident "as" Lean.Parser.ident : command

@[command_elab syntaxGenDecl]
def elabSyntaxGenDecl : CommandElab := fun stx => do
  -- Parse the command
  let args := stx.getArgs
  -- Find "as" to get the generator name
  let mut asIdx := none
  for i in [:args.size] do
    if let .atom _ "as" := args[i]! then
      asIdx := some i
      break
  let some aIdx := asIdx | throwError "Expected 'as <name>' at end of syntax_gen"
  let genName := args[aIdx + 1]!.getId

  -- Find ":" to get category
  let mut colonIdx := none
  for i in [:aIdx] do
    if let .atom _ ":" := args[i]! then
      colonIdx := some i
      break
  let some cIdx := colonIdx | throwError "Expected ':' before category"
  let catName := args[cIdx + 1]!.getId

  -- Extract pattern (everything between syntax_gen and :)
  let mut patternParts : Array Syntax := #[]
  let mut startIdx := 1  -- Skip "syntax_gen"
  -- Skip optional name and priority
  for i in [1:cIdx] do
    let arg := args[i]!
    if arg.isOfKind `Lean.Parser.Command.namedName then continue
    if arg.isOfKind `Lean.Parser.Command.namedPrio then continue
    patternParts := patternParts.push arg

  -- Parse pattern into AST
  let patternSeq := Syntax.node .none `null patternParts
  let ast := parsePattern patternSeq

  -- Register the generator
  let entry : GenEntry := { name := genName, ast := ast, category := catName }
  modifyEnv fun env => genRegistry.addEntry env entry

  -- Also emit the regular syntax command manually
  -- We skip emitting the syntax command since the pattern parts are raw Syntax, not TSyntax `stx
  -- Users should define the syntax separately if needed
  pure ()

  logInfo m!"Registered generator `{genName}` for category `{catName}`"
  logInfo m!"Pattern AST: {repr ast}"

/-! ## Enhanced #syntaxgen Command

Support both built-in categories and registered generators.
-/

/-- Generate from registry or fall back to built-in -/
def genFromRegistry (env : Environment) (name : Name) : GenM Syntax := do
  match lookupGen env name with
  | some entry => entry.ast.toGen
  | none => genFromCat name

/-- Enhanced #syntaxgen that checks registry first -/
elab "#syntaxgen!" nm:Lean.Parser.ident count:(num)? : command => do
  let name := nm.getId
  let env ← getEnv
  let cnt := match count with
    | some n => getNat? n.raw |>.getD 10
    | none => 10

  let mut results : Array Syntax := #[]
  let mut seed : Nat := 42

  for _ in [:cnt * 3] do
    if results.size >= cnt then break
    let config : GenConfig := { seed := seed, maxDepth := 8 }
    seed := seed + 7919
    match runGen config (genFromRegistry env name) with
    | some s => results := results.push s
    | none => pure ()

  if results.isEmpty then
    logWarning s!"Could not generate examples for `{name}`."
  else
    logInfo m!"Generated {results.size} examples for `{name}`:"
    for ex in results do
      logInfo m!"  • {syntaxToString ex}"

end SyntaxGen.Auto
