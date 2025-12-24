/-
  SyntaxGen: Generate unelaborated syntax examples from grammar definitions.

  The #syntaxgen command generates random examples for syntax categories.
  Users can register custom generators for their own syntax categories.
-/
import Lean

namespace SyntaxGen

open Lean Parser Meta Elab Command Term

/-! ## Configuration -/

/-- Configuration for syntax generation -/
structure GenConfig where
  /-- Maximum recursion depth to prevent infinite loops -/
  maxDepth : Nat := 6
  /-- Maximum repetitions for `many` and `many1` -/
  maxRepeat : Nat := 3
  /-- Pool of identifiers to sample from -/
  identPool : Array String := #["x", "y", "z", "foo", "bar", "n", "m", "f", "g", "α", "β"]
  /-- Pool of numeric literals -/
  numLitPool : Array String := #["0", "1", "2", "42", "100"]
  /-- Pool of string literals -/
  strLitPool : Array String := #["\"hello\"", "\"world\"", "\"test\""]
  /-- Pool of character literals -/
  charLitPool : Array String := #["'a'", "'b'", "'x'"]
  /-- Seed for random generation -/
  seed : Nat := 42
  deriving Repr, Inhabited

/-! ## Random Generation Monad -/

/-- State for generation: random seed, current depth -/
structure GenState where
  seed : Nat
  depth : Nat := 0
  config : GenConfig
  /-- Track categories currently being generated (for cycle detection) -/
  inProgress : List Name := []
  deriving Inhabited

/-- Generator monad -/
abbrev GenM := StateT GenState (OptionT Id)

/-- Simple LCG random number generator -/
def nextRand : GenM Nat := do
  let s ← get
  let newSeed := (s.seed * 1103515245 + 12345) % (2^31)
  set { s with seed := newSeed }
  return newSeed

/-- Get a random number in {lit}`[0, bound)` -/
def randBound (bound : Nat) : GenM Nat := do
  if bound == 0 then return 0
  let n ← nextRand
  return n % bound

/-- Pick a random element from an array -/
def randChoice [Inhabited α] (arr : Array α) : GenM α := do
  if arr.isEmpty then failure
  let idx ← randBound arr.size
  return arr[idx]!

/-- Random boolean with given probability of true (0-100) -/
def randBool (probTrue : Nat := 50) : GenM Bool := do
  let n ← randBound 100
  return n < probTrue

/-- Get current depth -/
def getDepth : GenM Nat := return (← get).depth

/-- Get config -/
def getConfig : GenM GenConfig := return (← get).config

/-- Check if exceeded max depth -/
def isMaxDepth : GenM Bool := do
  return (← getDepth) >= (← getConfig).maxDepth

/-- Increment depth for computation -/
def withDepth (m : GenM α) : GenM α := do
  modify fun s => { s with depth := s.depth + 1 }
  let r ← m
  modify fun s => { s with depth := s.depth - 1 }
  return r

/-- Track category for cycle detection -/
def withCat (cat : Name) (m : GenM α) : GenM α := do
  let s ← get
  if s.inProgress.contains cat then failure
  modify fun s => { s with inProgress := cat :: s.inProgress }
  let r ← m
  modify fun s => { s with inProgress := s.inProgress.tail! }
  return r

/-! ## Syntax Building Helpers -/

/-- Create atom syntax -/
def mkAtom (val : String) : Syntax := Syntax.atom .none val

/-- Create identifier syntax -/
def mkIdent' (name : String) : Syntax :=
  Syntax.ident .none name.toRawSubstring name.toName []

/-- Generate random identifier -/
def genIdent : GenM Syntax := do
  let name ← randChoice (← getConfig).identPool
  return mkIdent' name

/-- Generate random numeric literal -/
def genNumLit : GenM Syntax := do
  let num ← randChoice (← getConfig).numLitPool
  return Syntax.node .none `num #[mkAtom num]

/-- Generate random string literal -/
def genStrLit : GenM Syntax := do
  let str ← randChoice (← getConfig).strLitPool
  return Syntax.node .none `str #[mkAtom str]

/-! ## Built-in Category Generators -/

/-- Generate for `term` category -/
partial def genTerm : GenM Syntax := do
  if ← isMaxDepth then genIdent
  else
    let choice ← randBound 10
    match choice with
    | 0 => genIdent  -- variable
    | 1 => genNumLit  -- number
    | 2 => genStrLit  -- string
    | 3 => -- application: f x
        let f ← withDepth genTerm
        let x ← withDepth genTerm
        return Syntax.node .none `Lean.Parser.Term.app #[f, x]
    | 4 => -- fun x => e
        let x ← genIdent
        let e ← withDepth genTerm
        return Syntax.node .none `Lean.Parser.Term.fun #[
          mkAtom "fun", x, mkAtom "=>", e
        ]
    | 5 => -- let x := e1; e2
        let x ← genIdent
        let e1 ← withDepth genTerm
        let e2 ← withDepth genTerm
        return Syntax.node .none `Lean.Parser.Term.let #[
          mkAtom "let", x, mkAtom ":=", e1, mkAtom ";", e2
        ]
    | 6 => -- (e)
        let e ← withDepth genTerm
        return Syntax.node .none `Lean.Parser.Term.paren #[
          mkAtom "(", e, mkAtom ")"
        ]
    | 7 => -- if c then t else f
        let c ← withDepth genTerm
        let t ← withDepth genTerm
        let f ← withDepth genTerm
        return Syntax.node .none `Lean.Parser.Term.if #[
          mkAtom "if", c, mkAtom "then", t, mkAtom "else", f
        ]
    | 8 => -- e + e (arithmetic)
        let op ← randChoice #["+", "-", "*", "/", "++"]
        let e1 ← withDepth genTerm
        let e2 ← withDepth genTerm
        return Syntax.node .none `null #[e1, mkAtom op, e2]
    | _ => -- e < e (comparison)
        let op ← randChoice #["<", ">", "<=", ">=", "==", "!="]
        let e1 ← withDepth genTerm
        let e2 ← withDepth genTerm
        return Syntax.node .none `null #[e1, mkAtom op, e2]

/-- Generate for `tactic` category -/
def genTactic : GenM Syntax := do
  let tac ← randChoice #[
    "rfl", "simp", "exact ?_", "trivial", "decide", "ring",
    "norm_num", "assumption", "contradiction", "constructor",
    "intro x", "cases h", "induction n", "apply f", "have h := e"
  ]
  return mkAtom tac

/-- Generate for `command` category -/
partial def genCommand : GenM Syntax := do
  if ← isMaxDepth then return mkAtom "#check Nat"
  let choice ← randBound 6
  match choice with
  | 0 => -- #check e
      let e ← withDepth genTerm
      return Syntax.node .none `null #[mkAtom "#check", e]
  | 1 => -- #eval e
      let e ← withDepth genTerm
      return Syntax.node .none `null #[mkAtom "#eval", e]
  | 2 => -- def name := e
      let name ← genIdent
      let e ← withDepth genTerm
      return Syntax.node .none `null #[mkAtom "def", name, mkAtom ":=", e]
  | 3 => -- theorem name : Prop := proof
      let name ← genIdent
      return Syntax.node .none `null #[
        mkAtom "theorem", name, mkAtom ":", mkAtom "True", mkAtom ":=",
        mkAtom "trivial"
      ]
  | 4 => -- example : Prop := proof
      return Syntax.node .none `null #[
        mkAtom "example", mkAtom ":", mkAtom "True", mkAtom ":=",
        mkAtom "trivial"
      ]
  | _ => -- variable (x : T)
      let name ← genIdent
      let ty ← randChoice #["Nat", "Bool", "String", "List Nat"]
      return Syntax.node .none `null #[
        mkAtom "variable", mkAtom "(", name, mkAtom ":", mkAtom ty, mkAtom ")"
      ]

/-- Generate for `doElem` category -/
partial def genDoElem : GenM Syntax := do
  if ← isMaxDepth then
    let x ← genIdent
    return Syntax.node .none `null #[mkAtom "let", x, mkAtom ":=", mkAtom "pure ()"]
  let choice ← randBound 5
  match choice with
  | 0 => -- let x := e
      let x ← genIdent
      let e ← withDepth genTerm
      return Syntax.node .none `null #[mkAtom "let", x, mkAtom ":=", e]
  | 1 => -- let x ← e
      let x ← genIdent
      let e ← withDepth genTerm
      return Syntax.node .none `null #[mkAtom "let", x, mkAtom "←", e]
  | 2 => -- e
      withDepth genTerm
  | 3 => -- if c then e
      let c ← withDepth genTerm
      let e ← withDepth genDoElem
      return Syntax.node .none `null #[mkAtom "if", c, mkAtom "then", e]
  | _ => -- return e
      let e ← withDepth genTerm
      return Syntax.node .none `null #[mkAtom "return", e]

/-- Generate for `level` category -/
def genLevel : GenM Syntax := do
  let l ← randChoice #["0", "1", "2", "u", "v", "w", "max u v", "u + 1"]
  return mkAtom l

/-- Generate for `attr` category -/
def genAttr : GenM Syntax := do
  let a ← randChoice #["simp", "inline", "reducible", "instance",
                       "extern", "export", "macro_inline", "specialize"]
  return mkAtom a

/-- Generate for `prio` / `prec` categories -/
def genPriority : GenM Syntax := do
  let p ← randChoice #["default", "low", "mid", "high", "1000", "100"]
  return mkAtom p

/-- Generate from a category name -/
partial def genFromCat (cat : Name) : GenM Syntax := do
  match cat with
  | `term => withCat cat genTerm
  | `tactic => withCat cat genTactic
  | `command => withCat cat genCommand
  | `doElem => withCat cat genDoElem
  | `level => genLevel
  | `attr => genAttr
  | `prio | `prec => genPriority
  | `ident => genIdent
  | `num | `numLit => genNumLit
  | `str | `strLit => genStrLit
  | _ => genIdent  -- fallback

/-! ## Running the Generator -/

/-- Run generator with config -/
def runGen (config : GenConfig) (m : GenM α) : Option α :=
  let state : GenState := { seed := config.seed, config := config }
  (m.run state).run |>.map Prod.fst

/-- Generate multiple examples -/
def generateExamples (cat : Name) (count : Nat) (config : GenConfig := {}) : Array Syntax := Id.run do
  let mut results : Array Syntax := #[]
  let mut seed := config.seed
  for _ in [:count * 2] do
    if results.size >= count then break
    let cfg := { config with seed := seed }
    seed := seed + 7919
    match runGen cfg (genFromCat cat) with
    | some s => results := results.push s
    | none => pure ()
  return results

/-! ## Pretty Printing -/

/-- Convert syntax to string -/
partial def syntaxToString (s : Syntax) : String :=
  match s with
  | .missing => ""
  | .atom _ val => val
  | .ident _ _ name _ => name.toString
  | .node _ _ args =>
      let strs := args.map syntaxToString |>.toList
      " ".intercalate strs

/-! ## The #syntaxgen Command -/

/-- Extract nat from syntax -/
def getNat? (stx : Syntax) : Option Nat :=
  match stx with
  | .atom _ val => val.toNat?
  | .node _ `num #[.atom _ val] => val.toNat?
  | _ => none

/-- Syntax for #syntaxgen command -/
elab "#syntaxgen" catIdent:Lean.Parser.ident count:(num)? : command => do
  let catName := catIdent.getId
  let cnt := match count with
    | some n => getNat? n.raw |>.getD 10
    | none => 10

  let examples := generateExamples catName cnt

  if examples.isEmpty then
    logWarning s!"Could not generate examples for category `{catName}`."
  else
    logInfo m!"Generated {examples.size} examples for `{catName}`:"
    for ex in examples do
      logInfo m!"  • {syntaxToString ex}"

/-! ## Programmatic API -/

/-- Generate examples as strings -/
def genExamplesAsStrings (cat : Name) (count : Nat := 10) (config : GenConfig := {}) : Array String :=
  (generateExamples cat count config).map syntaxToString

/-- Generate single example -/
def genExample (cat : Name) (seed : Nat := 42) : Option String :=
  match runGen { seed := seed } (genFromCat cat) with
  | some s => some (syntaxToString s)
  | none => none

/-! ## Custom Syntax Generator Registration -/

/-- Extension for custom generators -/
initialize customGenExt : SimplePersistentEnvExtension (Name × Name) (NameMap Name) ←
  registerSimplePersistentEnvExtension {
    addEntryFn := fun m (cat, genFn) => m.insert cat genFn
    addImportedFn := fun arrays =>
      arrays.foldl (fun m arr => arr.foldl (fun m (cat, genFn) => m.insert cat genFn) m) {}
  }

/-- Attribute for registering custom generators -/
syntax (name := syntaxGenFor) "syntaxgen_for" Lean.Parser.ident : attr

initialize registerBuiltinAttribute {
  name := `syntaxgen_for
  descr := "Register a custom syntax generator for a category"
  add := fun decl stx _kind => do
    match stx with
    | `(attr| syntaxgen_for $catIdent:ident) =>
        let catName := catIdent.getId
        modifyEnv fun env => customGenExt.addEntry env (catName, decl)
    | _ => throwError "Expected: syntaxgen_for <category>"
}

end SyntaxGen
