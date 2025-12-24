/-
  SyntaxGen.Domain.Pools: Configurable identifier pools by domain.

  Provides domain-specific name pools for realistic syntax generation.
  Domains include: mathlib (proofs), programming (IO/effects), meta (Lean metaprogramming).
-/
import Lean
import SyntaxGen.Basic

namespace SyntaxGen.Domain

open Lean

/-! ## Domain Pools Structure -/

/-- Domain-specific identifier pools for realistic generation -/
structure DomainPools where
  /-- Domain identifier (e.g., "mathlib", "programming", "meta") -/
  name : String
  /-- Type names common in this domain -/
  types : Array String := #[]
  /-- Qualified names (Namespace.function style) -/
  qualifiedNames : Array String := #[]
  /-- Variable names -/
  variables : Array String := #[]
  /-- Hypothesis names (for proofs) -/
  hypotheses : Array String := #[]
  /-- Function names -/
  functions : Array String := #[]
  /-- Tactic names -/
  tactics : Array String := #[]
  /-- Field/method names -/
  fields : Array String := #[]
  /-- Constructor names (for match patterns) -/
  constructors : Array String := #[]
  deriving Repr, Inhabited

/-! ## Built-in Domain Configurations -/

/-- Generic proof-style pools (library-agnostic) -/
def proofPools : DomainPools := {
  name := "proof"
  types := #[
    -- Core types
    "Nat", "Int", "Bool", "Prop", "Type",
    "List", "Array", "Option", "Sum", "Prod",
    -- Type variables
    "α", "β", "γ", "A", "B", "C"
  ]
  qualifiedNames := #[
    -- Generic qualified patterns (not library-specific)
    "A.f", "B.g", "T.mk", "T.rec",
    "H.val", "P.property", "S.mk", "R.intro"
  ]
  variables := #["n", "m", "k", "i", "j", "a", "b", "c", "x", "y", "z"]
  hypotheses := #["h", "h1", "h2", "h3", "hx", "hy", "hn", "hm", "hp", "hq", "this", "ih"]
  functions := #[
    "f", "g", "id", "comp", "map", "filter", "fold",
    "length", "size", "succ", "pred"
  ]
  tactics := #[
    -- Core Lean 4 tactics only (no mathlib)
    "rfl", "trivial", "decide", "simp", "omega",
    "exact", "apply", "intro", "intros", "cases", "induction",
    "rw", "have", "let", "obtain", "use",
    "constructor", "left", "right", "ext", "funext",
    "contradiction", "assumption"
  ]
  fields := #["1", "2", "fst", "snd", "val", "property"]
  constructors := #["zero", "succ", "nil", "cons", "none", "some", "inl", "inr", "mk"]
}

/-- Backwards compatibility alias -/
def mathlibPools : DomainPools := proofPools

/-- Programming-style pools for IO and effects -/
def programmingPools : DomainPools := {
  name := "programming"
  types := #[
    -- Core effect types
    "IO", "String", "Array", "List", "Option", "Except",
    "Bool", "Nat", "Int", "Unit",
    -- Monad transformers
    "StateT", "ReaderT", "ExceptT"
  ]
  qualifiedNames := #[
    -- Generic patterns (not Lean stdlib-specific)
    "M.run", "M.pure", "M.bind",
    "T.get", "T.set", "T.modify",
    "A.map", "A.filter", "A.fold"
  ]
  variables := #["ctx", "env", "state", "config", "result", "input", "output", "data", "s", "x"]
  hypotheses := #["h", "ok", "err", "res"]
  functions := #[
    "main", "run", "process", "handle", "parse", "format",
    "read", "write", "get", "set", "update", "modify"
  ]
  tactics := #["rfl", "simp", "decide", "exact", "apply"]
  fields := #["val", "get", "set", "toList", "toArray", "toString"]
  constructors := #["ok", "error", "none", "some", "nil", "cons"]
}

/-- Metaprogramming pools for Lean meta code -/
def metaPools : DomainPools := {
  name := "meta"
  types := #[
    -- Core meta types
    "Expr", "Syntax", "Name", "Level",
    -- Monads (generic patterns)
    "M", "CoreM", "MetaM", "TermElabM", "TacticM"
  ]
  qualifiedNames := #[
    -- Generic meta patterns
    "M.run", "M.get", "M.set",
    "Expr.app", "Expr.const", "Syntax.node"
  ]
  variables := #["stx", "e", "expr", "ty", "type", "ctx", "env", "name", "info"]
  hypotheses := #["h", "heq", "hty"]
  functions := #[
    "elab", "check", "infer", "reduce", "whnf",
    "mkApp", "mkConst", "mkLambda", "mkForall"
  ]
  tactics := #["rfl", "simp", "exact", "apply", "trivial"]
  fields := #["kind", "args", "raw", "val"]
  constructors := #["app", "const", "lam", "lit", "node", "atom", "ident"]
}

/-- Get a pool by name -/
def getPoolByName (name : String) : DomainPools :=
  match name with
  | "proof" | "math" | "mathlib" => proofPools
  | "programming" | "prog" | "io" => programmingPools
  | "meta" | "metaprogramming" | "elab" => metaPools
  | _ => proofPools  -- default

/-! ## Pool-Based Generators -/

open SyntaxGen

/-- Generate from a specific pool field -/
def genFromPool (pools : DomainPools) (field : DomainPools → Array String) : GenM Syntax := do
  let arr := field pools
  if arr.isEmpty then
    genIdent  -- fallback to basic ident
  else
    let name ← randChoice arr
    return mkIdent' name

/-- Generate a type name from the pool -/
def genType (pools : DomainPools) : GenM Syntax := genFromPool pools (·.types)

/-- Generate a qualified name from the pool -/
def genQualifiedName (pools : DomainPools) : GenM Syntax := do
  let roll ← randBound 100
  if roll < 70 && !pools.qualifiedNames.isEmpty then
    -- Use a known qualified name
    let name ← randChoice pools.qualifiedNames
    return mkIdent' name
  else
    -- Generate Namespace.function style (use PascalCase names, not Greek letters)
    let namespaces := #["List", "Array", "Option", "Nat", "Int", "Bool", "String", "IO", "T", "M"]
    let ns ← randChoice namespaces
    let fn ← randChoice (if pools.functions.isEmpty then #["bar"] else pools.functions)
    return mkIdent' s!"{ns}.{fn}"

/-- Generate a variable name from the pool -/
def genVariable (pools : DomainPools) : GenM Syntax := genFromPool pools (·.variables)

/-- Generate a hypothesis name from the pool -/
def genHypothesis (pools : DomainPools) : GenM Syntax := genFromPool pools (·.hypotheses)

/-- Generate a function name from the pool -/
def genFunction (pools : DomainPools) : GenM Syntax := genFromPool pools (·.functions)

/-- Generate a tactic name from the pool -/
def genTacticName (pools : DomainPools) : GenM Syntax := do
  if pools.tactics.isEmpty then
    return mkAtom "rfl"
  else
    let name ← randChoice pools.tactics
    return mkAtom name

/-- Generate a field/projection name from the pool -/
def genField (pools : DomainPools) : GenM Syntax := do
  if pools.fields.isEmpty then
    return mkAtom "1"
  else
    let name ← randChoice pools.fields
    return mkAtom name

/-- Generate a constructor name from the pool -/
def genConstructor (pools : DomainPools) : GenM Syntax := genFromPool pools (·.constructors)

/-- Generate a fresh hypothesis name with index -/
def genFreshHypothesis (pools : DomainPools) (index : Nat) : GenM Syntax := do
  let base ← randChoice (if pools.hypotheses.isEmpty then #["h"] else pools.hypotheses)
  if index == 0 then
    return mkIdent' base
  else
    return mkIdent' s!"{base}{index}"

/-- Generate a typed variable name based on type hint -/
def genTypedVar (pools : DomainPools) (typeHint : Option String := none) : GenM Syntax := do
  match typeHint with
  | some "Nat" | some "Int" =>
      let name ← randChoice #["n", "m", "k", "i", "j"]
      return mkIdent' name
  | some "Bool" =>
      let name ← randChoice #["b", "c", "p", "q"]
      return mkIdent' name
  | some "Prop" =>
      let name ← randChoice #["P", "Q", "R", "S"]
      return mkIdent' name
  | some "List" | some "Array" =>
      let name ← randChoice #["xs", "ys", "zs", "as", "bs"]
      return mkIdent' name
  | some "String" =>
      let name ← randChoice #["s", "t", "str", "msg"]
      return mkIdent' name
  | _ => genVariable pools

end SyntaxGen.Domain
