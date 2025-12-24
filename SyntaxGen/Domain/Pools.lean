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

/-- Mathlib-style pools for mathematical proofs -/
def mathlibPools : DomainPools := {
  name := "mathlib"
  types := #[
    "Nat", "Int", "Real", "Rat", "Complex",
    "Bool", "Prop", "Type",
    "List", "Array", "Set", "Finset", "Multiset",
    "Option", "Sum", "Prod", "Subtype",
    "Fin", "ZMod", "Polynomial",
    "Group", "Ring", "Field", "Module", "Algebra"
  ]
  qualifiedNames := #[
    -- Nat lemmas
    "Nat.add_comm", "Nat.add_assoc", "Nat.mul_comm", "Nat.mul_assoc",
    "Nat.zero_add", "Nat.add_zero", "Nat.one_mul", "Nat.mul_one",
    "Nat.succ_ne_zero", "Nat.succ_pos", "Nat.lt_succ_self",
    -- Int lemmas
    "Int.add_comm", "Int.neg_neg", "Int.sub_self", "Int.add_neg_cancel",
    -- List lemmas
    "List.map_map", "List.filter_filter", "List.length_append",
    "List.reverse_reverse", "List.mem_append", "List.nil_append",
    -- Set lemmas
    "Set.mem_union", "Set.mem_inter", "Set.subset_refl",
    "Set.inter_comm", "Set.union_comm", "Set.empty_subset",
    -- General algebra
    "add_comm", "mul_comm", "add_assoc", "mul_assoc",
    "zero_add", "add_zero", "one_mul", "mul_one",
    "neg_neg", "sub_self", "add_neg_cancel"
  ]
  variables := #["n", "m", "k", "i", "j", "a", "b", "c", "x", "y", "z"]
  hypotheses := #["h", "h1", "h2", "h3", "hx", "hy", "hn", "hm", "hp", "hq", "this", "ih"]
  functions := #[
    "id", "comp", "map", "filter", "foldl", "foldr",
    "length", "size", "card", "sum", "prod",
    "succ", "pred", "abs", "neg", "inv"
  ]
  tactics := #[
    "rfl", "trivial", "decide", "simp", "ring", "linarith", "omega",
    "exact", "apply", "intro", "intros", "cases", "induction",
    "rw", "rewrite", "have", "let", "obtain", "use",
    "constructor", "left", "right", "ext", "funext",
    "contradiction", "absurd", "exfalso", "by_contra",
    "assumption", "norm_num", "positivity", "field_simp"
  ]
  fields := #["1", "2", "fst", "snd", "left", "right", "symm", "trans", "mp", "mpr"]
  constructors := #["zero", "succ", "nil", "cons", "none", "some", "inl", "inr", "true", "false"]
}

/-- Programming-style pools for IO and effects -/
def programmingPools : DomainPools := {
  name := "programming"
  types := #[
    "IO", "String", "Char", "UInt8", "UInt32", "UInt64",
    "Array", "List", "HashMap", "HashSet",
    "Option", "Except", "EStateM", "ReaderT", "StateT",
    "Bool", "Nat", "Int", "Float",
    "FilePath", "Handle", "Task", "BaseIO"
  ]
  qualifiedNames := #[
    -- IO operations
    "IO.println", "IO.print", "IO.getLine", "IO.getStdin", "IO.getStdout",
    "IO.FS.readFile", "IO.FS.writeFile", "IO.FS.Handle.getLine",
    -- String operations
    "String.append", "String.length", "String.push", "String.mk",
    "String.toList", "String.isEmpty", "String.trim", "String.splitOn",
    -- Array operations
    "Array.push", "Array.pop", "Array.get!", "Array.set!",
    "Array.size", "Array.empty", "Array.map", "Array.filter",
    -- Option/Except
    "Option.get!", "Option.getD", "Option.map", "Option.bind",
    "Except.ok", "Except.error", "Except.map",
    -- Control flow
    "pure", "bind", "throw", "catch", "try"
  ]
  variables := #["ctx", "env", "state", "config", "result", "input", "output", "data", "buf", "s"]
  hypotheses := #["h", "ok", "err", "res"]
  functions := #[
    "main", "run", "process", "handle", "parse", "format",
    "read", "write", "open", "close", "init", "setup",
    "get", "set", "update", "modify", "with"
  ]
  tactics := #["rfl", "simp", "decide", "native_decide", "exact", "apply"]
  fields := #["val", "ref", "get", "set", "modify", "toList", "toArray", "toString"]
  constructors := #["ok", "error", "none", "some", "nil", "cons", "unit"]
}

/-- Metaprogramming pools for Lean meta code -/
def metaPools : DomainPools := {
  name := "meta"
  types := #[
    "Expr", "Syntax", "Name", "Level", "MVarId", "FVarId", "LVarId",
    "LocalDecl", "ConstantInfo", "Environment", "LocalContext",
    "MetaM", "TermElabM", "TacticM", "CommandElabM", "CoreM",
    "Macro", "MacroM", "TSyntax", "Ident"
  ]
  qualifiedNames := #[
    -- Meta operations
    "Lean.Meta.inferType", "Lean.Meta.isDefEq", "Lean.Meta.whnf",
    "Lean.Meta.mkFreshExprMVar", "Lean.Meta.instantiateMVars",
    -- Elab operations
    "Lean.Elab.Term.elabTerm", "Lean.Elab.Term.withSynthesize",
    "Lean.Elab.Tactic.evalTactic", "Lean.Elab.Command.elabCommand",
    -- Syntax operations
    "Lean.Syntax.getKind", "Lean.Syntax.getArgs", "Lean.Syntax.isOfKind",
    "Lean.Syntax.mkApp", "Lean.Syntax.mkLit",
    -- Environment
    "Lean.Environment.find?", "Lean.Environment.addDecl",
    -- Core
    "Lean.getEnv", "Lean.getRef", "Lean.throwError", "Lean.logInfo"
  ]
  variables := #["stx", "e", "expr", "ty", "type", "ctx", "lctx", "mctx", "env", "name", "info"]
  hypotheses := #["h", "heq", "hty"]
  functions := #[
    "elabTerm", "elabType", "elabTactic", "elabCommand",
    "inferType", "isDefEq", "whnf", "reduce",
    "mkApp", "mkConst", "mkLambda", "mkForall",
    "withLocalDecl", "withMVarContext", "withLCtx"
  ]
  tactics := #["rfl", "simp", "exact", "apply", "trivial"]
  fields := #["kind", "args", "raw", "getId", "getNat", "getIdent"]
  constructors := #["app", "const", "fvar", "mvar", "lam", "forallE", "lit", "sort"]
}

/-- Get a pool by name -/
def getPoolByName (name : String) : DomainPools :=
  match name with
  | "mathlib" | "math" | "proof" => mathlibPools
  | "programming" | "prog" | "io" => programmingPools
  | "meta" | "metaprogramming" | "elab" => metaPools
  | _ => mathlibPools  -- default

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
    -- Generate Namespace.function style
    let ty ← randChoice (if pools.types.isEmpty then #["Foo"] else pools.types)
    let fn ← randChoice (if pools.functions.isEmpty then #["bar"] else pools.functions)
    return mkIdent' s!"{ty}.{fn}"

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
