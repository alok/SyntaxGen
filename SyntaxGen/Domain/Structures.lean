/-
  SyntaxGen.Domain.Structures: Declaration-level syntax generators.

  Generates well-formed structure, inductive, class, instance, and abbrev declarations.
-/
import Lean
import SyntaxGen.Basic
import SyntaxGen.Domain.Pools

namespace SyntaxGen.Domain

open Lean SyntaxGen

/-! ## Name Generators for Declarations -/

/-- Generate a type name (PascalCase) -/
def genTypeName : GenM Syntax := do
  let prefixes := #["My", "Custom", "New", "Base", "Simple", ""]
  let bases := #["Config", "State", "Data", "Info", "Context", "Options", "Result", "Error", "Tree", "Node", "List", "Pair"]
  let pfx ← randChoice prefixes
  let base ← randChoice bases
  return mkIdent' s!"{pfx}{base}"

/-- Generate a field name (camelCase) -/
def genFieldName : GenM Syntax := do
  let names := #["name", "value", "data", "info", "count", "size", "index", "level", "depth", "parent", "child", "left", "right", "key", "result", "status", "enabled", "visible"]
  let name ← randChoice names
  return mkIdent' name

/-- Generate a constructor name -/
def genCtorName : GenM Syntax := do
  let names := #["mk", "new", "default", "empty", "nil", "cons", "leaf", "node", "zero", "succ", "inl", "inr", "some", "none", "ok", "error"]
  let name ← randChoice names
  return mkIdent' name

/-- Generate a class name -/
def genClassName : GenM Syntax := do
  let names := #["MyClass", "HasToString", "IsValid", "CanParse", "Serializable", "Comparable", "Printable", "Processable"]
  let name ← randChoice names
  return mkIdent' name

/-- Generate a simple type expression -/
def genSimpleType (pools : DomainPools) : GenM Syntax := do
  let roll ← randBound 100
  if roll < 60 then
    genType pools
  else if roll < 80 then
    -- List α, Array β, Option γ
    let container ← randChoice #["List", "Array", "Option"]
    let inner ← randChoice #["α", "β", "Nat", "String"]
    return Syntax.node .none `null #[mkIdent' container, mkIdent' inner]
  else
    -- Function type
    let a ← randChoice #["α", "Nat", "String"]
    let b ← randChoice #["β", "Bool", "Unit"]
    return Syntax.node .none `null #[mkIdent' a, mkAtom "→", mkIdent' b]

/-! ## Structure Generator -/

/-- Generate a structure field: `name : Type` or `name : Type := default` -/
def genStructField (pools : DomainPools) : GenM Syntax := do
  let name ← genFieldName
  let ty ← genSimpleType pools
  let roll ← randBound 100

  if roll < 70 then
    -- Simple field
    return Syntax.node .none `null #[name, mkAtom " :", ty]
  else
    -- Field with default
    let default ← randChoice #["default", "0", "\"\"", "true", "false", "#[]", "none"]
    return Syntax.node .none `null #[name, mkAtom " :", ty, mkAtom " :=", mkAtom default]

/-- Generate a structure declaration -/
partial def genStructure (pools : DomainPools) : GenM Syntax := do
  let name ← genTypeName
  let numFields ← randBound 3  -- 1-3 fields
  let roll ← randBound 100

  -- Type parameters
  let hasTypeParam := roll < 40
  let typeParam := if hasTypeParam then
    Syntax.node .none `null #[mkAtom "(", mkIdent' "α", mkAtom " :", mkAtom "Type", mkAtom ")"]
  else
    Syntax.missing

  let mut fields : Array Syntax := #[mkAtom "structure", name]
  if hasTypeParam then fields := fields.push typeParam
  fields := fields.push (mkAtom "where")

  for i in [:numFields + 1] do
    if i > 0 then fields := fields.push (mkAtom ";")
    let field ← genStructField pools
    fields := fields.push field

  return Syntax.node .none `null fields

/-! ## Inductive Generator -/

/-- Generate an inductive constructor: `| ctor : Type → InductiveName` -/
def genInductiveCtor (pools : DomainPools) (inductiveName : Syntax) (hasTypeParam : Bool) : GenM Syntax := do
  let ctorName ← genCtorName
  let roll ← randBound 100

  let result := if hasTypeParam then
    Syntax.node .none `null #[inductiveName, mkIdent' "α"]
  else
    inductiveName

  if roll < 30 then
    -- Nullary constructor: | nil : List α
    return Syntax.node .none `null #[mkAtom "|", ctorName, mkAtom ":", result]
  else if roll < 60 then
    -- Unary constructor: | cons : α → List α → List α
    let argTy ← genSimpleType pools
    return Syntax.node .none `null #[
      mkAtom "|", ctorName, mkAtom ":", argTy, mkAtom "→", result
    ]
  else
    -- Binary constructor: | node : Tree α → Tree α → Tree α
    return Syntax.node .none `null #[
      mkAtom "|", ctorName, mkAtom ":", result, mkAtom "→", result, mkAtom "→", result
    ]

/-- Generate an inductive declaration -/
partial def genInductive (pools : DomainPools) : GenM Syntax := do
  let name ← genTypeName
  let numCtors ← randBound 2  -- 1-2 constructors
  let roll ← randBound 100

  let hasTypeParam := roll < 50
  let typeParam := if hasTypeParam then
    Syntax.node .none `null #[mkAtom "(", mkIdent' "α", mkAtom " :", mkAtom "Type", mkAtom ")"]
  else
    Syntax.missing

  let mut result : Array Syntax := #[mkAtom "inductive", name]
  if hasTypeParam then result := result.push typeParam
  result := result.push (mkAtom "where")

  for _ in [:numCtors + 1] do
    let ctor ← genInductiveCtor pools name hasTypeParam
    result := result.push ctor

  return Syntax.node .none `null result

/-! ## Class Generator -/

/-- Generate a class method: `methodName : Type` -/
def genClassMethod (pools : DomainPools) : GenM Syntax := do
  let name ← genFieldName
  let ty ← genSimpleType pools
  return Syntax.node .none `null #[name, mkAtom " :", ty]

/-- Generate a class declaration -/
partial def genClass (pools : DomainPools) : GenM Syntax := do
  let name ← genClassName
  let numMethods ← randBound 2  -- 1-2 methods
  let roll ← randBound 100

  -- Class parameter
  let hasTypeParam := roll < 60
  let classParam := if hasTypeParam then
    Syntax.node .none `null #[mkAtom "(", mkIdent' "α", mkAtom " :", mkAtom "Type", mkAtom ")"]
  else
    Syntax.node .none `null #[mkAtom "(", mkIdent' "m", mkAtom " :", mkAtom "Type", mkAtom "→", mkAtom "Type", mkAtom ")"]

  let mut result : Array Syntax := #[mkAtom "class", name, classParam, mkAtom "where"]

  for i in [:numMethods + 1] do
    if i > 0 then result := result.push (mkAtom ";")
    let method ← genClassMethod pools
    result := result.push method

  return Syntax.node .none `null result

/-! ## Instance Generator -/

/-- Generate an instance implementation field -/
def genInstanceField (pools : DomainPools) : GenM Syntax := do
  let name ← genFieldName
  let impl ← randChoice #["fun x => x", "default", "0", "true", "id", "pure"]
  return Syntax.node .none `null #[name, mkAtom ":=", mkAtom impl]

/-- Generate an instance declaration -/
partial def genInstance (pools : DomainPools) : GenM Syntax := do
  let className ← randChoice #["ToString", "Repr", "BEq", "Hashable", "Inhabited", "DecidableEq"]
  let typeName ← genTypeName
  let numFields ← randBound 2  -- 0-2 fields

  let mut result : Array Syntax := #[
    mkAtom "instance", mkAtom ":", mkIdent' className, typeName, mkAtom "where"
  ]

  for _ in [:numFields] do
    let field ← genInstanceField pools
    result := result.push field

  return Syntax.node .none `null result

/-! ## Abbrev Generator -/

/-- Generate an abbrev declaration -/
def genAbbrev (pools : DomainPools) : GenM Syntax := do
  let name ← genTypeName
  let ty ← genSimpleType pools
  return Syntax.node .none `null #[mkAtom "abbrev", name, mkAtom ":=", ty]

/-! ## Def/Theorem Generator -/

/-- Generate a simple def declaration -/
partial def genDef (pools : DomainPools) : GenM Syntax := do
  let name ← genFieldName
  let roll ← randBound 100

  if roll < 40 then
    -- def name := value
    let value ← randChoice #["0", "1", "\"hello\"", "true", "#[]", "default"]
    return Syntax.node .none `null #[mkAtom "def", name, mkAtom ":=", mkAtom value]
  else if roll < 70 then
    -- def name (x : T) := body
    let param ← randChoice #["x", "n", "s"]
    let ty ← genType pools
    let body ← randChoice #["x", "x + 1", "s.length", "default"]
    return Syntax.node .none `null #[
      mkAtom "def", name, mkAtom "(", mkIdent' param, mkAtom " :", ty, mkAtom ")",
      mkAtom " :=", mkAtom body
    ]
  else
    -- def name : T := value
    let ty ← genSimpleType pools
    let value ← randChoice #["default", "0", "\"\"", "true"]
    return Syntax.node .none `null #[
      mkAtom "def", name, mkAtom " :", ty, mkAtom " :=", mkAtom value
    ]

/-- Generate a theorem declaration -/
partial def genTheorem (pools : DomainPools) : GenM Syntax := do
  let name ← genFieldName
  let prop ← randChoice #["True", "a = a", "n = n", "P → P", "∀ x, x = x"]
  let proof ← randChoice #["rfl", "trivial", "fun x => x", "id"]

  return Syntax.node .none `null #[
    mkAtom "theorem", name, mkAtom " :", mkAtom prop, mkAtom " :=", mkAtom proof
  ]

/-! ## Declaration Dispatcher -/

/-- Generate a random declaration -/
partial def genDeclaration (pools : DomainPools) : GenM Syntax := do
  let roll ← randBound 100

  if roll < 25 then genStructure pools
  else if roll < 45 then genInductive pools
  else if roll < 55 then genClass pools
  else if roll < 65 then genInstance pools
  else if roll < 75 then genAbbrev pools
  else if roll < 88 then genDef pools
  else genTheorem pools

end SyntaxGen.Domain
