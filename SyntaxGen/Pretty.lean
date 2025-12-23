/-
  SyntaxGen.Pretty: Pretty printing for generated syntax.
-/
import Lean
import SyntaxGen.Basic

namespace SyntaxGen.Pretty

open Lean Parser Elab Command Term Meta

/-! ## Enhanced Pretty Printing -/

/-- Check if a string contains a substring -/
def containsSub (s sub : String) : Bool :=
  (s.splitOn sub).length > 1

/-- Pretty print with proper spacing -/
partial def prettyPrint (stx : Syntax) : String := Id.run do
  match stx with
  | .missing => ""
  | .atom _ val =>
      if val ∈ ["+", "-", "*", "/", "++", "&&", "||", "==", "!=", "<", ">", "<=", ">=", "=>", ":=", "←", "→"] then
        s!" {val} "
      else if val ∈ [",", ";"] then
        s!"{val} "
      else
        val
  | .ident _ _ name _ => name.toString
  | .node _ _kind args =>
      let parts := args.map prettyPrint |>.toList
      let nonEmpty := parts.filter (·.length > 0)
      joinParts nonEmpty
where
  joinParts (parts : List String) : String := Id.run do
    let mut result := ""
    for p in parts do
      if p.isEmpty then continue
      if result.isEmpty then
        result := p
      else
        let prev := if result.isEmpty then none else some (String.Pos.Raw.get result ⟨result.length - 1⟩)
        let curr := if p.isEmpty then none else some (String.Pos.Raw.get p ⟨0⟩)
        let needSpace := match (prev, curr) with
          | (some '(', _) => false
          | (some '[', _) => false
          | (some '{', _) => false
          | (_, some ')') => false
          | (_, some ']') => false
          | (_, some '}') => false
          | (_, some ',') => false
          | (_, some ';') => false
          | (some ' ', _) => false
          | (_, some ' ') => false
          | _ => true
        if needSpace then result := result ++ " "
        result := result ++ p
    result

/-- Compact single-line format -/
def compact (stx : Syntax) : String :=
  prettyPrint stx |>.replace "  " " "

/-- Normalize syntax by removing redundant null nodes -/
partial def normalize (stx : Syntax) : Syntax :=
  match stx with
  | .node _info `null #[single] => normalize single
  | .node info kind args =>
      let args' := args.map normalize
      Syntax.node info kind args'
  | _ => stx

/-! ## Commands -/

/-- Generate and pretty print -/
elab "#syntaxgen_pretty" cat:Lean.Parser.ident count:(num)? : command => do
  let catName := cat.getId
  let cnt := match count with
    | some n => getNat? n.raw |>.getD 5
    | none => 5

  let mut results : Array String := #[]
  let mut seed : Nat := 42

  for _ in [:cnt * 2] do
    if results.size >= cnt then break
    let config : GenConfig := { seed := seed }
    seed := seed + 7919
    match runGen config (genFromCat catName) with
    | some stx =>
        let normalized := normalize stx
        let pretty := prettyPrint normalized
        if pretty.length > 0 then
          results := results.push pretty
    | none => pure ()

  if results.isEmpty then
    logWarning s!"Could not generate examples for `{catName}`."
  else
    logInfo m!"Generated {results.size} pretty-printed examples for `{catName}`:"
    for ex in results do
      logInfo m!"  {ex}"

end SyntaxGen.Pretty
