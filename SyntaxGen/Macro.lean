/-
  SyntaxGen.Macro: Template-based syntax generation.

  Since ParserDescr is compiled away, we provide a template-based approach
  where users can define generators with a simple template syntax.
-/
import Lean
import SyntaxGen.Basic

namespace SyntaxGen.Macro

open Lean Parser Elab Command Term Meta

/-!
# Template-based Generation

Templates define what a syntax looks like with placeholders for categories.
-/

/-- A template for generating syntax -/
structure GenTemplate where
  /-- Template parts: Left = literal, Right = category to generate -/
  parts : Array (String ⊕ Name)
  deriving Inhabited, Repr

namespace GenTemplate

/-- Create a generator from a template -/
def toGen (t : GenTemplate) : GenM Syntax := do
  let mut results : Array Syntax := #[]
  for part in t.parts do
    match part with
    | .inl s => results := results.push (mkAtom s)
    | .inr cat => results := results.push (← genFromCat cat)
  return Syntax.node .none `null results

/-- Helper to build templates -/
def lit (s : String) : String ⊕ Name := .inl s
def cat (n : Name) : String ⊕ Name := .inr n

/-- Build a template from parts -/
def mk' (parts : List (String ⊕ Name)) : GenTemplate := { parts := parts.toArray }

end GenTemplate

/-! ## Predefined Templates -/

/-- List comprehension: {lit}`[ expr | item in items ]` -/
def listCompSimple : GenTemplate := GenTemplate.mk' [
  .inl "[", .inr `term, .inl " | ", .inr `term, .inl " in ", .inr `term, .inl "]"
]

/-- List comprehension with filter: {lit}`[ expr | item in items if pred ]` -/
def listCompFilter : GenTemplate := GenTemplate.mk' [
  .inl "[", .inr `term, .inl " | ", .inr `term, .inl " in ", .inr `term,
  .inl " if ", .inr `term, .inl "]"
]

/-- Generate list comprehension (randomly with or without filter) -/
def genListComp : GenM Syntax := do
  if ← randBool 50 then
    listCompFilter.toGen
  else
    listCompSimple.toGen

/-- Point 2D: point(x, y) -/
def point2dTemplate : GenTemplate := GenTemplate.mk' [
  .inl "point", .inl "(", .inr `num, .inl ",", .inr `num, .inl ")"
]

/-- Point 3D: vec(x, y, z) -/
def vec3dTemplate : GenTemplate := GenTemplate.mk' [
  .inl "vec", .inl "(", .inr `num, .inl ",", .inr `num, .inl ",", .inr `num, .inl ")"
]

/-- #mycheck command -/
def myCheckTemplate : GenTemplate := GenTemplate.mk' [
  .inl "#mycheck", .inr `term
]

/-! ## Template Registry -/

/-- Get generator for a known template name -/
def getTemplateGen (name : Name) : Option (GenM Syntax) :=
  match name with
  | `listComp | `listComprehension => some genListComp
  | `point | `point2d => some point2dTemplate.toGen
  | `vec | `vec3d => some vec3dTemplate.toGen
  | `myCheck | `mycheck => some myCheckTemplate.toGen
  | _ => none

/-! ## Command -/

/-- Generate examples from a template -/
elab "#gen_template" nm:Lean.Parser.ident count:(num)? : command => do
  let name := nm.getId
  let cnt := match count with
    | some n => getNat? n.raw |>.getD 5
    | none => 5

  let gen := getTemplateGen name |>.getD (genFromCat name)

  let mut results : Array Syntax := #[]
  let mut seed : Nat := 42

  for _ in [:cnt * 2] do
    if results.size >= cnt then break
    let config : GenConfig := { seed := seed }
    seed := seed + 7919
    match runGen config gen with
    | some s => results := results.push s
    | none => pure ()

  if results.isEmpty then
    logWarning s!"Could not generate examples for template `{name}`."
  else
    logInfo m!"Generated {results.size} examples for `{name}` template:"
    for ex in results do
      logInfo m!"  • {syntaxToString ex}"

/-!
# Elaboration-based Template Definition

Users can define templates using an elab rule that captures the structure.
-/

/-- Syntax for defining templates inline -/
syntax (name := defgen) "#defgen" Lean.Parser.ident ":=" str ("%" Lean.Parser.ident)* : command

/-- Elaborate template definition -/
@[command_elab defgen]
def elabDefGen : CommandElab := fun stx => do
  let name := stx[1].getId
  -- For now just log what we're defining
  logInfo m!"Defined template `{name}` (templates are predefined in this version)"

end SyntaxGen.Macro
