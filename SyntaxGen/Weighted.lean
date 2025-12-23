/-
  SyntaxGen.Weighted: Probability-weighted syntax generation.

  Produces more realistic examples by making simpler constructs more common.
-/
import Lean
import SyntaxGen.Basic

namespace SyntaxGen.Weighted

open Lean Parser Elab Command Term Meta

/-! ## Weighted Choice -/

/-- A weighted choice: value paired with relative probability weight -/
structure Weighted (α : Type) where
  value : α
  weight : Nat  -- Higher = more likely
  deriving Inhabited

/-- Pick from weighted choices -/
def weightedChoice [Inhabited α] (choices : Array (Weighted α)) : GenM α := do
  if choices.isEmpty then failure
  let totalWeight := choices.foldl (fun acc c => acc + c.weight) 0
  if totalWeight == 0 then return choices[0]!.value
  let roll ← randBound totalWeight
  let mut cumulative := 0
  for c in choices do
    cumulative := cumulative + c.weight
    if roll < cumulative then
      return c.value
  return choices[choices.size - 1]!.value

/-! ## Weighted Term Generator -/

/-- Weighted term generation with simpler terms more common -/
partial def genWeightedTerm : GenM Syntax := do
  let depth ← getDepth
  let maxD := (← getConfig).maxDepth

  -- At max depth, only allow leaves
  if depth >= maxD then
    let roll ← randBound 100
    if roll < 60 then genIdent
    else if roll < 90 then genNumLit
    else genStrLit

  else
    -- Weight distribution favoring simpler terms at higher depth
    let roll ← randBound 400
    let depthPenalty := depth * 20

    -- Leaves (high probability)
    if roll < 100 + depth * 15 then genIdent
    else if roll < 160 + depth * 10 then genNumLit
    else if roll < 180 + depth * 5 then genStrLit
    -- Application (moderate)
    else if roll < 220 - depthPenalty then do
        let f ← withDepth genWeightedTerm
        let x ← withDepth genWeightedTerm
        return Syntax.node .none `Lean.Parser.Term.app #[f, x]
    -- Lambda
    else if roll < 235 - depthPenalty then do
        let x ← genIdent
        let body ← withDepth genWeightedTerm
        return Syntax.node .none `null #[mkAtom "fun", x, mkAtom "=>", body]
    -- Let
    else if roll < 245 - depthPenalty then do
        let x ← genIdent
        let val ← withDepth genWeightedTerm
        let body ← withDepth genWeightedTerm
        return Syntax.node .none `null #[mkAtom "let", x, mkAtom ":=", val, mkAtom ";", body]
    -- Parentheses
    else if roll < 275 - depthPenalty then do
        let inner ← withDepth genWeightedTerm
        return Syntax.node .none `null #[mkAtom "(", inner, mkAtom ")"]
    -- If-then-else
    else if roll < 283 - depthPenalty then do
        let cond ← withDepth genWeightedTerm
        let thn ← withDepth genWeightedTerm
        let els ← withDepth genWeightedTerm
        return Syntax.node .none `null #[mkAtom "if", cond, mkAtom "then", thn, mkAtom "else", els]
    -- Binary operators
    else if roll < 320 - depthPenalty then do
        let op ← randChoice #["+", "-", "*", "/", "++", "&&", "||", "<", ">", "=="]
        let lhs ← withDepth genWeightedTerm
        let rhs ← withDepth genWeightedTerm
        return Syntax.node .none `null #[lhs, mkAtom " ", mkAtom op, mkAtom " ", rhs]
    -- Field projection
    else if roll < 345 - depthPenalty then do
        let base ← withDepth genWeightedTerm
        let field ← randChoice #["1", "2", "fst", "snd", "length"]
        return Syntax.node .none `null #[base, mkAtom ".", mkAtom field]
    else genIdent

/-! ## Weighted Tactic Generator -/

partial def genWeightedTactic : GenM Syntax := do
  let roll ← randBound 300
  if roll < 50 then return mkAtom "rfl"
  else if roll < 90 then return mkAtom "simp"
  else if roll < 120 then do
      let t ← withDepth genWeightedTerm
      return Syntax.node .none `null #[mkAtom "exact", t]
  else if roll < 145 then do
      let t ← genIdent
      return Syntax.node .none `null #[mkAtom "apply", t]
  else if roll < 180 then do
      let name ← randChoice #["x", "h", "n", "a"]
      return Syntax.node .none `null #[mkAtom "intro", mkIdent' name]
  else if roll < 200 then do
      let h ← randChoice #["h", "hx", "this"]
      return Syntax.node .none `null #[mkAtom "cases", mkIdent' h]
  else if roll < 220 then do
      let n ← randChoice #["n", "m", "x"]
      return Syntax.node .none `null #[mkAtom "induction", mkIdent' n]
  else if roll < 240 then do
      let h ← randChoice #["h", "key", "this"]
      let t ← withDepth genWeightedTerm
      return Syntax.node .none `null #[mkAtom "have", mkIdent' h, mkAtom ":=", t]
  else if roll < 265 then return mkAtom "constructor"
  else if roll < 290 then return mkAtom "trivial"
  else return mkAtom "decide"

/-! ## Weighted Category Dispatch -/

/-- Generate with weights for any category -/
def genWeighted (cat : Name) : GenM Syntax := do
  match cat with
  | `term => genWeightedTerm
  | `tactic => genWeightedTactic
  | _ => genFromCat cat

/-! ## Command -/

/-- Generate weighted examples -/
elab "#syntaxgen_weighted" cat:Lean.Parser.ident count:(num)? : command => do
  let catName := cat.getId
  let cnt := match count with
    | some n => getNat? n.raw |>.getD 10
    | none => 10

  let mut results : Array Syntax := #[]
  let mut seed : Nat := 42

  for _ in [:cnt * 2] do
    if results.size >= cnt then break
    let config : GenConfig := { seed := seed, maxDepth := 6 }
    seed := seed + 7919
    match runGen config (genWeighted catName) with
    | some s => results := results.push s
    | none => pure ()

  if results.isEmpty then
    logWarning s!"Could not generate weighted examples for `{catName}`."
  else
    logInfo m!"Generated {results.size} weighted examples for `{catName}`:"
    for ex in results do
      logInfo m!"  • {syntaxToString ex}"

end SyntaxGen.Weighted
