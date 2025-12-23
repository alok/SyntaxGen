/-
  SyntaxGen.Shrink: Shrinking for Syntax values.

  Provides shrinking for minimizing counterexamples in property testing.
-/
import Lean
import SyntaxGen.Basic

namespace SyntaxGen.Shrink

open Lean

/-! ## Shrinking Strategies -/

/-- Count nodes in a syntax tree -/
partial def size (stx : Syntax) : Nat :=
  match stx with
  | .missing => 0
  | .atom _ _ => 1
  | .ident _ _ _ _ => 1
  | .node _ _ args => 1 + args.foldl (fun acc a => acc + size a) 0

/-- Check if name contains substring -/
def nameContains (n : Name) (sub : String) : Bool :=
  (n.toString.splitOn sub).length > 1

/-- Shrink a syntax tree by producing simpler variants -/
partial def shrink (stx : Syntax) : Array Syntax := Id.run do
  let mut results : Array Syntax := #[]

  match stx with
  | .missing => return #[]
  | .atom _ _ => return #[]
  | .ident _ _ _ _ => return #[]

  | .node info kind args =>
      -- Strategy 1: Remove optional parts
      for i in [:args.size] do
        let mut newArgs := args
        newArgs := newArgs.set! i .missing
        results := results.push (Syntax.node info kind newArgs)

      -- Strategy 2: Use a child node directly
      for arg in args do
        if !arg.isMissing && !arg.isAtom then
          results := results.push arg

      -- Strategy 3: Shrink individual children
      for i in [:args.size] do
        for shrunk in shrink args[i]! do
          let mut newArgs := args
          newArgs := newArgs.set! i shrunk
          results := results.push (Syntax.node info kind newArgs)

      -- Strategy 4: Remove trailing elements
      if kind == `null && args.size > 1 then
        for dropCount in [1:args.size] do
          let shortened := args.toSubarray.toArray.take (args.size - dropCount)
          if shortened.size > 0 then
            results := results.push (Syntax.node info kind shortened)

      -- Strategy 5: Replace compound expressions with simpler ones
      if nameContains kind "app" then
        if let some f := args[0]? then results := results.push f
      else if nameContains kind "let" then
        if let some body := args.back? then results := results.push body
      else if nameContains kind "if" then
        for i in [1:args.size] do
          if !args[i]!.isAtom then results := results.push args[i]!
      else if nameContains kind "fun" then
        if let some body := args.back? then results := results.push body
      else if nameContains kind "paren" then
        for arg in args do
          if !arg.isAtom then results := results.push arg

  return results.filter fun s => !s.isMissing

/-- Shrink iteratively until fixed point -/
partial def shrinkAll (stx : Syntax) (maxIter : Nat := 100) : Array Syntax := Id.run do
  let mut all : Array Syntax := #[stx]
  let mut frontier := shrink stx
  let mut seen : Std.HashSet UInt64 := {}
  seen := seen.insert (hash (toString stx))

  for _ in [:maxIter] do
    if frontier.isEmpty then break
    let mut newFrontier : Array Syntax := #[]
    for s in frontier do
      let h := hash (toString s)
      if !seen.contains h then
        seen := seen.insert h
        all := all.push s
        newFrontier := newFrontier ++ shrink s
    frontier := newFrontier

  return all

/-- Get shrunk versions sorted by size (smallest first) -/
def shrinkBySize (stx : Syntax) : Array Syntax :=
  let all := shrinkAll stx
  all.qsort (fun a b => size a < size b)

/-! ## Commands -/

/-- Show shrink tree for a generated syntax -/
elab "#syntaxgen_shrink" cat:Lean.Parser.ident seed:(num)? : command => do
  let catName := cat.getId
  let s := match seed with
    | some n => SyntaxGen.getNat? n.raw |>.getD 42
    | none => 42

  let config : SyntaxGen.GenConfig := { seed := s }
  match SyntaxGen.runGen config (SyntaxGen.genFromCat catName) with
  | none =>
      logWarning s!"Could not generate example for `{catName}`"
  | some stx =>
      let original := SyntaxGen.syntaxToString stx
      let shrinks := shrinkBySize stx

      logInfo m!"Original ({size stx} nodes): {original}"
      logInfo m!"\nShrink tree ({shrinks.size} variants):"
      let mut idx : Nat := 0
      for s in shrinks.toList.take 10 do
        idx := idx + 1
        let pretty := SyntaxGen.syntaxToString s
        logInfo m!"  {idx}. ({size s} nodes) {pretty.take 60}"

      if shrinks.size > 10 then
        logInfo m!"  ... and {shrinks.size - 10} more"

end SyntaxGen.Shrink
