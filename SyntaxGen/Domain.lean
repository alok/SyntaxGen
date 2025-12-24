/-
  SyntaxGen.Domain: Domain-specific realistic syntax generators.

  Re-exports all Domain modules for convenient importing.
-/

import SyntaxGen.Domain.Pools
import SyntaxGen.Domain.Terms
import SyntaxGen.Domain.Tactics
import SyntaxGen.Domain.Structures
import SyntaxGen.Domain.Maze
import SyntaxGen.Pretty

namespace SyntaxGen.Domain

open Lean SyntaxGen.Pretty

/-! ## Convenience Commands -/

open Elab Command Meta in
/-- Generate domain-specific terms: `#syntaxgen_domain mathlib term 10` -/
elab "#syntaxgen_domain" domain:ident cat:ident count:(num)? : command => do
  let domainName := domain.getId.toString
  let catName := cat.getId
  let cnt := match count with
    | some n => n.getNat
    | none => 5

  let pools := getPoolByName domainName

  let config : SyntaxGen.GenConfig := { seed := 42, maxDepth := 5 }
  let mut results : Array String := #[]
  let mut seed := config.seed

  for _ in [:cnt * 2] do
    if results.size >= cnt then break
    let cfg := { config with seed := seed }
    seed := seed + 7919

    let gen := match catName with
      | `term => genDomainTerm pools
      | `tactic => genFormattedTacticSeq pools 5
      | `structure | `decl => genDeclaration pools
      | `forall => genForallChain pools
      | `exists => genExists pools
      | `do => genDoBlock pools
      | `match => genMatchExpr pools
      | _ => genDomainTerm pools

    match SyntaxGen.runGen cfg gen with
    | some stx =>
        let formatted := cleanSpaces (prettyPrint (normalize stx))
        if !formatted.isEmpty then results := results.push formatted
    | none => pure ()

  logInfo m!"Generated {results.size} {catName} examples for domain '{domainName}':"
  for i in [:results.size] do
    logInfo m!"  {i + 1}. {results[i]!}"

open Elab Command Meta in
/-- Generate with AI hints: `#syntaxgen_guided proof term 5 "complex proof patterns:forall,exists"`
    Hint keywords:
    - Complexity: "simple", "complex", "minimal"
    - Style: "functional", "imperative", "proof"
    - Verbosity: "verbose", "terse"
    - Patterns: "patterns:forall,match,do" (prefer these)
    - Avoid: "avoid:lambda,if" (avoid these)
-/
elab "#syntaxgen_guided" domain:ident cat:ident count:(num)? hint:(str)? : command => do
  let domainName := domain.getId.toString
  let catName := cat.getId
  let cnt := match count with
    | some n => n.getNat
    | none => 5
  let hintStr := match hint with
    | some s => s.getString
    | none => ""

  let pools := getPoolByName domainName
  let genHint := SyntaxGen.parseGenHint hintStr

  let config : SyntaxGen.GenConfig := { seed := 42, maxDepth := 5 }
  let mut results : Array String := #[]
  let mut seed := config.seed

  for _ in [:cnt * 2] do
    if results.size >= cnt then break
    let cfg := { config with seed := seed }
    seed := seed + 7919

    let gen := match catName with
      | `term => genDomainTerm pools
      | `tactic => genFormattedTacticSeq pools 5
      | `structure | `decl => genDeclaration pools
      | `forall => genForallChain pools
      | `exists => genExists pools
      | `do => genDoBlock pools
      | `match => genMatchExpr pools
      | _ => genDomainTerm pools

    match SyntaxGen.runGen cfg gen genHint with
    | some stx =>
        let formatted := cleanSpaces (prettyPrint (normalize stx))
        if !formatted.isEmpty then results := results.push formatted
    | none => pure ()

  let hintDesc := if hintStr.isEmpty then "" else s!" with hint \"{hintStr}\""
  logInfo m!"Generated {results.size} {catName} examples for '{domainName}'{hintDesc}:"
  for i in [:results.size] do
    logInfo m!"  {i + 1}. {results[i]!}"

open Elab Command Meta in
/-- Show all hint variations for a domain: `#syntaxgen_variations proof term` -/
elab "#syntaxgen_variations" domain:ident cat:ident : command => do
  let domainName := domain.getId.toString
  let catName := cat.getId
  let pools := getPoolByName domainName

  let variations := #[
    ("simple", "Low complexity, simple patterns"),
    ("complex", "High complexity, nested patterns"),
    ("functional", "Functional style (lambda, match)"),
    ("imperative", "Imperative style (do, let, if)"),
    ("proof", "Proof style (forall, exists, show)")
  ]

  for (hintStr, desc) in variations do
    let genHint := SyntaxGen.parseGenHint hintStr
    let config : SyntaxGen.GenConfig := { seed := 42, maxDepth := 5 }
    let mut results : Array String := #[]
    let mut seed := config.seed

    for _ in [:6] do
      if results.size >= 3 then break
      let cfg := { config with seed := seed }
      seed := seed + 7919

      let gen := match catName with
        | `term => genDomainTerm pools
        | `tactic => genFormattedTacticSeq pools 3
        | _ => genDomainTerm pools

      match SyntaxGen.runGen cfg gen genHint with
      | some stx =>
          let formatted := cleanSpaces (prettyPrint (normalize stx))
          if !formatted.isEmpty then results := results.push formatted
      | none => pure ()

    logInfo m!"\n{hintStr.toUpper} ({desc}):"
    for i in [:results.size] do
      logInfo m!"  {i + 1}. {results[i]!}"

open Elab Command Meta in
/-- Generate tactic sequences: `#syntaxgen_tactic_seq mathlib 5` -/
elab "#syntaxgen_tactic_seq" domain:ident count:(num)? steps:(num)? : command => do
  let domainName := domain.getId.toString
  let cnt := match count with
    | some n => n.getNat
    | none => 5
  let maxSteps := match steps with
    | some n => n.getNat
    | none => 5

  let pools := getPoolByName domainName

  let config : SyntaxGen.GenConfig := { seed := 42, maxDepth := 6 }
  let mut results : Array String := #[]
  let mut seed := config.seed

  for _ in [:cnt * 2] do
    if results.size >= cnt then break
    let cfg := { config with seed := seed }
    seed := seed + 7919

    match SyntaxGen.runGen cfg (genFormattedTacticSeq pools maxSteps) with
    | some stx =>
        let formatted := cleanSpaces (prettyPrint (normalize stx))
        if !formatted.isEmpty then results := results.push formatted
    | none => pure ()

  logInfo m!"Generated {results.size} tactic sequences for domain '{domainName}':"
  for i in [:results.size] do
    logInfo m!"  {i + 1}. {results[i]!}"

open Elab Command Meta in
/-- Generate structure/inductive declarations: `#syntaxgen_structure 5` -/
elab "#syntaxgen_structure" count:(num)? : command => do
  let cnt := match count with
    | some n => n.getNat
    | none => 5

  let pools := mathlibPools  -- Use mathlib pools for type names

  let config : SyntaxGen.GenConfig := { seed := 42, maxDepth := 4 }
  let mut results : Array String := #[]
  let mut seed := config.seed

  for _ in [:cnt * 2] do
    if results.size >= cnt then break
    let cfg := { config with seed := seed }
    seed := seed + 7919

    match SyntaxGen.runGen cfg (genDeclaration pools) with
    | some stx =>
        let formatted := cleanSpaces (prettyPrint (normalize stx))
        if !formatted.isEmpty then results := results.push formatted
    | none => pure ()

  logInfo m!"Generated {results.size} declarations:"
  for i in [:results.size] do
    logInfo m!"  {i + 1}. {results[i]!}"

/-! ## Commands for Understanding Syntax -/

open Elab Command Meta in
/-- Explore all pattern types systematically: `#syntaxgen_explore proof`
    Shows 2 examples of each pattern type to build intuition for the syntax.
-/
elab "#syntaxgen_explore" domain:ident : command => do
  let domainName := domain.getId.toString
  let pools := getPoolByName domainName

  -- All pattern types with descriptions and direct generator functions
  let patterns : Array (String × String × (DomainPools → GenM Syntax)) := #[
    ("forall", "Universal quantification: ∀ (x : T), body", genForallChain),
    ("exists", "Existential quantification: ∃ x, P x", genExists),
    ("anonymous", "Anonymous constructors: ⟨a, b, c⟩", genAnonymousConstructor),
    ("lambda", "Lambda expressions: fun x => e", genLambda),
    ("proofLambda", "Proof lambdas: fun h => h.property", genProofLambda),
    ("showBy", "Show-by expressions: show P by tactic", genShowBy),
    ("do", "Do-notation blocks: do let x ← f; g x", genDoBlock),
    ("match", "Pattern matching: match x with | .a => ...", genMatchExpr),
    ("methodChain", "Method chains: x.f.g |>.h", genMethodChain),
    ("if", "Conditionals: if c then t else e", genIfThenElse),
    ("let", "Let bindings: let x := e; body", genLetIn),
    ("app", "Function application: f x y", genApp),
    ("typeAscription", "Type ascriptions: (e : T)", genTypeAscription)
  ]

  logInfo m!"=== Exploring {domainName} syntax patterns ===\n"

  let mut patternSeed : Nat := 42

  for (pattern, desc, gen) in patterns do
    let mut examples : Array String := #[]
    let mut seed := patternSeed

    for _ in [:8] do  -- Attempts to find unique examples
      if examples.size >= 2 then break
      let cfg : SyntaxGen.GenConfig := { seed := seed, maxDepth := 4 }
      seed := seed + 7919
      -- Directly call the specific generator
      match SyntaxGen.runGen cfg (gen pools) with
      | some stx =>
          let formatted := cleanSpaces (prettyPrint (normalize stx))
          if !formatted.isEmpty && !examples.contains formatted then
            examples := examples.push formatted
      | none => pure ()

    patternSeed := patternSeed + 100000  -- Different seed range per pattern

    if !examples.isEmpty then
      logInfo m!"• {pattern} ({desc}):"
      for ex in examples do
        logInfo m!"    {ex}"

open Elab Command Meta in
/-- Progressive complexity curriculum: `#syntaxgen_curriculum proof 5`
    Generates examples from simple to complex to build understanding.
-/
elab "#syntaxgen_curriculum" domain:ident perLevel:(num)? : command => do
  let domainName := domain.getId.toString
  let pools := getPoolByName domainName
  let cnt := match perLevel with
    | some n => n.getNat
    | none => 3

  let levels := #[
    ("Level 1: Atoms", "minimal", 2),
    ("Level 2: Simple", "simple", 3),
    ("Level 3: Moderate", "", 4),
    ("Level 4: Complex", "complex", 5),
    ("Level 5: Nested", "complex patterns:forall,match,do", 6)
  ]

  logInfo m!"=== {domainName} syntax curriculum ===\n"

  for (levelName, hintStr, depth) in levels do
    let genHint := SyntaxGen.parseGenHint hintStr
    let config : SyntaxGen.GenConfig := { seed := 42, maxDepth := depth }
    let mut examples : Array String := #[]
    let mut seed := config.seed

    for _ in [:cnt * 3] do
      if examples.size >= cnt then break
      let cfg := { config with seed := seed }
      seed := seed + 7919
      match SyntaxGen.runGen cfg (genDomainTerm pools) genHint with
      | some stx =>
          let formatted := cleanSpaces (prettyPrint (normalize stx))
          if !formatted.isEmpty && !examples.contains formatted then
            examples := examples.push formatted
      | none => pure ()

    logInfo m!"{levelName}:"
    for ex in examples do
      logInfo m!"  {ex}"
    logInfo m!""

open Elab Command Meta in
/-- Generate training batch: `#syntaxgen_batch proof term 100 seed:12345`
    Generates large batches suitable for ML training with reproducible seeds.
-/
elab "#syntaxgen_batch" domain:ident cat:ident count:(num)? seedOpt:(str)? : command => do
  let domainName := domain.getId.toString
  let catName := cat.getId
  let cnt := match count with
    | some n => n.getNat
    | none => 50
  let baseSeed := match seedOpt with
    | some s =>
        let seedStr := s.getString
        if seedStr.startsWith "seed:" then
          (seedStr.drop 5).toNat?.getD 42
        else seedStr.toNat?.getD 42
    | none => 42

  let pools := getPoolByName domainName
  let config : SyntaxGen.GenConfig := { seed := baseSeed, maxDepth := 5 }
  let mut results : Array String := #[]
  let mut seed := baseSeed

  -- Use diverse hints to get variety
  let hints := #["", "simple", "complex", "functional", "imperative", "proof"]

  for _ in [:cnt * 3] do
    if results.size >= cnt then break
    let hintIdx := seed % hints.size
    let genHint := SyntaxGen.parseGenHint hints[hintIdx]!
    let cfg := { config with seed := seed }
    seed := seed + 7919

    let gen := match catName with
      | `term => genDomainTerm pools
      | `tactic => genFormattedTacticSeq pools 5
      | `structure | `decl => genDeclaration pools
      | _ => genDomainTerm pools

    match SyntaxGen.runGen cfg gen genHint with
    | some stx =>
        let formatted := cleanSpaces (prettyPrint (normalize stx))
        if !formatted.isEmpty && !results.contains formatted then
          results := results.push formatted
    | none => pure ()

  logInfo m!"=== Training batch: {results.size} unique {catName} examples ==="
  logInfo m!"Domain: {domainName}, Seed: {baseSeed}\n"
  for i in [:results.size] do
    logInfo m!"{results[i]!}"

open Elab Command Meta in
/-- Side-by-side style comparison: `#syntaxgen_contrast proof`
    Shows how the same concepts look in different styles.
-/
elab "#syntaxgen_contrast" domain:ident : command => do
  let domainName := domain.getId.toString
  let pools := getPoolByName domainName

  let styles := #[
    ("Minimal/Terse", "minimal terse"),
    ("Functional", "functional"),
    ("Imperative", "imperative"),
    ("Proof-oriented", "proof"),
    ("Complex/Verbose", "complex verbose")
  ]

  logInfo m!"=== Style contrast for {domainName} ===\n"

  for (styleName, hintStr) in styles do
    let genHint := SyntaxGen.parseGenHint hintStr
    let config : SyntaxGen.GenConfig := { seed := 42, maxDepth := 5 }
    let mut examples : Array String := #[]
    let mut seed := config.seed

    for _ in [:8] do
      if examples.size >= 3 then break
      let cfg := { config with seed := seed }
      seed := seed + 7919
      match SyntaxGen.runGen cfg (genDomainTerm pools) genHint with
      | some stx =>
          let formatted := cleanSpaces (prettyPrint (normalize stx))
          if !formatted.isEmpty && !examples.contains formatted then
            examples := examples.push formatted
      | none => pure ()

    logInfo m!"▸ {styleName}:"
    for ex in examples do
      logInfo m!"    {ex}"
    logInfo m!""

end SyntaxGen.Domain
