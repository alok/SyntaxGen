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

end SyntaxGen.Domain
