/-
  SyntaxGen.RoundTrip: Parse round-trip testing for syntax.

  The key property: generate → format → parse → should produce valid syntax.
  This helps verify both the generator and the parser/pretty-printer.
-/
import Lean
import SyntaxGen.Basic
import SyntaxGen.Pretty
import SyntaxGen.Weighted

namespace SyntaxGen.RoundTrip

open Lean Parser Elab Command Term Meta

/-! ## Round-trip Testing -/

/-- Result of a round-trip test -/
inductive RoundTripResult where
  | success : Syntax → RoundTripResult      -- Parsed successfully
  | parseError : String → RoundTripResult   -- Failed to parse
  | emptyOutput : RoundTripResult           -- Generator produced empty string
  deriving Inhabited

/-- Attempt to parse a string as a syntax category -/
def tryParse (cat : Name) (input : String) : CommandElabM RoundTripResult := do
  if input.isEmpty || input.all Char.isWhitespace then
    return .emptyOutput

  -- Try to parse as the given category using Parser.runParserCategory
  let env ← getEnv
  let inputCtx := Parser.mkInputContext input "<generated>"

  -- Use runParserCategory which handles category lookup
  match Parser.runParserCategory env cat input "<generated>" with
  | Except.ok stx => return .success stx
  | Except.error err => return .parseError err

/-- Run a single round-trip test -/
def testRoundTrip (cat : Name) (seed : Nat) : CommandElabM (Option (String × RoundTripResult)) := do
  let config : GenConfig := { seed := seed, maxDepth := 5 }
  match runGen config (Weighted.genWeighted cat) with
  | none => return none
  | some stx =>
      let formatted := Pretty.prettyPrint stx
      let result ← tryParse cat formatted
      return some (formatted, result)

/-- Statistics for round-trip testing -/
structure RoundTripStats where
  total : Nat := 0
  successes : Nat := 0
  parseErrors : Nat := 0
  emptyOutputs : Nat := 0
  deriving Repr, Inhabited

namespace RoundTripStats

def successRate (s : RoundTripStats) : Float :=
  if s.total == 0 then 0.0
  else Float.ofNat s.successes / Float.ofNat s.total * 100.0

def add (s : RoundTripStats) (r : RoundTripResult) : RoundTripStats :=
  match r with
  | .success _ => { s with total := s.total + 1, successes := s.successes + 1 }
  | .parseError _ => { s with total := s.total + 1, parseErrors := s.parseErrors + 1 }
  | .emptyOutput => { s with total := s.total + 1, emptyOutputs := s.emptyOutputs + 1 }

end RoundTripStats

/-! ## Commands -/

/-- Run round-trip tests and report results -/
elab "#syntaxgen_roundtrip" cat:Lean.Parser.ident count:(num)? : command => do
  let catName := cat.getId
  let cnt := match count with
    | some n => getNat? n.raw |>.getD 50
    | none => 50

  let mut stats : RoundTripStats := {}
  let mut failures : Array (String × String) := #[]
  let mut seed : Nat := 42

  for _ in [:cnt * 2] do
    if stats.total >= cnt then break
    match ← testRoundTrip catName seed with
    | some (formatted, result) =>
        stats := stats.add result
        match result with
        | .parseError err =>
            if failures.size < 5 then
              failures := failures.push (formatted, err)
        | _ => pure ()
    | none => pure ()
    seed := seed + 7919

  let rate := stats.successRate

  logInfo m!"Round-trip test results for `{catName}`:"
  logInfo m!"  Total: {stats.total}"
  logInfo m!"  Successes: {stats.successes} ({rate.toString.take 5}%)"
  logInfo m!"  Parse errors: {stats.parseErrors}"
  logInfo m!"  Empty outputs: {stats.emptyOutputs}"

  if !failures.isEmpty then
    logInfo m!"\nSample failures:"
    for (input, err) in failures do
      logInfo m!"  Input: {input.take 60}..."
      logInfo m!"  Error: {err.take 80}"

/-- Verbose round-trip: show each test -/
elab "#syntaxgen_roundtrip_verbose" cat:Lean.Parser.ident count:(num)? : command => do
  let catName := cat.getId
  let cnt := match count with
    | some n => getNat? n.raw |>.getD 10
    | none => 10

  let mut seed : Nat := 42
  let mut tested := 0

  for _ in [:cnt * 2] do
    if tested >= cnt then break
    match ← testRoundTrip catName seed with
    | some (formatted, result) =>
        tested := tested + 1
        match result with
        | .success _ =>
            logInfo m!"✓ {formatted}"
        | .parseError err =>
            logWarning m!"✗ {formatted}"
            logWarning m!"  Error: {err.take 60}"
        | .emptyOutput =>
            logInfo m!"○ (empty output)"
    | none => pure ()
    seed := seed + 7919

/-! ## Property-Based Test Helpers

These functions can be used with Plausible for property-based testing.
-/

/-- Check if a syntax string parses successfully -/
def parsesSuccessfully (cat : Name) (s : String) : CommandElabM Bool := do
  match ← tryParse cat s with
  | .success _ => return true
  | _ => return false

/-- Generate a test case for property-based testing -/
def genTestCase (cat : Name) (seed : Nat) : Option String :=
  let config : GenConfig := { seed := seed, maxDepth := 5 }
  match runGen config (Weighted.genWeighted cat) with
  | some stx => some (Pretty.prettyPrint stx)
  | none => none

end SyntaxGen.RoundTrip
