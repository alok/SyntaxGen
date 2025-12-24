/-
  Test file for SyntaxGen

  This file demonstrates the #syntaxgen command with custom syntax.
-/
import SyntaxGen

open SyntaxGen SyntaxGen.Macro

/-! ## Basic Category Generation -/

-- Generate examples for built-in categories
#syntaxgen term 5
#syntaxgen tactic 5
#syntaxgen command 3

/-! ## Weighted Generation (More Realistic) -/

-- Weighted generation produces more natural-looking examples
#syntaxgen_weighted term 5
#syntaxgen_weighted tactic 5

/-! ## Pretty Printing -/

-- Pretty-printed output with proper spacing
#syntaxgen_pretty term 5

/-! ## Template-based Generation -/

-- Use predefined templates for custom syntax
#gen_template listComp 5
#gen_template point2d 3
#gen_template myCheck 3

/-! ## Shrinking for Property Testing -/

-- Show shrink tree for generated syntax (useful for counterexample minimization)
#syntaxgen_shrink term 42

/-! ## Export & Statistics -/

-- Show JSON Lines output
#syntaxgen_jsonl term 5

-- Statistics about generated samples
#syntaxgen_stats term 20

/-! ## Round-trip Testing -/

-- Test: generate → format → parse
#syntaxgen_roundtrip term 10

/-! ## List Comprehension Example -/

/-- List comprehension syntax. -/
syntax (name := listComprehension) "[" term "|" term " in " term (" if " term)? "]" : term

macro_rules
  | `([ $expr | $item in $items if $predicate ]) =>
      `(List.map (fun $item => $expr) (List.filter (fun $item => $predicate) $items))
  | `([ $expr | $item in $items ]) =>
      `(List.map (fun $item => $expr) $items)

-- These work because we defined the macro rules
def squares  : List Nat := [ x | x in [0,1,2,3,4,5]]
def evensSq  : List Nat := [ x ^ 2 | x in List.range 6 if x % 2 == 0 ]

#eval squares   -- [0,1,2,3,4,5]
#eval evensSq   -- [0,4,16]

-- The template-based generator produces examples that match this syntax!
-- (The examples are unelaborated - they show the surface syntax)

/-! ## Point Syntax Example -/

syntax (name := point2d') "point" "(" term "," term ")" : term

macro_rules
  | `(point($x, $y)) => `(($x, $y))

-- Template generator produces matching examples
#eval (point(1, 2) : Nat × Nat)  -- (1, 2)

/-!
## How to Add Custom Templates

To add a template for your own syntax:

1. Define the template in `SyntaxGen.Macro`:
```lean
def myTemplate : GenTemplate := GenTemplate.mk' [
  .inl "mykeyword", .inr `term, .inl "->", .inr `term
]
```

2. Register it in `getTemplateGen`:
```lean
| `myTemplate => some myTemplate.toGen
```

3. Use it:
```lean
#gen_template myTemplate 5
```

## Plausible Integration

This could integrate with Plausible for property-based testing:
- `SampleableExt Syntax` for generating syntax
- `Shrinkable Syntax` for minimizing counterexamples
- Test parser round-trips: generate → format → parse → compare
-/

/-! ## Domain-Specific Generation -/

-- Domain-specific term generation (mathlib, programming, meta)
#syntaxgen_domain mathlib term 5
#syntaxgen_domain programming term 5
#syntaxgen_domain meta term 3

-- Forall and exists patterns
#syntaxgen_domain mathlib forall 3
#syntaxgen_domain mathlib exists 3

-- Do-notation and match patterns
#syntaxgen_domain programming do 3
#syntaxgen_domain programming match 3

-- Multi-step tactic sequences
#syntaxgen_tactic_seq mathlib 3

-- Structure and inductive declarations
#syntaxgen_structure 5

/-! ## Summary of Commands -/

-- Basic generation
-- #syntaxgen <category> [count]  -- Generate from built-in categories

-- Enhanced generation
-- #syntaxgen_weighted <category> [count]  -- Weighted (realistic) distribution
-- #syntaxgen_pretty <category> [count]    -- Pretty-printed output
-- #syntaxgen! <name> [count]              -- With auto-registry lookup

-- Templates
-- #gen_template <name> [count]   -- Generate from templates
-- syntax_gen <pattern> : <category> as <name>  -- Define & register generator

-- Export (for ML training data)
-- #syntaxgen_jsonl <category> [count]     -- JSON Lines format
-- #syntaxgen_export <category> [count] to "file.jsonl"  -- Export to file
-- #syntaxgen_batch [cat1, cat2] [count] to "file.jsonl" -- Batch export
-- #syntaxgen_stats <category> [count]     -- Statistics

-- Testing
-- #syntaxgen_roundtrip <category> [count]         -- Round-trip test
-- #syntaxgen_roundtrip_verbose <category> [count] -- Verbose round-trip
-- #syntaxgen_shrink <category> [seed]             -- Shrink tree

-- Domain-specific generation
-- #syntaxgen_domain <domain> <category> [count]   -- Domain-aware generation
--   Domains: mathlib, programming, meta
--   Categories: term, tactic, forall, exists, do, match, structure
-- #syntaxgen_tactic_seq <domain> [count] [steps]  -- Multi-step tactic sequences
-- #syntaxgen_structure [count]                    -- Structure/inductive declarations
