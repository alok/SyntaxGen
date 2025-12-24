import SyntaxGen.Basic
import SyntaxGen.Weighted
import SyntaxGen.Auto
import SyntaxGen.Macro
import SyntaxGen.Pretty
import SyntaxGen.Export
import SyntaxGen.RoundTrip
import SyntaxGen.Shrink
import SyntaxGen.Domain

/-!
# SyntaxGen: Syntax Example Generator for Lean 4

A lightweight data generator that produces unelaborated syntax examples
from grammar definitions. Useful for creating training data for ML models
and property-based testing.

## Quick Start

```
import SyntaxGen

-- Generate examples for built-in categories
#syntaxgen term 5
#syntaxgen tactic 10

-- Weighted generation (more realistic distribution)
#syntaxgen_weighted term 5

-- Pretty-printed output
#syntaxgen_pretty term 3

-- Export to JSON for ML training
#syntaxgen_export term 1000 to "terms.jsonl"

-- Round-trip testing (generate → format → parse)
#syntaxgen_roundtrip term 50

-- Show shrink tree for debugging
#syntaxgen_shrink term 42

-- Domain-specific generation (mathlib, programming, meta)
#syntaxgen_domain mathlib term 5
#syntaxgen_domain programming term 5
#syntaxgen_tactic_seq mathlib 3
#syntaxgen_structure 3
```

## Modules

- {lit}`Basic`: Core generator monad and built-in category generators
- {lit}`Weighted`: Probability-weighted generation for realistic examples
- {lit}`Auto`: Automatic generator extraction from syntax patterns
- {lit}`Macro`: Template-based generation for custom syntax
- {lit}`Pretty`: Enhanced pretty-printing
- {lit}`Export`: JSON/CSV export for ML datasets
- {lit}`RoundTrip`: Parse round-trip testing
- {lit}`Shrink`: Syntax shrinking for counterexample minimization
- {lit}`Domain`: Domain-specific generators (mathlib, programming, meta)

## Plausible Integration

Can integrate with [Plausible](https://github.com/leanprover-community/plausible):
- {lit}`SampleableExt Syntax` for property-based testing
- {lit}`Shrinkable Syntax` via the Shrink module
- Round-trip tests for parser verification
-/
