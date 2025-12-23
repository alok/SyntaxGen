# SyntaxGen

A lightweight syntax example generator for Lean 4. Produces unelaborated syntax trees from grammar definitions, useful for ML training data and property-based testing.

[![Documentation](https://img.shields.io/badge/docs-GitHub%20Pages-blue)](https://alok.github.io/SyntaxGen/)

## Quick Start

```lean
import SyntaxGen

-- Generate examples for built-in categories
#syntaxgen term 5
#syntaxgen tactic 10
#syntaxgen command 3

-- Weighted generation (simpler terms more common)
#syntaxgen_weighted term 10

-- Pretty-printed output
#syntaxgen_pretty term 5

-- Export to JSON Lines for ML training
#syntaxgen_export term 1000 to "training_data.jsonl"

-- Round-trip testing (generate → format → parse)
#syntaxgen_roundtrip term 50

-- Show shrink tree for debugging
#syntaxgen_shrink term 42
```

## Installation

Add to your `lakefile.lean`:

```lean
require SyntaxGen from git "https://github.com/alok/SyntaxGen"@"main"
```

Or `lakefile.toml`:

```toml
[[require]]
name = "SyntaxGen"
git = "https://github.com/alok/SyntaxGen"
rev = "main"
```

## Features

### Core Generation
- **`#syntaxgen <category> [count]`** - Generate from built-in categories (term, tactic, command, etc.)
- **`#syntaxgen_weighted <category> [count]`** - Probability-weighted generation for realistic examples
- **`#syntaxgen_pretty <category> [count]`** - Pretty-printed output with proper spacing

### ML Training Export
- **`#syntaxgen_export <category> [count] to "<path>"`** - Export to JSON Lines or CSV
- **`#syntaxgen_batch [cat1, cat2, ...] [count] to "<path>"`** - Batch export multiple categories
- **`#syntaxgen_stats <category> [count]`** - Statistics about generated samples
- **`#syntaxgen_jsonl <category> [count]`** - Preview JSON Lines output

### Testing & Debugging
- **`#syntaxgen_roundtrip <category> [count]`** - Test generate → format → parse cycle
- **`#syntaxgen_roundtrip_verbose <category> [count]`** - Verbose round-trip with per-example output
- **`#syntaxgen_shrink <category> [seed]`** - Show shrink tree for counterexample minimization

### Templates
- **`#gen_template <name> [count]`** - Generate from predefined templates
- **`syntax_gen <pattern> : <category> as <name>`** - Register custom syntax generator

## Modules

| Module | Description |
|--------|-------------|
| `Basic` | Core generator monad and built-in category generators |
| `Weighted` | Probability-weighted generation for realistic examples |
| `Pretty` | Enhanced pretty-printing with smart spacing |
| `Export` | JSON/CSV export for ML training datasets |
| `RoundTrip` | Parse round-trip testing |
| `Shrink` | Syntax shrinking for property testing |
| `Auto` | Automatic generator registration from patterns |
| `Macro` | Template-based generation |

## Example: Custom Syntax

```lean
import SyntaxGen

-- Define list comprehension syntax
syntax "[" term "|" term " in " term (" if " term)? "]" : term

macro_rules
  | `([ $expr | $item in $items if $pred ]) =>
      `(List.map (fun $item => $expr) (List.filter (fun $item => $pred) $items))
  | `([ $expr | $item in $items ]) =>
      `(List.map (fun $item => $expr) $items)

-- Use template generator for this syntax
#gen_template listComp 5
```

## JSON Export Format

Each line in `.jsonl` output:
```json
{"text": "fun x => x + 1", "category": "term", "complexity": 5, "seed": 42}
```

## Plausible Integration

SyntaxGen is designed to integrate with [Plausible](https://github.com/leanprover-community/plausible):

- `Shrinkable Syntax` via the Shrink module
- Round-trip tests for parser verification
- Deterministic generation for reproducibility

## License

MIT
