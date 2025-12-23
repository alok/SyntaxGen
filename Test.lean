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

/-! ## Template-based Generation -/

-- Use predefined templates for custom syntax
#gen_template listComp 5
#gen_template point2d 3
#gen_template myCheck 3

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

/-! ## Summary of Commands -/

-- #syntaxgen <category> [count]  -- Generate from built-in categories
-- #gen_template <name> [count]   -- Generate from templates
-- #syntaxgen_list <category>     -- List all syntax kinds for a category
