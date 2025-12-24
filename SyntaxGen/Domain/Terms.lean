/-
  SyntaxGen.Domain.Terms: Realistic term pattern generators.

  Generates well-formed term syntax for different domains:
  - Math-style: forall chains, exists, proof lambdas, anonymous constructors
  - Programming-style: do-notation, match expressions, method chains
-/
import Lean
import SyntaxGen.Basic
import SyntaxGen.Domain.Pools

namespace SyntaxGen.Domain

open Lean SyntaxGen

/-! ## Math-Style Term Patterns -/

/-- Generate a simple predicate application: `P x` or `P x y` -/
def genPredApp (pools : DomainPools) : GenM Syntax := do
  let pred ← randChoice (if pools.hypotheses.isEmpty then #["P", "Q"] else #["P", "Q", "R"])
  let x ← genVariable pools
  if ← randBool 30 then
    let y ← genVariable pools
    return Syntax.node .none `null #[mkIdent' pred, x, y]
  else
    return Syntax.node .none `null #[mkIdent' pred, x]

/-- Generate body for forall: either predicate, implication, or nested forall -/
partial def genForallBody (pools : DomainPools) : GenM Syntax := do
  if ← isMaxDepth then
    genPredApp pools
  else
    let roll ← randBound 100
    if roll < 30 then
      -- Simple predicate: P x
      genPredApp pools
    else if roll < 70 then
      -- Implication: P x → Q x
      let p ← withDepth (genPredApp pools)
      let q ← withDepth (genPredApp pools)
      return Syntax.node .none `null #[p, mkAtom "→", q]
    else
      -- Nested forall (call genForallChain for nesting)
      withDepth (genPredApp pools)  -- Avoid mutual recursion

/-- Generate a forall expression: `∀ (x : T), body` -/
partial def genForall (pools : DomainPools) : GenM Syntax := do
  if ← isMaxDepth then
    genPredApp pools
  else
    let x ← genVariable pools
    let ty ← genType pools
    let body ← withDepth (genForallBody pools)
    return Syntax.node .none `null #[
      mkAtom "∀", mkAtom "(", x, mkAtom " : ", ty, mkAtom ")", mkAtom ",", body
    ]

/-- Generate a forall chain with multiple binders -/
partial def genForallChain (pools : DomainPools) : GenM Syntax := do
  if ← isMaxDepth then
    genPredApp pools
  else
    let numBinders ← randBound 3  -- 0-2 additional binders
    let mut result ← withDepth (genForallBody pools)

    -- Build from inside out (add binders wrapping the result)
    for _ in [:numBinders + 1] do
      let x ← genVariable pools
      let ty ← genType pools
      -- Alternate between ∀ and → style
      if ← randBool 50 then
        result := Syntax.node .none `null #[
          mkAtom "∀", mkAtom "(", x, mkAtom " : ", ty, mkAtom ")", mkAtom ",", result
        ]
      else
        result := Syntax.node .none `null #[
          mkAtom "(", x, mkAtom " : ", ty, mkAtom ")", mkAtom "→", result
        ]
    return result

/-- Generate an exists expression: `∃ x, P x` or `∃ x, P x ∧ Q x` -/
partial def genExists (pools : DomainPools) : GenM Syntax := do
  if ← isMaxDepth then
    genPredApp pools
  else
    let x ← genVariable pools
    let pred1 ← withDepth (genPredApp pools)

    -- 50% chance of conjunction
    if ← randBool 50 then
      let pred2 ← withDepth (genPredApp pools)
      return Syntax.node .none `null #[
        mkAtom "∃", x, mkAtom ",", pred1, mkAtom "∧", pred2
      ]
    else
      return Syntax.node .none `null #[
        mkAtom "∃", x, mkAtom ",", pred1
      ]

/-- Generate anonymous constructor: `⟨a, b, c⟩` -/
def genAnonymousConstructor (pools : DomainPools) : GenM Syntax := do
  let numArgs ← randBound 3  -- 1-3 args
  let mut args : Array Syntax := #[]
  for i in [:numArgs + 1] do
    if i > 0 then args := args.push (mkAtom ",")
    let arg ← genVariable pools
    args := args.push arg
  return Syntax.node .none `null (#[mkAtom "⟨"] ++ args ++ #[mkAtom "⟩"])

/-- Generate show-by expression: `show P by tactic` -/
def genShowBy (pools : DomainPools) : GenM Syntax := do
  let prop ← randChoice #["P", "Q", "True", "a = a", "n = n"]
  -- Only use tactics that can close a goal alone
  let tac ← randChoice #["rfl", "trivial", "assumption", "decide", "simp"]
  return Syntax.node .none `null #[mkAtom "show", mkIdent' prop, mkAtom "by", mkAtom tac]

/-- Generate proof lambda: `fun h => h.method` -/
def genProofLambda (pools : DomainPools) : GenM Syntax := do
  let h ← genHypothesis pools
  let method ← genField pools
  return Syntax.node .none `null #[
    mkAtom "fun", h, mkAtom "=>", h, mkAtom ".", method
  ]

/-- Generate type ascription: `(expr : Type)` -/
partial def genTypeAscription (pools : DomainPools) : GenM Syntax := do
  let inner ← genVariable pools
  let ty ← genType pools
  return Syntax.node .none `null #[mkAtom "(", inner, mkAtom " : ", ty, mkAtom ")"]

/-! ## Programming-Style Term Patterns -/

/-- Generate a simple do element (non-recursive) -/
def genSimpleDoElem (pools : DomainPools) : GenM Syntax := do
  let roll ← randBound 100
  if roll < 40 then
    -- let x := e
    let x ← genVariable pools
    let e ← genFunction pools
    return Syntax.node .none `null #[mkAtom "let", x, mkAtom ":=", e]
  else if roll < 70 then
    -- let x ← e
    let x ← genVariable pools
    let e ← genQualifiedName pools
    return Syntax.node .none `null #[mkAtom "let", x, mkAtom "←", e]
  else
    -- function call
    genQualifiedName pools

/-- Generate do-notation block: `do let x ← f; g x` -/
partial def genDoBlock (pools : DomainPools) : GenM Syntax := do
  if ← isMaxDepth then
    let f ← genQualifiedName pools
    return Syntax.node .none `null #[mkAtom "do", f]
  else
    let numSteps ← randBound 3  -- 1-3 steps
    let mut stmts : Array Syntax := #[mkAtom "do"]

    for i in [:numSteps + 1] do
      let stmt ← withDepth (genSimpleDoElem pools)
      if i > 0 then stmts := stmts.push (mkAtom ";")
      stmts := stmts.push stmt

    return Syntax.node .none `null stmts

/-- Generate a match arm: `| .ctor => body` -/
def genMatchArm (pools : DomainPools) : GenM Syntax := do
  let ctor ← genConstructor pools
  let body ← genVariable pools
  return Syntax.node .none `null #[mkAtom "|", mkAtom ".", ctor, mkAtom "=>", body]

/-- Generate match expression: `match x with | .some y => ... | .none => ...` -/
partial def genMatchExpr (pools : DomainPools) : GenM Syntax := do
  if ← isMaxDepth then
    genVariable pools
  else
    let scrutinee ← genVariable pools
    let numArms ← randBound 2  -- 1-2 arms

    let mut arms : Array Syntax := #[mkAtom "match", scrutinee, mkAtom "with"]

    for _ in [:numArms + 1] do
      let arm ← withDepth (genMatchArm pools)
      arms := arms.push arm

    return Syntax.node .none `null arms

/-- Generate method chain: `xs.map f |>.filter p` -/
partial def genMethodChain (pools : DomainPools) : GenM Syntax := do
  if ← isMaxDepth then
    genVariable pools
  else
    let base ← genVariable pools
    let numMethods ← randBound 3  -- 1-3 methods

    let mut result := base
    for _ in [:numMethods + 1] do
      let method ← randChoice #["map", "filter", "foldl", "take", "drop", "reverse", "toList", "toArray"]
      let arg ← genVariable pools

      -- Alternate between .method arg and |>.method arg
      if ← randBool 50 then
        result := Syntax.node .none `null #[result, mkAtom ".", mkAtom method, arg]
      else
        result := Syntax.node .none `null #[result, mkAtom "|>.", mkAtom method, arg]

    return result

/-- Generate where clause: `f x where f := ...` -/
partial def genWhereClause (pools : DomainPools) : GenM Syntax := do
  if ← isMaxDepth then
    genVariable pools
  else
    let f ← genFunction pools
    let x ← genVariable pools
    let body ← withDepth (genVariable pools)
    return Syntax.node .none `null #[
      f, x, mkAtom "where", f, mkAtom ":=", body
    ]

/-- Generate if-then-else: `if cond then t else e` -/
partial def genIfThenElse (pools : DomainPools) : GenM Syntax := do
  if ← isMaxDepth then
    genVariable pools
  else
    let cond ← genVariable pools
    let thn ← withDepth (genVariable pools)
    let els ← withDepth (genVariable pools)
    return Syntax.node .none `null #[
      mkAtom "if", cond, mkAtom "then", thn, mkAtom "else", els
    ]

/-- Generate let-in expression: `let x := e; body` -/
partial def genLetIn (pools : DomainPools) : GenM Syntax := do
  if ← isMaxDepth then
    genVariable pools
  else
    let x ← genVariable pools
    let e ← withDepth (genVariable pools)
    let body ← withDepth (genVariable pools)
    return Syntax.node .none `null #[
      mkAtom "let", x, mkAtom ":=", e, mkAtom ";", body
    ]

/-! ## Shared Patterns -/

/-- Generate field projection: `x.field` or `x.1` -/
def genFieldProjection (pools : DomainPools) : GenM Syntax := do
  let base ← genVariable pools
  let field ← genField pools
  return Syntax.node .none `null #[base, mkAtom ".", field]

/-- Generate instance argument: `[Monad m]` -/
def genInstanceArg (pools : DomainPools) : GenM Syntax := do
  let cls ← randChoice #["Monad", "Functor", "Applicative", "ToString", "Repr", "BEq", "Hashable", "Inhabited"]
  let tyvar ← randChoice #["m", "α", "β", "f"]
  return Syntax.node .none `null #[mkAtom "[", mkIdent' cls, mkIdent' tyvar, mkAtom "]"]

/-- Generate function application: `f x` or `f x y` -/
partial def genApp (pools : DomainPools) : GenM Syntax := do
  if ← isMaxDepth then
    genVariable pools
  else
    let f ← genQualifiedName pools
    let x ← withDepth (genVariable pools)
    if ← randBool 40 then
      let y ← withDepth (genVariable pools)
      return Syntax.node .none `null #[f, x, y]
    else
      return Syntax.node .none `null #[f, x]

/-- Generate lambda: `fun x => body` -/
partial def genLambda (pools : DomainPools) : GenM Syntax := do
  if ← isMaxDepth then
    genVariable pools
  else
    let x ← genVariable pools
    let body ← withDepth (genVariable pools)
    return Syntax.node .none `null #[mkAtom "fun", x, mkAtom "=>", body]

/-! ## Domain-Specific Term Dispatcher -/

/-- Generate a realistic term for the given domain -/
partial def genDomainTerm (pools : DomainPools) : GenM Syntax := do
  if ← isMaxDepth then
    genVariable pools
  else
    let roll ← randBound 100

    match pools.name with
    | "mathlib" | "math" | "proof" =>
        -- Math-style patterns
        if roll < 15 then genForallChain pools
        else if roll < 25 then genExists pools
        else if roll < 35 then genAnonymousConstructor pools
        else if roll < 45 then genProofLambda pools
        else if roll < 55 then genShowBy pools
        else if roll < 65 then genTypeAscription pools
        else if roll < 80 then genApp pools
        else genVariable pools

    | "programming" | "prog" | "io" =>
        -- Programming-style patterns
        if roll < 20 then genDoBlock pools
        else if roll < 35 then genMatchExpr pools
        else if roll < 50 then genMethodChain pools
        else if roll < 60 then genIfThenElse pools
        else if roll < 70 then genLetIn pools
        else if roll < 80 then genApp pools
        else genQualifiedName pools

    | "meta" | "metaprogramming" | "elab" =>
        -- Meta patterns (similar to programming but more qualified names)
        if roll < 15 then genDoBlock pools
        else if roll < 25 then genMatchExpr pools
        else if roll < 40 then genQualifiedName pools
        else if roll < 55 then genApp pools
        else if roll < 65 then genTypeAscription pools
        else if roll < 75 then genMethodChain pools
        else genVariable pools

    | _ =>
        -- Default: mix of patterns
        if roll < 20 then genApp pools
        else if roll < 35 then genLambda pools
        else if roll < 50 then genIfThenElse pools
        else if roll < 65 then genLetIn pools
        else genVariable pools

end SyntaxGen.Domain
