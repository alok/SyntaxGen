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
    return Syntax.node .none `null #[mkIdent' pred, mkAtom " ", x, mkAtom " ", y]
  else
    return Syntax.node .none `null #[mkIdent' pred, mkAtom " ", x]

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
  let allVars := if pools.variables.isEmpty then #["x", "y", "z"] else pools.variables
  let mut usedVars : Array String := #[]
  let mut args : Array Syntax := #[]
  for i in [:numArgs + 1] do
    if i > 0 then args := args.push (mkAtom ", ")
    -- Pick unique variable
    let available := allVars.filter (· ∉ usedVars)
    let varName ← if available.isEmpty then randChoice allVars else randChoice available
    usedVars := usedVars.push varName
    args := args.push (mkIdent' varName)
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
    return Syntax.node .none `null #[mkAtom "let ", x, mkAtom " := ", e]
  else if roll < 70 then
    -- let x ← f arg
    let x ← genVariable pools
    let f ← genQualifiedName pools
    let arg ← genVariable pools
    return Syntax.node .none `null #[mkAtom "let ", x, mkAtom " ← ", f, mkAtom " ", arg]
  else
    -- function call with arg
    let f ← genQualifiedName pools
    let arg ← genVariable pools
    return Syntax.node .none `null #[f, mkAtom " ", arg]

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

/-- Generate a match arm with given constructor: `| .ctor => body` -/
def genMatchArmWith (pools : DomainPools) (ctorName : String) : GenM Syntax := do
  let body ← genVariable pools
  return Syntax.node .none `null #[mkAtom "| .", mkIdent' ctorName, mkAtom " => ", body]

/-- Generate match expression: `match x with | .some y => ... | .none => ...` -/
partial def genMatchExpr (pools : DomainPools) : GenM Syntax := do
  if ← isMaxDepth then
    genVariable pools
  else
    let scrutinee ← genVariable pools
    let numArms ← randBound 2  -- 1-2 arms

    -- Pick unique constructors for each arm
    let allCtors := if pools.constructors.isEmpty then #["some", "none"] else pools.constructors
    let mut usedCtors : Array String := #[]
    let mut arms : Array Syntax := #[mkAtom "match ", scrutinee, mkAtom " with"]

    for _ in [:numArms + 1] do
      -- Pick a constructor not yet used
      let available := allCtors.filter (· ∉ usedCtors)
      if available.isEmpty then break  -- No more unique constructors
      let ctor ← randChoice available
      usedCtors := usedCtors.push ctor
      let arm ← withDepth (genMatchArmWith pools ctor)
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
  return Syntax.node .none `null #[mkAtom "[", mkIdent' cls, mkAtom " ", mkIdent' tyvar, mkAtom "]"]

/-- Generate function application: `f x` or `f x y` -/
partial def genApp (pools : DomainPools) : GenM Syntax := do
  if ← isMaxDepth then
    genVariable pools
  else
    let f ← genQualifiedName pools
    let x ← withDepth (genVariable pools)
    if ← randBool 40 then
      let y ← withDepth (genVariable pools)
      return Syntax.node .none `null #[f, mkAtom " ", x, mkAtom " ", y]
    else
      return Syntax.node .none `null #[f, mkAtom " ", x]

/-- Generate lambda: `fun x => body` -/
partial def genLambda (pools : DomainPools) : GenM Syntax := do
  if ← isMaxDepth then
    genVariable pools
  else
    let x ← genVariable pools
    let body ← withDepth (genVariable pools)
    return Syntax.node .none `null #[mkAtom "fun", x, mkAtom "=>", body]

/-! ## Domain-Specific Term Dispatcher -/

/-- Pattern weights for hint-based generation -/
structure PatternWeights where
  forall_ : Nat := 0
  exists_ : Nat := 0
  anonymous : Nat := 0
  proofLambda : Nat := 0
  showBy : Nat := 0
  doBlock : Nat := 0
  match_ : Nat := 0
  methodChain : Nat := 0
  ifThenElse : Nat := 0
  letIn : Nat := 0
  app : Nat := 0
  lambda : Nat := 0
  typeAscription : Nat := 0
  var : Nat := 0

/-- Pattern name to weight field mapping -/
def patternNames : Array (String × String) := #[
  ("forall", "forall_"), ("exists", "exists_"), ("anonymous", "anonymous"),
  ("proofLambda", "proofLambda"), ("showBy", "showBy"), ("do", "doBlock"),
  ("match", "match_"), ("methodChain", "methodChain"), ("if", "ifThenElse"),
  ("let", "letIn"), ("app", "app"), ("lambda", "lambda"),
  ("typeAscription", "typeAscription"), ("variable", "var")
]

/-- Adjust a single weight based on prefer/avoid -/
def adjustSingleWeight (hint : GenHint) (pattern : String) (base : Nat) : Nat :=
  if hint.prefersPattern pattern then min (base * 3 + 20) 100  -- Strong boost
  else if hint.avoidsPattern pattern then 0  -- Completely disable avoided patterns
  else base

/-- Apply pattern preferences/avoidances to weights -/
def applyPatternHints (hint : GenHint) (w : PatternWeights) : PatternWeights :=
  { forall_ := adjustSingleWeight hint "forall" w.forall_
    exists_ := adjustSingleWeight hint "exists" w.exists_
    anonymous := adjustSingleWeight hint "anonymous" w.anonymous
    proofLambda := adjustSingleWeight hint "proofLambda" w.proofLambda
    showBy := adjustSingleWeight hint "showBy" w.showBy
    doBlock := adjustSingleWeight hint "do" w.doBlock
    match_ := adjustSingleWeight hint "match" w.match_
    methodChain := adjustSingleWeight hint "methodChain" w.methodChain
    ifThenElse := adjustSingleWeight hint "if" w.ifThenElse
    letIn := adjustSingleWeight hint "let" w.letIn
    app := adjustSingleWeight hint "app" w.app
    lambda := adjustSingleWeight hint "lambda" w.lambda
    typeAscription := adjustSingleWeight hint "typeAscription" w.typeAscription
    var := adjustSingleWeight hint "variable" w.var }

/-- Build weights based on domain and hints -/
def buildWeights (pools : DomainPools) : GenM PatternWeights := do
  let hint ← getHint
  let style := hint.style
  let complexity := hint.complexity

  -- Base weights depend on domain
  let mut w : PatternWeights := {}

  match pools.name with
  | "mathlib" | "math" | "proof" =>
      w := { w with
        forall_ := 15, exists_ := 10, anonymous := 10, proofLambda := 10
        showBy := 10, typeAscription := 10, app := 15, var := 20
      }
  | "programming" | "prog" | "io" =>
      w := { w with
        doBlock := 20, match_ := 15, methodChain := 15, ifThenElse := 10
        letIn := 10, app := 10, var := 20
      }
  | "meta" | "metaprogramming" | "elab" =>
      w := { w with
        doBlock := 15, match_ := 10, methodChain := 15, typeAscription := 10
        app := 20, var := 30
      }
  | _ =>
      w := { w with
        app := 20, lambda := 15, ifThenElse := 15, letIn := 15, var := 35
      }

  -- Adjust for style preference
  match style with
  | "functional" =>
      w := { w with lambda := w.lambda + 15, match_ := w.match_ + 10
                    doBlock := max w.doBlock 5 - 5 }
  | "imperative" =>
      w := { w with doBlock := w.doBlock + 15, letIn := w.letIn + 10
                    ifThenElse := w.ifThenElse + 10 }
  | "proof" =>
      w := { w with forall_ := w.forall_ + 15, exists_ := w.exists_ + 10
                    showBy := w.showBy + 10, proofLambda := w.proofLambda + 10 }
  | _ => pure ()

  -- Adjust for complexity: low complexity → more simple patterns
  if complexity < 30 then
    w := { w with var := w.var + 30
                  forall_ := w.forall_ / 2, exists_ := w.exists_ / 2
                  doBlock := w.doBlock / 2, match_ := w.match_ / 2 }
  else if complexity > 70 then
    w := { w with var := max w.var 10 - 10
                  forall_ := w.forall_ + 10, doBlock := w.doBlock + 10
                  match_ := w.match_ + 10, methodChain := w.methodChain + 10 }

  -- Apply explicit pattern preferences and avoidances
  w := applyPatternHints hint w

  return w

/-- Select pattern based on weights -/
def selectPattern (w : PatternWeights) : GenM String := do
  let total := w.forall_ + w.exists_ + w.anonymous + w.proofLambda + w.showBy +
               w.doBlock + w.match_ + w.methodChain + w.ifThenElse + w.letIn +
               w.app + w.lambda + w.typeAscription + w.var
  if total == 0 then return "variable"

  let roll ← randBound total
  let mut cumulative := 0

  cumulative := cumulative + w.forall_
  if roll < cumulative then return "forall"
  cumulative := cumulative + w.exists_
  if roll < cumulative then return "exists"
  cumulative := cumulative + w.anonymous
  if roll < cumulative then return "anonymous"
  cumulative := cumulative + w.proofLambda
  if roll < cumulative then return "proofLambda"
  cumulative := cumulative + w.showBy
  if roll < cumulative then return "showBy"
  cumulative := cumulative + w.doBlock
  if roll < cumulative then return "do"
  cumulative := cumulative + w.match_
  if roll < cumulative then return "match"
  cumulative := cumulative + w.methodChain
  if roll < cumulative then return "methodChain"
  cumulative := cumulative + w.ifThenElse
  if roll < cumulative then return "if"
  cumulative := cumulative + w.letIn
  if roll < cumulative then return "let"
  cumulative := cumulative + w.app
  if roll < cumulative then return "app"
  cumulative := cumulative + w.lambda
  if roll < cumulative then return "lambda"
  cumulative := cumulative + w.typeAscription
  if roll < cumulative then return "typeAscription"

  return "variable"

/-- Generate a realistic term for the given domain, respecting hints -/
partial def genDomainTerm (pools : DomainPools) : GenM Syntax := do
  if ← isMaxDepth then
    genVariable pools
  else
    let weights ← buildWeights pools
    let pattern ← selectPattern weights

    match pattern with
    | "forall" => genForallChain pools
    | "exists" => genExists pools
    | "anonymous" => genAnonymousConstructor pools
    | "proofLambda" => genProofLambda pools
    | "showBy" => genShowBy pools
    | "do" => genDoBlock pools
    | "match" => genMatchExpr pools
    | "methodChain" => genMethodChain pools
    | "if" => genIfThenElse pools
    | "let" => genLetIn pools
    | "app" => genApp pools
    | "lambda" => genLambda pools
    | "typeAscription" => genTypeAscription pools
    | _ => genVariable pools

end SyntaxGen.Domain
