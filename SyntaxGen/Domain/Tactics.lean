/-
  SyntaxGen.Domain.Tactics: Stateful multi-step tactic sequence generators.

  Generates coherent tactic sequences that track introduced hypotheses and variables,
  producing realistic proof scripts.
-/
import Lean
import SyntaxGen.Basic
import SyntaxGen.Domain.Pools

namespace SyntaxGen.Domain

open Lean SyntaxGen

/-! ## Tactic Sequence State -/

/-- State for generating coherent tactic sequences -/
structure TacticSeqState where
  /-- Hypotheses introduced so far (by intro, have, etc.) -/
  hypotheses : Array String := #[]
  /-- Variables introduced so far -/
  variables : Array String := #[]
  /-- Whether we're inside a case split (cases/induction) -/
  inCaseSplit : Bool := false
  /-- Current step number -/
  stepNumber : Nat := 0
  /-- Domain pools for name generation -/
  pools : DomainPools := mathlibPools
  deriving Repr, Inhabited

/-- Extended generator monad with tactic state -/
abbrev TacGenM := StateT TacticSeqState GenM

/-- Lift GenM to TacGenM -/
def liftGen (m : GenM α) : TacGenM α := StateT.lift m

/-! ## State Helpers -/

/-- Add a hypothesis to the state -/
def addHypothesis (name : String) : TacGenM Unit :=
  modify fun st => { st with hypotheses := st.hypotheses.push name }

/-- Add a variable to the state -/
def addVariable (name : String) : TacGenM Unit :=
  modify fun st => { st with variables := st.variables.push name }

/-- Increment step counter -/
def nextStep : TacGenM Nat := do
  let st ← get
  modify fun st => { st with stepNumber := st.stepNumber + 1 }
  return st.stepNumber

/-- Get a random hypothesis from state, or generate fresh one -/
def getHypothesis : TacGenM String := do
  let st ← get
  if st.hypotheses.isEmpty then
    let h := s!"h{st.stepNumber}"
    addHypothesis h
    return h
  else
    liftGen (randChoice st.hypotheses)

/-- Get a random variable from state, or generate fresh one -/
def getVariable : TacGenM String := do
  let st ← get
  if st.variables.isEmpty then
    let v := s!"x{st.stepNumber}"
    addVariable v
    return v
  else
    liftGen (randChoice st.variables)

/-- Generate a fresh hypothesis name -/
def genFreshHyp : TacGenM String := do
  let st ← get
  let idx := st.hypotheses.size
  let base ← liftGen (randChoice #["h", "hp", "hq", "hx"])
  let name := if idx == 0 then base else s!"{base}{idx}"
  addHypothesis name
  return name

/-- Generate a fresh variable name -/
def genFreshVar : TacGenM String := do
  let st ← get
  let idx := st.variables.size
  let base ← liftGen (randChoice #["x", "y", "n", "m"])
  let name := if idx == 0 then base else s!"{base}{idx}"
  addVariable name
  return name

/-! ## Individual Tactic Generators -/

/-- Generate intro tactic: `intro h` or `intro x y z` -/
def genIntro : TacGenM Syntax := do
  let roll ← liftGen (randBound 100)
  if roll < 60 then
    -- intro h (hypothesis)
    let h ← genFreshHyp
    return Syntax.node .none `null #[mkAtom "intro", mkIdent' h]
  else
    -- intro x y (variables)
    let numVars ← liftGen (randBound 2)
    let mut args : Array Syntax := #[mkAtom "intro"]
    for _ in [:numVars + 1] do
      let v ← genFreshVar
      args := args.push (mkIdent' v)
    return Syntax.node .none `null args

/-- Generate intros tactic: `intros` -/
def genIntros : TacGenM Syntax := do
  -- Add some hypotheses/variables to state
  for _ in [:2] do
    let h ← genFreshHyp
    pure ()
  return mkAtom "intros"

/-- Generate have tactic: `have h := e` or `have h : T := e` -/
def genHave : TacGenM Syntax := do
  let st ← get
  let h ← genFreshHyp
  let roll ← liftGen (randBound 100)

  if roll < 50 then
    -- have h := lemma
    let lemma ← liftGen (genQualifiedName st.pools)
    return Syntax.node .none `null #[mkAtom "have", mkIdent' h, mkAtom ":=", lemma]
  else if roll < 80 && !st.hypotheses.isEmpty then
    -- have h := existing_h.method
    let existing ← getHypothesis
    let method ← liftGen (randChoice #[".symm", ".trans", ".mp", ".mpr"])
    return Syntax.node .none `null #[
      mkAtom "have", mkIdent' h, mkAtom ":=", mkIdent' existing, mkAtom method
    ]
  else
    -- have h : T := by tac
    let ty ← liftGen (randChoice #["True", "P", "Q", "a = a"])
    let tac ← liftGen (randChoice #["rfl", "trivial", "simp"])
    return Syntax.node .none `null #[
      mkAtom "have", mkIdent' h, mkAtom ":", mkIdent' ty, mkAtom ":=",
      mkAtom "by", mkAtom tac
    ]

/-- Generate obtain tactic: `obtain ⟨a, b⟩ := h` -/
def genObtain : TacGenM Syntax := do
  let st ← get
  let a ← genFreshVar
  let b ← genFreshVar
  let h ← if st.hypotheses.isEmpty then genFreshHyp else getHypothesis
  return Syntax.node .none `null #[
    mkAtom "obtain", mkAtom "⟨", mkIdent' a, mkAtom ",", mkIdent' b, mkAtom "⟩",
    mkAtom ":=", mkIdent' h
  ]

/-- Generate cases tactic: `cases h` or `cases h with | ... => ...` -/
def genCases : TacGenM Syntax := do
  let st ← get
  let h ← if st.hypotheses.isEmpty then genFreshHyp else getHypothesis
  modify fun st => { st with inCaseSplit := true }

  let roll ← liftGen (randBound 100)
  if roll < 50 then
    -- Simple: cases h
    return Syntax.node .none `null #[mkAtom "cases", mkIdent' h]
  else
    -- With arms: cases h with | ctor1 => tac1 | ctor2 => tac2
    let ctor1 ← liftGen (randChoice #["inl", "inr", "zero", "succ", "nil", "cons", "some", "none"])
    let tac1 ← liftGen (randChoice #["rfl", "simp", "trivial", "exact this"])
    let ctor2 ← liftGen (randChoice #["inl", "inr", "zero", "succ", "nil", "cons", "some", "none"])
    let tac2 ← liftGen (randChoice #["rfl", "simp", "trivial", "assumption"])
    return Syntax.node .none `null #[
      mkAtom "cases", mkIdent' h, mkAtom "with",
      mkAtom "|", mkAtom ctor1, mkAtom "=>", mkAtom tac1,
      mkAtom "|", mkAtom ctor2, mkAtom "=>", mkAtom tac2
    ]

/-- Generate induction tactic: `induction n` or `induction n with | ... => ...` -/
def genInduction : TacGenM Syntax := do
  let st ← get
  let v ← if st.variables.isEmpty then genFreshVar else getVariable
  modify fun st => { st with inCaseSplit := true }

  let roll ← liftGen (randBound 100)
  if roll < 50 then
    return Syntax.node .none `null #[mkAtom "induction", mkIdent' v]
  else
    let tac1 ← liftGen (randChoice #["rfl", "simp", "trivial"])
    let tac2 ← liftGen (randChoice #["simp [ih]", "exact ih", "rfl"])
    return Syntax.node .none `null #[
      mkAtom "induction", mkIdent' v, mkAtom "with",
      mkAtom "|", mkAtom "zero", mkAtom "=>", mkAtom tac1,
      mkAtom "|", mkAtom "succ", mkIdent' "n", mkIdent' "ih", mkAtom "=>", mkAtom tac2
    ]

/-- Generate simp tactic: `simp`, `simp only`, or `simp [lemmas]` -/
def genSimp : TacGenM Syntax := do
  let st ← get
  let roll ← liftGen (randBound 100)

  if roll < 30 then
    return mkAtom "simp"
  else if roll < 50 then
    return Syntax.node .none `null #[mkAtom "simp", mkAtom "only"]
  else if roll < 70 then
    -- simp at h
    let h ← if st.hypotheses.isEmpty then genFreshHyp else getHypothesis
    return Syntax.node .none `null #[mkAtom "simp", mkAtom "at", mkIdent' h]
  else
    -- simp [lemma1, lemma2]
    let numLemmas ← liftGen (randBound 2)
    let mut args : Array Syntax := #[mkAtom "simp", mkAtom "["]
    for i in [:numLemmas + 1] do
      if i > 0 then args := args.push (mkAtom ",")
      let lemma ← liftGen (genQualifiedName st.pools)
      args := args.push lemma
    args := args.push (mkAtom "]")
    return Syntax.node .none `null args

/-- Generate rw tactic: `rw [lemma]` or `rw [lemma] at h` -/
def genRewrite : TacGenM Syntax := do
  let st ← get
  let lemma ← liftGen (genQualifiedName st.pools)

  let roll ← liftGen (randBound 100)
  if roll < 60 || st.hypotheses.isEmpty then
    return Syntax.node .none `null #[mkAtom "rw", mkAtom "[", lemma, mkAtom "]"]
  else
    let h ← getHypothesis
    return Syntax.node .none `null #[
      mkAtom "rw", mkAtom "[", lemma, mkAtom "]", mkAtom "at", mkIdent' h
    ]

/-- Generate apply tactic: `apply lemma` -/
def genApply : TacGenM Syntax := do
  let st ← get
  let lemma ← liftGen (genQualifiedName st.pools)
  return Syntax.node .none `null #[mkAtom "apply", lemma]

/-- Generate exact tactic: `exact h` or `exact lemma` -/
def genExact : TacGenM Syntax := do
  let st ← get
  let roll ← liftGen (randBound 100)

  if roll < 50 && !st.hypotheses.isEmpty then
    let h ← getHypothesis
    return Syntax.node .none `null #[mkAtom "exact", mkIdent' h]
  else if roll < 70 then
    let lemma ← liftGen (genQualifiedName st.pools)
    return Syntax.node .none `null #[mkAtom "exact", lemma]
  else
    return Syntax.node .none `null #[mkAtom "exact", mkAtom "this"]

/-- Generate constructor/left/right tactic -/
def genConstructorTactic : TacGenM Syntax := do
  let roll ← liftGen (randBound 100)
  if roll < 40 then
    return mkAtom "constructor"
  else if roll < 60 then
    return mkAtom "left"
  else if roll < 80 then
    return mkAtom "right"
  else
    return mkAtom "ext"

/-- Generate trivial closing tactics -/
def genClosing : TacGenM Syntax := do
  let roll ← liftGen (randBound 100)
  if roll < 20 then return mkAtom "rfl"
  else if roll < 35 then return mkAtom "trivial"
  else if roll < 45 then return mkAtom "decide"
  else if roll < 60 then return mkAtom "assumption"
  else if roll < 70 then return mkAtom "contradiction"
  else if roll < 80 then return mkAtom "ring"
  else if roll < 90 then return mkAtom "omega"
  else return mkAtom "linarith"

/-! ## Tactic Sequence Generator -/

/-- Check if a tactic is a closing tactic -/
def isClosingTactic (stx : Syntax) : Bool :=
  match stx with
  | .atom _ s => s ∈ ["rfl", "trivial", "decide", "assumption", "contradiction",
                      "ring", "omega", "linarith", "done"]
  | .node _ _ args =>
      if let some first := args[0]? then
        match first with
        | .atom _ s => s ∈ ["exact", "exfalso"]
        | _ => false
      else false
  | _ => false

/-- Generate the next tactic based on current state and step number -/
def genNextTactic : TacGenM Syntax := do
  let st ← get
  let step := st.stepNumber
  let _ ← nextStep
  let roll ← liftGen (randBound 100)

  -- Early phase: intro, have, obtain
  if step < 2 then
    if roll < 50 then genIntro
    else if roll < 70 then genHave
    else if roll < 85 then genObtain
    else genIntros

  -- Mid phase: cases, induction, simp, rw, apply
  else if step < 4 then
    if roll < 25 && !st.hypotheses.isEmpty then genCases
    else if roll < 40 && !st.variables.isEmpty then genInduction
    else if roll < 60 then genSimp
    else if roll < 80 then genRewrite
    else genApply

  -- Late phase: closing tactics
  else
    if roll < 50 then genClosing
    else if roll < 70 then genExact
    else if roll < 85 then genSimp
    else genConstructorTactic

/-- Generate a coherent tactic sequence -/
def genTacticSeq (pools : DomainPools) (maxSteps : Nat := 5) : GenM (Array Syntax) := do
  let initState : TacticSeqState := { pools := pools }
  let gen : TacGenM (Array Syntax) := do
    let numSteps ← liftGen (randBound maxSteps)
    let mut tactics : Array Syntax := #[]

    for _ in [:numSteps + 1] do
      let tac ← genNextTactic
      tactics := tactics.push tac
      -- Stop if we generated a closing tactic
      if isClosingTactic tac then break

    return tactics

  (gen.run initState).map Prod.fst

/-- Format a tactic sequence with semicolons -/
def formatTacticSeq (tactics : Array Syntax) : Syntax :=
  if tactics.isEmpty then
    mkAtom "sorry"
  else if tactics.size == 1 then
    tactics[0]!
  else Id.run do
    let mut result : Array Syntax := #[]
    for i in [:tactics.size] do
      if i > 0 then result := result.push (mkAtom ";")
      result := result.push tactics[i]!
    return Syntax.node .none `null result

/-- Generate a formatted tactic sequence -/
def genFormattedTacticSeq (pools : DomainPools) (maxSteps : Nat := 5) : GenM Syntax := do
  let tactics ← genTacticSeq pools maxSteps
  return formatTacticSeq tactics

end SyntaxGen.Domain
