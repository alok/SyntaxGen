/-
  SyntaxGen.Domain.Maze: Generators for maze game syntax pieces.

  Generates individual components of maze syntax:
  - Cells: ░ (empty), ▓ (wall), @ (player)
  - Rows: │░▓@░│
  - Borders: ─, ┌───┐, └───┘

  Supports guided generation via MazeHint for AI-directed synthesis.
-/
import Lean
import SyntaxGen.Basic

namespace SyntaxGen.Domain.Maze

open Lean SyntaxGen

/-! ## Guided Generation Hints -/

/-- Hints for guiding maze generation -/
structure MazeHint where
  /-- Wall density: 0-100 (percentage of walls) -/
  wallDensity : Nat := 30
  /-- Pattern type: "open", "dense", "corridor", "random" -/
  pattern : String := "random"
  /-- Force player in specific column (0-indexed), none = random -/
  playerCol : Option Nat := none
  /-- Ensure path exists (avoid fully blocked rows) -/
  ensurePath : Bool := true
  deriving Repr, Inhabited

/-- Parse hint string into MazeHint -/
def parseHint (s : String) : MazeHint :=
  let words := s.splitOn " " |>.filter (· != "")
  words.foldl (init := {}) fun hint w =>
    if w == "open" then { hint with wallDensity := 15, pattern := "open" }
    else if w == "dense" then { hint with wallDensity := 60, pattern := "dense" }
    else if w == "corridor" then { hint with wallDensity := 40, pattern := "corridor" }
    else if w == "sparse" then { hint with wallDensity := 10 }
    else if w == "blocked" then { hint with ensurePath := false }
    else if w.startsWith "wall:" then
      match w.drop 5 |>.toNat? with
      | some n => { hint with wallDensity := min n 100 }
      | none => hint
    else hint

/-! ## Maze Cell Generators -/

/-- Generate a single maze cell: ░, ▓, or @ -/
def genCell : GenM Syntax := do
  let roll ← randBound 100
  if roll < 60 then
    return mkAtom "░"  -- empty (most common)
  else if roll < 90 then
    return mkAtom "▓"  -- wall
  else
    return mkAtom "@"  -- player (rare)

/-- Generate a cell with hint-guided density (no player) -/
def genCellWithHint (hint : MazeHint) : GenM Syntax := do
  let roll ← randBound 100
  if roll < (100 - hint.wallDensity) then
    return mkAtom "░"  -- empty
  else
    return mkAtom "▓"  -- wall

/-- Generate an empty cell -/
def genEmptyCell : GenM Syntax := return mkAtom "░"

/-- Generate a wall cell -/
def genWallCell : GenM Syntax := return mkAtom "▓"

/-- Generate a player cell -/
def genPlayerCell : GenM Syntax := return mkAtom "@"

/-! ## Maze Row Generators -/

/-- Generate a sequence of cells for a row -/
def genCellSequence (width : Nat) : GenM (Array Syntax) := do
  let mut cells : Array Syntax := #[]
  for _ in [:width] do
    let cell ← genCell
    cells := cells.push cell
  return cells

/-- Generate a game row: │░▓@░│ -/
def genRow (width : Nat) : GenM Syntax := do
  let cells ← genCellSequence width
  -- Build: │ cell1 cell2 ... │
  let mut parts : Array Syntax := #[mkAtom "│"]
  for c in cells do
    parts := parts.push c
  parts := parts.push (mkAtom "│")
  return Syntax.node .none `null parts

/-- Generate a row with guaranteed player position -/
def genRowWithPlayer (width : Nat) (playerCol : Nat) : GenM Syntax := do
  let mut parts : Array Syntax := #[mkAtom "│"]
  for i in [:width] do
    if i == playerCol then
      parts := parts.push (mkAtom "@")
    else
      let roll ← randBound 100
      if roll < 70 then
        parts := parts.push (mkAtom "░")
      else
        parts := parts.push (mkAtom "▓")
  parts := parts.push (mkAtom "│")
  return Syntax.node .none `null parts

/-- Generate a row with hint guidance -/
def genRowWithHint (width : Nat) (hint : MazeHint) : GenM Syntax := do
  let mut parts : Array Syntax := #[mkAtom "│"]
  let mut hasPath := false

  for i in [:width] do
    let cell ← match hint.pattern with
      | "corridor" =>
          -- Corridor: walls on edges, empty in middle
          if i == 0 || i == width - 1 then pure (mkAtom "▓")
          else do
            hasPath := true
            pure (mkAtom "░")
      | "open" =>
          -- Open: mostly empty
          let roll ← randBound 100
          if roll < 85 then do hasPath := true; pure (mkAtom "░")
          else pure (mkAtom "▓")
      | _ =>
          -- Random with density
          genCellWithHint hint
    -- Track if we have at least one path
    if cell.isAtom && cell.isOfKind `null == false then
      if let some s := cell.isLit? `null then
        if s == "░" then hasPath := true
    parts := parts.push cell

  -- Ensure at least one path cell if required
  if hint.ensurePath && !hasPath && width > 0 then
    let pathIdx ← randBound width
    parts := parts.set! (pathIdx + 1) (mkAtom "░")  -- +1 for leading │

  parts := parts.push (mkAtom "│")
  return Syntax.node .none `null parts

/-- Generate a wall row (all walls) -/
def genWallRow (width : Nat) : GenM Syntax := do
  let mut parts : Array Syntax := #[mkAtom "│"]
  for _ in [:width] do
    parts := parts.push (mkAtom "▓")
  parts := parts.push (mkAtom "│")
  return Syntax.node .none `null parts

/-- Generate an empty row (all empty) -/
def genEmptyRow (width : Nat) : GenM Syntax := do
  let mut parts : Array Syntax := #[mkAtom "│"]
  for _ in [:width] do
    parts := parts.push (mkAtom "░")
  parts := parts.push (mkAtom "│")
  return Syntax.node .none `null parts

/-! ## Border Generators -/

/-- Generate horizontal border segment: ─ -/
def genBorderSegment : GenM Syntax := return mkAtom "─"

/-- Generate top border: ┌───┐ -/
def genTopBorder (width : Nat) : GenM Syntax := do
  let mut parts : Array Syntax := #[mkAtom "┌"]
  for _ in [:width] do
    parts := parts.push (mkAtom "─")
  parts := parts.push (mkAtom "┐")
  return Syntax.node .none `null parts

/-- Generate bottom border: └───┘ -/
def genBottomBorder (width : Nat) : GenM Syntax := do
  let mut parts : Array Syntax := #[mkAtom "└"]
  for _ in [:width] do
    parts := parts.push (mkAtom "─")
  parts := parts.push (mkAtom "┘")
  return Syntax.node .none `null parts

/-! ## Composite Generators -/

/-- Generate a small maze section (just a few rows, not a complete maze) -/
def genMazeSection (width : Nat) (height : Nat) : GenM Syntax := do
  let mut rows : Array Syntax := #[]
  for _ in [:height] do
    let row ← genRow width
    rows := rows.push row
    rows := rows.push (mkAtom "\n")
  return Syntax.node .none `null rows

/-- Generate a guided maze section with hints -/
def genGuidedMazeSection (width : Nat) (height : Nat) (hint : MazeHint) : GenM Syntax := do
  let mut rows : Array Syntax := #[]

  -- Decide which row gets the player
  let playerRow ← randBound height
  let playerCol ← match hint.playerCol with
    | some c => pure (min c (width - 1))
    | none => randBound width

  for i in [:height] do
    let row ← if i == playerRow then
        -- This row has the player
        genRowWithPlayer width playerCol
      else
        genRowWithHint width hint
    rows := rows.push row
    rows := rows.push (mkAtom "\n")

  return Syntax.node .none `null rows

/-- Generate a maze corridor pattern -/
def genCorridor (length : Nat) : GenM Syntax := do
  let mut parts : Array Syntax := #[mkAtom "│▓"]
  for _ in [:length] do
    parts := parts.push (mkAtom "░")
  parts := parts.push (mkAtom "▓│")
  return Syntax.node .none `null parts

/-- Generate a random maze piece (cell, row, or border) -/
def genMazePiece : GenM Syntax := do
  let roll ← randBound 100
  if roll < 30 then
    -- Single cell
    genCell
  else if roll < 60 then
    -- Row (3-6 cells)
    let width ← randBound 4
    genRow (width + 3)
  else if roll < 80 then
    -- Top border
    let width ← randBound 4
    genTopBorder (width + 3)
  else
    -- Bottom border
    let width ← randBound 4
    genBottomBorder (width + 3)

/-! ## Pretty Printing -/

/-- Pretty print maze syntax -/
def prettyMaze (stx : Syntax) : String :=
  match stx with
  | .atom _ val => val
  | .node _ _ args =>
      let parts := args.map prettyMaze |>.toList
      String.join parts
  | _ => ""

/-! ## Commands -/

/-- Generate and display maze pieces -/
elab "#syntaxgen_maze_piece" count:(num)? : command => do
  let cnt := match count with
    | some n => getNat? n.raw |>.getD 5
    | none => 5

  let mut results : Array String := #[]
  let mut seed : Nat := 42

  for _ in [:cnt] do
    let config : GenConfig := { seed := seed }
    seed := seed + 7919
    match runGen config genMazePiece with
    | some stx => results := results.push (prettyMaze stx)
    | none => pure ()

  if results.isEmpty then
    logWarning "Could not generate maze pieces."
  else
    logInfo m!"Generated {results.size} maze pieces:"
    for i in [:results.size] do
      logInfo m!"  {i + 1}. {results[i]!}"

/-- Generate maze rows -/
elab "#syntaxgen_maze_row" width:(num)? count:(num)? : command => do
  let w := match width with
    | some n => getNat? n.raw |>.getD 5
    | none => 5
  let cnt := match count with
    | some n => getNat? n.raw |>.getD 5
    | none => 5

  let mut results : Array String := #[]
  let mut seed : Nat := 42

  for _ in [:cnt] do
    let config : GenConfig := { seed := seed }
    seed := seed + 7919
    match runGen config (genRow w) with
    | some stx => results := results.push (prettyMaze stx)
    | none => pure ()

  logInfo m!"Generated {results.size} maze rows (width {w}):"
  for i in [:results.size] do
    logInfo m!"  {i + 1}. {results[i]!}"

/-- Generate maze cells -/
elab "#syntaxgen_maze_cell" count:(num)? : command => do
  let cnt := match count with
    | some n => getNat? n.raw |>.getD 10
    | none => 10

  let mut results : Array String := #[]
  let mut seed : Nat := 42

  for _ in [:cnt] do
    let config : GenConfig := { seed := seed }
    seed := seed + 7919
    match runGen config genCell with
    | some stx => results := results.push (prettyMaze stx)
    | none => pure ()

  let cellStr := String.intercalate " " results.toList
  logInfo m!"Generated {results.size} maze cells: {cellStr}"

/-- Generate maze borders -/
elab "#syntaxgen_maze_border" width:(num)? : command => do
  let w := match width with
    | some n => getNat? n.raw |>.getD 5
    | none => 5

  let mut seed : Nat := 42
  let topConfig : GenConfig := { seed := seed }
  let botConfig : GenConfig := { seed := seed + 1 }

  match runGen topConfig (genTopBorder w), runGen botConfig (genBottomBorder w) with
  | some top, some bot =>
      logInfo m!"Maze borders (width {w}):"
      logInfo m!"  Top:    {prettyMaze top}"
      logInfo m!"  Bottom: {prettyMaze bot}"
  | _, _ => logWarning "Could not generate borders."

/-- Generate a maze section (multiple rows) -/
elab "#syntaxgen_maze_section" width:(num)? height:(num)? : command => do
  let w := match width with
    | some n => getNat? n.raw |>.getD 4
    | none => 4
  let h := match height with
    | some n => getNat? n.raw |>.getD 3
    | none => 3

  let config : GenConfig := { seed := 42 }
  let topConfig : GenConfig := { seed := 43 }
  let botConfig : GenConfig := { seed := 44 }

  match runGen topConfig (genTopBorder w),
        runGen config (genMazeSection w h),
        runGen botConfig (genBottomBorder w) with
  | some top, some rows, some bot =>
      logInfo m!"Maze section ({w}x{h}):"
      logInfo m!"{prettyMaze top}"
      logInfo m!"{prettyMaze rows}{prettyMaze bot}"
  | _, _, _ => logWarning "Could not generate maze section."

/-- Generate a guided maze with hints
    Hints: "open", "dense", "corridor", "sparse", "wall:N" (N = 0-100)
    Example: #syntaxgen_maze_guided 5 4 "corridor"
             #syntaxgen_maze_guided 6 5 "dense wall:70"
-/
elab "#syntaxgen_maze_guided" width:(num)? height:(num)? hint:(str)? : command => do
  let w := match width with
    | some n => getNat? n.raw |>.getD 5
    | none => 5
  let h := match height with
    | some n => getNat? n.raw |>.getD 4
    | none => 4
  let hintStr := match hint with
    | some s => s.getString
    | none => "random"

  let mazeHint := parseHint hintStr

  let config : GenConfig := { seed := 42 }
  let topConfig : GenConfig := { seed := 43 }
  let botConfig : GenConfig := { seed := 44 }

  match runGen topConfig (genTopBorder w),
        runGen config (genGuidedMazeSection w h mazeHint),
        runGen botConfig (genBottomBorder w) with
  | some top, some rows, some bot =>
      logInfo m!"Guided maze ({w}x{h}) with hint \"{hintStr}\":"
      logInfo m!"{prettyMaze top}"
      logInfo m!"{prettyMaze rows}{prettyMaze bot}"
  | _, _, _ => logWarning "Could not generate guided maze."

/-- Generate multiple maze variations with different hints -/
elab "#syntaxgen_maze_variations" width:(num)? height:(num)? : command => do
  let w := match width with
    | some n => getNat? n.raw |>.getD 5
    | none => 5
  let h := match height with
    | some n => getNat? n.raw |>.getD 3
    | none => 3

  let hints := #["open", "dense", "corridor", "sparse"]

  for i in [:hints.size] do
    let hintStr := hints[i]!
    let mazeHint := parseHint hintStr
    let config : GenConfig := { seed := 42 + i * 100 }
    let topConfig : GenConfig := { seed := 43 + i * 100 }
    let botConfig : GenConfig := { seed := 44 + i * 100 }

    match runGen topConfig (genTopBorder w),
          runGen config (genGuidedMazeSection w h mazeHint),
          runGen botConfig (genBottomBorder w) with
    | some top, some rows, some bot =>
        logInfo m!"\n{hintStr.toUpper} maze:"
        logInfo m!"{prettyMaze top}"
        logInfo m!"{prettyMaze rows}{prettyMaze bot}"
    | _, _, _ => pure ()

end SyntaxGen.Domain.Maze
