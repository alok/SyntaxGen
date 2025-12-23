/-
  SyntaxGen.Export: Export generated syntax to various formats.

  Useful for creating ML training datasets:
  - JSON Lines (one example per line)
  - CSV
  - Pretty-printed Lean code
-/
import Lean
import SyntaxGen.Basic
import SyntaxGen.Weighted

namespace SyntaxGen.Export

open Lean Parser Elab Command Term Meta System

/-! ## JSON Encoding -/

/-- Escape a string for JSON -/
def escapeJson (s : String) : String := Id.run do
  let mut result := ""
  for c in s.toList do
    match c with
    | '\\' => result := result ++ "\\\\"
    | '"' => result := result ++ "\\\""
    | '\n' => result := result ++ "\\n"
    | '\r' => result := result ++ "\\r"
    | '\t' => result := result ++ "\\t"
    | c => result := result.push c
  return result

/-- A generated example with metadata -/
structure GenSample where
  /-- The raw syntax string -/
  text : String
  /-- The syntax category -/
  category : Name
  /-- Approximate complexity (token count) -/
  complexity : Nat
  /-- Random seed used -/
  seed : Nat
  deriving Inhabited, Repr

namespace GenSample

/-- Convert to JSON object string -/
def toJson (s : GenSample) : String :=
  s!"\{\"text\": \"{escapeJson s.text}\", \"category\": \"{s.category}\", \"complexity\": {s.complexity}, \"seed\": {s.seed}}"

/-- Convert to CSV row (text, category, complexity, seed) -/
def toCsv (s : GenSample) : String :=
  s!"\"{escapeJson s.text}\",\"{s.category}\",{s.complexity},{s.seed}"

end GenSample

/-! ## Bulk Generation -/

/-- Count approximate tokens in a string -/
def countTokens (s : String) : Nat :=
  s.splitOn " " |>.filter (·.length > 0) |>.length

/-- Generate many samples with metadata -/
def generateSamples (cat : Name) (count : Nat) (startSeed : Nat := 42) (weighted : Bool := false) : Array GenSample := Id.run do
  let mut results : Array GenSample := #[]
  let mut seed := startSeed

  for _ in [:count * 3] do
    if results.size >= count then break
    let config : GenConfig := { seed := seed, maxDepth := 6 }
    let gen := if weighted then Weighted.genWeighted cat else genFromCat cat
    match runGen config gen with
    | some stx =>
        let text := syntaxToString stx
        let sample : GenSample := {
          text := text
          category := cat
          complexity := countTokens text
          seed := seed
        }
        results := results.push sample
    | none => pure ()
    seed := seed + 7919

  return results

/-! ## Export Commands -/

/-- Generate and show as JSON Lines -/
elab "#syntaxgen_jsonl" cat:Lean.Parser.ident count:(num)? : command => do
  let catName := cat.getId
  let cnt := match count with
    | some n => getNat? n.raw |>.getD 100
    | none => 100

  let samples := generateSamples catName cnt

  logInfo m!"Generated {samples.size} samples for `{catName}` (JSON Lines):"
  for s in samples.toList.take 10 do
    logInfo m!"{s.toJson}"
  if samples.size > 10 then
    logInfo m!"... and {samples.size - 10} more"

/-- Generate and save to file -/
elab "#syntaxgen_export" cat:Lean.Parser.ident count:(num)? "to" path:str : command => do
  let catName := cat.getId
  let cnt := match count with
    | some n => getNat? n.raw |>.getD 1000
    | none => 1000
  let filePath := path.raw.isStrLit?.getD ""

  let samples := generateSamples catName cnt (weighted := true)

  -- Determine format from extension
  let content := if filePath.endsWith ".jsonl" then
    samples.map (·.toJson) |>.toList |> "\n".intercalate
  else if filePath.endsWith ".csv" then
    let header := "text,category,complexity,seed"
    let rows := samples.map (·.toCsv) |>.toList |> "\n".intercalate
    s!"{header}\n{rows}"
  else
    -- Default to pretty-printed
    samples.map (·.text) |>.toList |> "\n".intercalate

  -- Write to file
  IO.FS.writeFile ⟨filePath⟩ content

  logInfo m!"Exported {samples.size} samples to {filePath}"

/-! ## Statistics -/

/-- Show statistics about generated samples -/
elab "#syntaxgen_stats" cat:Lean.Parser.ident count:(num)? : command => do
  let catName := cat.getId
  let cnt := match count with
    | some n => getNat? n.raw |>.getD 100
    | none => 100

  let samples := generateSamples catName cnt (weighted := true)

  if samples.isEmpty then
    logWarning s!"No samples generated for `{catName}`"
    return

  let complexities := samples.map (·.complexity)
  let total := complexities.foldl (· + ·) 0
  let avg := total / samples.size
  let maxC := complexities.foldl max 0
  let minC := complexities.foldl min maxC

  let lengths := samples.map (·.text.length)
  let avgLen := lengths.foldl (· + ·) 0 / samples.size
  let maxLen := lengths.foldl max 0

  logInfo m!"Statistics for {samples.size} `{catName}` samples:"
  logInfo m!"  Token complexity: min={minC}, avg={avg}, max={maxC}"
  logInfo m!"  Character length: avg={avgLen}, max={maxLen}"

  -- Show complexity distribution
  let mut buckets : Array Nat := #[0, 0, 0, 0, 0]  -- 1-5, 6-10, 11-20, 21-50, 50+
  for c in complexities do
    if c <= 5 then buckets := buckets.set! 0 (buckets[0]! + 1)
    else if c <= 10 then buckets := buckets.set! 1 (buckets[1]! + 1)
    else if c <= 20 then buckets := buckets.set! 2 (buckets[2]! + 1)
    else if c <= 50 then buckets := buckets.set! 3 (buckets[3]! + 1)
    else buckets := buckets.set! 4 (buckets[4]! + 1)

  logInfo m!"  Distribution: 1-5={buckets[0]!}, 6-10={buckets[1]!}, 11-20={buckets[2]!}, 21-50={buckets[3]!}, 50+={buckets[4]!}"

/-! ## Batch Generation for Multiple Categories -/

/-- Generate samples for multiple categories -/
elab "#syntaxgen_batch" "[" cats:Lean.Parser.ident,* "]" count:(num)? "to" path:str : command => do
  let catNames := cats.getElems.map (·.getId)
  let cnt := match count with
    | some n => getNat? n.raw |>.getD 100
    | none => 100
  let filePath := path.raw.isStrLit?.getD ""

  let mut allSamples : Array GenSample := #[]
  for cat in catNames do
    let samples := generateSamples cat cnt (weighted := true)
    allSamples := allSamples ++ samples

  let content := allSamples.map (·.toJson) |>.toList |> "\n".intercalate
  IO.FS.writeFile ⟨filePath⟩ content

  logInfo m!"Exported {allSamples.size} samples across {catNames.size} categories to {filePath}"

end SyntaxGen.Export
