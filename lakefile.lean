import Lake
open Lake DSL

package SyntaxGen where
  version := v!"0.1.0"

-- Main library
@[default_target]
lean_lib SyntaxGen where
  roots := #[`SyntaxGen]

-- CLI executable
@[default_target]
lean_exe syntaxgen where
  root := `Main
