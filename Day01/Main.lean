import Parser

open IO IO.FS.Stream

open Parser Parser.Char.ASCII

def parsePair m [Monad m]: SimpleParserT Substring Char m (Nat × Nat) := do
  -- TODO: pointfree way to do this?
  let m ← parseNat
  dropMany <| whitespace
  let n ← parseNat
  return (m, n)

def main : IO Unit := do
  let stdin ← IO.getStdin
  let mut sum := 0
  -- Repeatedly parse lines and run parsePair on each line
  -- to get a list of (Nat, Nat) pairs
  let rec loop := do
    let line ← stdin.getLine
    if line == "" then
      IO.println s!"Sum: {sum}"
    else
      let (m, n) ← (parsePair.run line)?
      m + n + loop
  loop
