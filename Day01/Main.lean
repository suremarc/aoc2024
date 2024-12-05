import Parser

open IO IO.FS.Stream

open Parser Parser.Char.ASCII Parser.Result

inductive TreeSet where
  | empty
  | node (left: TreeSet) (value: Nat) (right: TreeSet)

def TreeSet.insert (t: TreeSet) (v: Nat) : TreeSet :=
  match t with
  | TreeSet.empty => TreeSet.node TreeSet.empty v TreeSet.empty
  | TreeSet.node left value right =>
    if v <= value then
      TreeSet.node (left.insert v) value right
    else
      TreeSet.node left value (right.insert v)

def TreeSet.toList (t: TreeSet) : List Nat :=
  match t with
  | TreeSet.empty => []
  | TreeSet.node left value right => (left.toList ++ [value] ++ right.toList)

-- count occurences
def TreeSet.count (t: TreeSet) (v: Nat) : Nat :=
  match t with
  | TreeSet.empty => 0
  | TreeSet.node left value right =>
    if v < value then
      left.count v
    else if v > value then
      right.count v
    else
      1 + left.count v + right.count v

def parsePair m [Monad m]: SimpleParserT Substring Char m (Nat × Nat) := do
  -- TODO: pointfree way to do this?
  let m ← parseNat
  dropMany <| whitespace
  let n ← parseNat
  return (m, n)

partial def main : IO Unit := do
  let stdin ← IO.getStdin
  -- Repeatedly parse lines and run parsePair on each line
  -- to get a list of (Nat, Nat) pairs
  -- Add the result to a running total
  let rec loop := do
    let line ← stdin.getLine
    if line == "" then
    return (TreeSet.empty, TreeSet.empty)
    else
      match ←  ((parsePair IO).run line) with
        | Result.ok _ (m, n) => do
          let (left, right) ← loop
          return (left.insert m, right.insert n)
        | Result.error _ e =>
          throw (IO.userError s!"parse error: {e}")
  let (left, right) ← loop
  let pairs := left.toList.zip right.toList
  let res := pairs.map (λ (m, n) => Int.natAbs (m-n))
  IO.println s!"total: {res.sum}"

  let multiplicity := left.toList.map (λ n => (n * right.count n))
  IO.println s!"multiplicity: {multiplicity.sum}"
