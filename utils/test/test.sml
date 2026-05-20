fun stringHash s =
  let
    fun loop (i, acc) =
      if i < 0 then acc
      else loop (i - 1, (acc + ord (String.sub (s, i))) mod 4096)
  in
    loop (size s - 1, 0)
  end

val words = ["delta", "alpha", "charlie", "bravo"]
val sortedWords = Lists_.msort String.< words

val intTable : string IntHashTable_.T = IntHashTable_.new 8
val _ = IntHashTable_.update (intTable, 1, "one")
val intLookup = IntHashTable_.tryLookup (intTable, 1)

val stringTable : (string, int) HashTable_.HashTable =
  HashTable_.new (8, op =, stringHash)
val _ = HashTable_.update (stringTable, "answer", 42)
val stringLookup = HashTable_.tryLookup (stringTable, "answer")

val stringMap : (string, int) BTree_.map = BTree_.empty (String.<, op =)
val stringMap = BTree_.define (stringMap, "answer", 42)
val mapLookup = BTree_.tryApply' (stringMap, "answer")

val _ =
  case (sortedWords, intLookup, stringLookup, mapLookup) of
    (["alpha", "bravo", "charlie", "delta"], SOME "one", SOME 42, SOME 42) =>
      print "utils smoke test passed\n"
  | _ =>
      raise Fail "utils smoke test failed"
