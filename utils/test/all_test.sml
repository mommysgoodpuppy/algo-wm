structure Counter_ = Counter ()

structure TestCrash : CRASH =
  struct
    fun impossible s = raise Fail ("impossible: " ^ s)
    fun unimplemented s = raise Fail ("unimplemented: " ^ s)
  end

structure IntHashSet =
  HashSet(
    structure Crash = TestCrash
    structure Lists = Lists_
    type element = int
    val eq = op =
    fun hash i = i)

val counterStart = Counter_.counter ()
val counterNext = Counter_.counter ()

val setA = Set_.list_to_set [1, 2, 2, 3]
val setHasTwo = Set_.is_member (2, setA)

val hashSetA = IntHashSet.add_list (IntHashSet.empty_set 4, [1, 2, 3])
val hashSetHasThree = IntHashSet.is_member (hashSetA, 3)

val intTree : string IntBTree_.T = IntBTree_.empty
val intTree = IntBTree_.define (intTree, 7, "seven")
val intTreeLookup = IntBTree_.tryApply' (intTree, 7)

val renderedText = Text_.from_string "mlworks"
val _ = Print_.print "print utility ok\n"

val sexprText = Sexpr_.printSexpr (fn s => s) (Sexpr_.ATOM "x")
val special = LispUtils_.svref 1
val lispValue = LispUtils_.letv [(special, 2)] (fn () => LispUtils_.!! special)

val _ =
  if counterStart = 0
     andalso counterNext = 1
     andalso setHasTwo
     andalso hashSetHasThree
     andalso intTreeLookup = SOME "seven"
     andalso sexprText = "x"
     andalso lispValue = 2
  then print "expanded utils smoke test passed\n"
  else raise Fail "expanded utils smoke test failed"
