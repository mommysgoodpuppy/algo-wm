fun readFile path =
  let
    val ins = TextIO.openIn path
    fun loop chunks =
      case TextIO.inputLine ins of
          "" => String.concat (List.rev chunks)
        | line => loop (line :: chunks)
    val text = loop []
    val _ = TextIO.closeIn ins
  in
    text
  end

fun parseAndPrint source =
  let
    val program = Parser.parseString source
  in
    print (Ast.stringOfProgram program ^ "\n")
  end
  handle Lexer.LexError (msg, pos) =>
      print ("lex error at " ^ Lexer.positionText pos ^ ": " ^ msg ^ "\n")
    | Parser.ParseError (msg, pos) =>
      print ("parse error at " ^ Lexer.positionText pos ^ ": " ^ msg ^ "\n")

val sample =
  "let id(x) = x;\n" ^
  "let add(x: Number, y: Number): Number = x + y;\n" ^
  "let choose(flag) = if (flag) { id(1) } else { add(1, 2) };\n"

fun main () =
  case CommandLine.arguments () of
      [] => parseAndPrint sample
    | [path] =>
      (parseAndPrint (readFile path)
       handle IO.Io _ => print ("could not read source file: " ^ path ^ "\n"))
    | _ => print "usage: mlw run sml.mlb [source.wm]\n"

val _ = main ()
