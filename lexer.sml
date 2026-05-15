structure Lexer =
struct
  datatype tokenKind =
      Ident of string
    | Constructor of string
    | IntLit of int
    | RealLit of string
    | StringLit of string
    | ByteLit of int
    | BoolLit of bool
    | KwLet | KwRec | KwMut | KwAnd | KwIf | KwElse | KwAs | KwVoid
    | LParen | RParen | LBrace | RBrace | LBracket | RBracket
    | Comma | Semi | Colon | Dot
    | Equals | FatArrow
    | Op of string
    | Question
    | EOF

  type pos = {line : int, col : int}
  datatype token = Token of tokenKind * pos

  exception LexError of string * pos

  fun kindName kind =
    case kind of
        Ident s => "identifier " ^ s
      | Constructor s => "constructor " ^ s
      | IntLit n => "integer " ^ Int.toString n
      | RealLit s => "number " ^ s
      | StringLit _ => "string"
      | ByteLit _ => "byte"
      | BoolLit true => "true"
      | BoolLit false => "false"
      | KwLet => "let"
      | KwRec => "rec"
      | KwMut => "mut"
      | KwAnd => "and"
      | KwIf => "if"
      | KwElse => "else"
      | KwAs => "as"
      | KwVoid => "void"
      | LParen => "("
      | RParen => ")"
      | LBrace => "{"
      | RBrace => "}"
      | LBracket => "["
      | RBracket => "]"
      | Comma => ","
      | Semi => ";"
      | Colon => ":"
      | Dot => "."
      | Equals => "="
      | FatArrow => "=>"
      | Op s => "operator " ^ s
      | Question => "?"
      | EOF => "end of file"

  fun positionText {line, col} =
    Int.toString line ^ ":" ^ Int.toString col

  fun isLower c = #"a" <= c andalso c <= #"z"
  fun isUpper c = #"A" <= c andalso c <= #"Z"
  fun isDigit c = #"0" <= c andalso c <= #"9"
  fun isAlpha c = isLower c orelse isUpper c
  fun isIdentStart c = c = #"_" orelse isLower c
  fun isIdentPart c = c = #"_" orelse isAlpha c orelse isDigit c
  fun isCtorStart c = isUpper c
  fun isSpace c = c = #" " orelse c = #"\t" orelse c = #"\r" orelse c = #"\n"
  fun isOpChar c =
    c = #"+" orelse c = #"-" orelse c = #"*" orelse c = #"/" orelse
    c = #"%" orelse c = #"!" orelse c = #"<" orelse c = #">" orelse
    c = #"|" orelse c = #"&" orelse c = #"="

  fun keyword word =
    case word of
        "let" => KwLet
      | "rec" => KwRec
      | "mut" => KwMut
      | "and" => KwAnd
      | "if" => KwIf
      | "else" => KwElse
      | "as" => KwAs
      | "void" => KwVoid
      | "true" => BoolLit true
      | "false" => BoolLit false
      | _ => Ident word

  fun hexValue c =
    if #"0" <= c andalso c <= #"9" then Char.ord c - Char.ord #"0"
    else if #"a" <= c andalso c <= #"f" then 10 + Char.ord c - Char.ord #"a"
    else if #"A" <= c andalso c <= #"F" then 10 + Char.ord c - Char.ord #"A"
    else ~1

  fun scan source =
    let
      val n = String.size source
      fun chr i = String.sub (source, i)
      fun pos line col = {line = line, col = col}
      fun emit kind line col acc = Token (kind, pos line col) :: acc
      fun advance c line col =
        if c = #"\n" then (line + 1, 1) else (line, col + 1)
      fun slice i j = String.substring (source, i, j - i)

      fun skipLine i line col =
        if i >= n then (i, line, col)
        else
          let val c = chr i
          in
            if c = #"\n" then (i + 1, line + 1, 1)
            else skipLine (i + 1) line (col + 1)
          end

      fun scanIdent i line col acc =
        let
          val start = i
          val startCol = col
          fun loop j ccol =
            if j < n andalso isIdentPart (chr j) then loop (j + 1) (ccol + 1)
            else (j, ccol)
          val (j, endCol) = loop (i + 1) (col + 1)
          val text = slice start j
        in
          loopMain j line endCol (emit (keyword text) line startCol acc)
        end

      and scanCtor i line col acc =
        let
          val start = i
          val startCol = col
          fun loop j ccol =
            if j < n andalso isIdentPart (chr j) then loop (j + 1) (ccol + 1)
            else (j, ccol)
          val (j, endCol) = loop (i + 1) (col + 1)
          val text = slice start j
        in
          loopMain j line endCol (emit (Constructor text) line startCol acc)
        end

      and scanNumber i line col acc =
        let
          val start = i
          val startCol = col
          fun digits j ccol =
            if j < n andalso isDigit (chr j) then digits (j + 1) (ccol + 1)
            else (j, ccol)
          val (afterWhole, colWhole) = digits i col
          val (afterFrac, colFrac, hasFrac) =
            if afterWhole + 1 < n andalso chr afterWhole = #"." andalso isDigit (chr (afterWhole + 1))
            then
              let val (j, ccol) = digits (afterWhole + 1) (colWhole + 1)
              in (j, ccol, true) end
            else (afterWhole, colWhole, false)
          val (afterExp, colExp, hasExp) =
            if afterFrac < n andalso (chr afterFrac = #"e" orelse chr afterFrac = #"E") then
              let
                val j0 = afterFrac + 1
                val c0 = colFrac + 1
                val (j1, c1) =
                  if j0 < n andalso (chr j0 = #"+" orelse chr j0 = #"-")
                  then (j0 + 1, c0 + 1)
                  else (j0, c0)
              in
                if j1 < n andalso isDigit (chr j1) then
                  let val (j2, c2) = digits j1 c1
                  in (j2, c2, true) end
                else raise LexError ("expected exponent digits", pos line c1)
              end
            else (afterFrac, colFrac, false)
          val text = slice start afterExp
          val kind =
            if hasFrac orelse hasExp then RealLit text
            else
              case Int.fromString text of
                  SOME value => IntLit value
                | NONE => raise LexError ("integer literal is too large: " ^ text, pos line startCol)
        in
          loopMain afterExp line colExp (emit kind line startCol acc)
        end

      and escapeAt i line col =
        if i >= n then raise LexError ("unterminated escape", pos line col)
        else
          case chr i of
              #"n" => (10, i + 1, col + 1)
            | #"r" => (13, i + 1, col + 1)
            | #"t" => (9, i + 1, col + 1)
            | #"\\" => (Char.ord #"\\", i + 1, col + 1)
            | #"'" => (Char.ord #"'", i + 1, col + 1)
            | #"\"" => (Char.ord #"\"", i + 1, col + 1)
            | #"x" =>
                if i + 2 < n then
                  let
                    val hi = hexValue (chr (i + 1))
                    val lo = hexValue (chr (i + 2))
                  in
                    if hi >= 0 andalso lo >= 0 then (hi * 16 + lo, i + 3, col + 3)
                    else raise LexError ("expected two hex digits after \\x", pos line col)
                  end
                else raise LexError ("expected two hex digits after \\x", pos line col)
            | c => raise LexError ("unknown escape \\" ^ String.str c, pos line col)

      and scanString i line col acc =
        let
          val startCol = col
          fun loop j ccol chars =
            if j >= n then raise LexError ("unterminated string literal", pos line startCol)
            else
              let val c = chr j
              in
                if c = #"\"" then
                  let val text = String.implode (List.rev chars)
                  in loopMain (j + 1) line (ccol + 1) (emit (StringLit text) line startCol acc) end
                else if c = #"\n" then raise LexError ("newline in string literal", pos line ccol)
                else if c = #"\\" then
                  let
                    val (code, next, nextCol) = escapeAt (j + 1) line (ccol + 1)
                  in
                    loop next nextCol (Char.chr code :: chars)
                  end
                else loop (j + 1) (ccol + 1) (c :: chars)
              end
        in
          loop (i + 1) (col + 1) []
        end

      and scanByte i line col acc =
        let
          val startCol = col
          val (code, next, nextCol) =
            if i + 1 >= n then raise LexError ("unterminated byte literal", pos line startCol)
            else if chr (i + 1) = #"\\" then escapeAt (i + 2) line (col + 2)
            else
              let val c = chr (i + 1)
              in
                if c = #"'" orelse c = #"\n" then raise LexError ("empty or unterminated byte literal", pos line startCol)
                else (Char.ord c, i + 2, col + 2)
              end
        in
          if next < n andalso chr next = #"'" then
            loopMain (next + 1) line (nextCol + 1) (emit (ByteLit code) line startCol acc)
          else raise LexError ("byte literal must contain exactly one byte", pos line nextCol)
        end

      and scanOperator i line col acc =
        let
          val start = i
          val startCol = col
          fun loop j ccol =
            if j < n andalso isOpChar (chr j) then loop (j + 1) (ccol + 1)
            else (j, ccol)
          val (j, endCol) = loop i col
          val text = slice start j
          val kind = case text of "=>" => FatArrow | "=" => Equals | _ => Op text
        in
          loopMain j line endCol (emit kind line startCol acc)
        end

      and loopMain i line col acc =
        if i >= n then List.rev (emit EOF line col acc)
        else
          let val c = chr i
          in
            if isSpace c then
              let val (line2, col2) = advance c line col
              in loopMain (i + 1) line2 col2 acc end
            else if c = #"-" andalso i + 1 < n andalso chr (i + 1) = #"-" then
              let val (i2, line2, col2) = skipLine (i + 2) line (col + 2)
              in loopMain i2 line2 col2 acc end
            else if c = #"/" andalso i + 1 < n andalso chr (i + 1) = #"/" then
              let val (i2, line2, col2) = skipLine (i + 2) line (col + 2)
              in loopMain i2 line2 col2 acc end
            else if isIdentStart c then scanIdent i line col acc
            else if isCtorStart c then scanCtor i line col acc
            else if isDigit c then scanNumber i line col acc
            else
              case c of
                  #"(" => loopMain (i + 1) line (col + 1) (emit LParen line col acc)
                | #")" => loopMain (i + 1) line (col + 1) (emit RParen line col acc)
                | #"{" => loopMain (i + 1) line (col + 1) (emit LBrace line col acc)
                | #"}" => loopMain (i + 1) line (col + 1) (emit RBrace line col acc)
                | #"[" => loopMain (i + 1) line (col + 1) (emit LBracket line col acc)
                | #"]" => loopMain (i + 1) line (col + 1) (emit RBracket line col acc)
                | #"," => loopMain (i + 1) line (col + 1) (emit Comma line col acc)
                | #";" => loopMain (i + 1) line (col + 1) (emit Semi line col acc)
                | #":" =>
                    if i + 1 < n andalso chr (i + 1) = #">" then
                      loopMain (i + 2) line (col + 2) (emit (Op ":>") line col acc)
                    else loopMain (i + 1) line (col + 1) (emit Colon line col acc)
                | #"." => loopMain (i + 1) line (col + 1) (emit Dot line col acc)
                | #"\"" => scanString i line col acc
                | #"'" => scanByte i line col acc
                | #"?" => loopMain (i + 1) line (col + 1) (emit Question line col acc)
                | _ =>
                    if isOpChar c then scanOperator i line col acc
                    else raise LexError ("unexpected character " ^ String.str c, pos line col)
          end
    in
      loopMain 0 1 1 []
    end
end
