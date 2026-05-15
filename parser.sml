structure Parser =
struct
  open Ast
  open Lexer

  exception ParseError of string * pos

  fun tokenPos toks =
    case toks of
        Token (_, p) :: _ => p
      | [] => {line = 0, col = 0}

  fun tokenKind toks =
    case toks of
        Token (k, _) :: _ => k
      | [] => EOF

  fun fail toks msg = raise ParseError (msg, tokenPos toks)

  fun sameKind (a, b) =
    case (a, b) of
        (KwLet, KwLet) => true
      | (KwRec, KwRec) => true
      | (KwMut, KwMut) => true
      | (KwAnd, KwAnd) => true
      | (KwIf, KwIf) => true
      | (KwElse, KwElse) => true
      | (KwAs, KwAs) => true
      | (KwVoid, KwVoid) => true
      | (LParen, LParen) => true
      | (RParen, RParen) => true
      | (LBrace, LBrace) => true
      | (RBrace, RBrace) => true
      | (LBracket, LBracket) => true
      | (RBracket, RBracket) => true
      | (Comma, Comma) => true
      | (Semi, Semi) => true
      | (Colon, Colon) => true
      | (Dot, Dot) => true
      | (Equals, Equals) => true
      | (FatArrow, FatArrow) => true
      | (Question, Question) => true
      | (EOF, EOF) => true
      | _ => false

  fun accept expected toks =
    case toks of
        Token (kind, _) :: rest => if sameKind (kind, expected) then SOME rest else NONE
      | [] => NONE

  fun expect expected toks =
    case accept expected toks of
        SOME rest => rest
      | NONE => fail toks ("expected " ^ kindName expected ^ ", found " ^ kindName (tokenKind toks))

  fun expectIdent toks =
    case toks of
        Token (Ident name, _) :: rest => (name, rest)
      | _ => fail toks ("expected identifier, found " ^ kindName (tokenKind toks))

  fun expectBindingName toks =
    case toks of
        Token (Ident name, _) :: rest => (name, rest)
      | Token (LParen, _) :: Token (Op text, _) :: Token (RParen, _) :: rest => (text, rest)
      | _ => fail toks ("expected binding name, found " ^ kindName (tokenKind toks))

  fun precedence text =
    case text of
        "*" => SOME 130
      | "/" => SOME 130
      | "%" => SOME 130
      | "+" => SOME 120
      | "-" => SOME 120
      | "++" => SOME 120
      | "<" => SOME 100
      | "<=" => SOME 100
      | ">" => SOME 100
      | ">=" => SOME 100
      | "==" => SOME 90
      | "!=" => SOME 90
      | "&&" => SOME 60
      | "||" => SOME 50
      | ":>" => SOME 30
      | _ => SOME 110

  fun parseProgram toks =
    let
      fun loop ts acc =
        case tokenKind ts of
            EOF => List.rev acc
          | KwLet =>
              let
                val (decl, rest) = parseLetDecl ts
              in
                loop rest (decl :: acc)
              end
          | _ => fail ts ("expected top-level declaration, found " ^ kindName (tokenKind ts))
    in
      loop toks []
    end

  and parseLetDecl toks =
    let
      val ts1 = expect KwLet toks
      val ts2 = skipLetModifiers ts1
      val (bindings, ts3) = parseBindingGroup ts2
      val ts4 = expect Semi ts3
    in
      (DLet bindings, ts4)
    end

  and skipLetModifiers toks =
    case tokenKind toks of
        KwRec => skipLetModifiers (expect KwRec toks)
      | KwMut => skipLetModifiers (expect KwMut toks)
      | _ => toks

  and parseBindingGroup toks =
    let
      val (first, rest) = parseBinding toks
      fun more ts acc =
        case accept KwAnd ts of
            SOME afterAnd =>
              let val (b, afterB) = parseBinding afterAnd
              in more afterB (b :: acc) end
          | NONE => (List.rev acc, ts)
    in
      more rest [first]
    end

  and parseBinding toks =
    let
      val (name, ts1) = expectBindingName toks
      val (params, ts2) =
        case accept LParen ts1 of
            SOME afterOpen => parseParamListTail afterOpen
          | NONE => ([], ts1)
      val (resultTy, ts3) =
        case accept Colon ts2 of
            SOME afterColon =>
              let val (ty, afterTy) = parseType afterColon
              in (SOME ty, afterTy) end
          | NONE => (NONE, ts2)
      val ts4 = expect Equals ts3
      val (body, ts5) = parseExpr ts4
    in
      (Binding (name, params, resultTy, body), ts5)
    end

  and parseParamListTail toks =
    case accept RParen toks of
        SOME rest => ([], rest)
      | NONE =>
          let
            val (first, rest) = parseParam toks
            fun loop ts acc =
              case accept Comma ts of
                  SOME afterComma =>
                    (case accept RParen afterComma of
                         SOME afterClose => (List.rev acc, afterClose)
                       | NONE =>
                           let val (p, afterP) = parseParam afterComma
                           in loop afterP (p :: acc) end)
                | NONE => (List.rev acc, expect RParen ts)
          in
            loop rest [first]
          end

  and parseParam toks =
    let
      val (pat, ts1) = parsePattern toks
      val (tyOpt, ts2) =
        case accept Colon ts1 of
            SOME afterColon =>
              let val (ty, afterTy) = parseType afterColon
              in (SOME ty, afterTy) end
          | NONE => (NONE, ts1)
    in
      (Param (pat, tyOpt), ts2)
    end

  and parseExpr toks =
    case tokenKind toks of
        KwIf => parseIf toks
      | LBrace => parseBlock toks
      | FatArrow =>
          let
            val afterArrow = expect FatArrow toks
            val (body, rest) = parseBlock afterArrow
          in
            (ELambda ([], body), rest)
          end
      | LParen =>
          (case tryLambda toks of
               SOME result => result
             | NONE => parseBinary 0 toks)
      | _ => parseBinary 0 toks

  and tryLambda toks =
    let
      val afterOpen = expect LParen toks
      val (params, afterParams) = parseParamListTail afterOpen
      val afterArrow = expect FatArrow afterParams
      val (body, rest) = parseBlock afterArrow
    in
      SOME (ELambda (params, body), rest)
    end handle ParseError _ => NONE

  and parseIf toks =
    let
      val ts1 = expect KwIf toks
      val ts2 = expect LParen ts1
      val (cond, ts3) = parseExpr ts2
      val ts4 = expect RParen ts3
      val (yesExpr, ts5) = parseBlock ts4
      val ts6 = expect KwElse ts5
      val (noExpr, ts7) = parseBlock ts6
    in
      (EIf (cond, yesExpr, noExpr), ts7)
    end

  and parseBlock toks =
    let
      val ts1 = expect LBrace toks
      fun loop ts stmts =
        case tokenKind ts of
            RBrace => (EBlock (List.rev stmts, NONE), expect RBrace ts)
          | KwLet =>
              let
                val afterLet = expect KwLet ts
                val afterMods = skipLetModifiers afterLet
                val (bindings, afterBindings) = parseBindingGroup afterMods
                val afterSemi = expect Semi afterBindings
              in
                loop afterSemi (SLet bindings :: stmts)
              end
          | _ =>
              let
                val (expr, afterExpr) = parseExpr ts
              in
                case accept Semi afterExpr of
                    SOME afterSemi => loop afterSemi (SExpr expr :: stmts)
                  | NONE =>
                      let val afterClose = expect RBrace afterExpr
                      in (EBlock (List.rev stmts, SOME expr), afterClose) end
              end
    in
      loop ts1 []
    end

  and parseBinary minPrec toks =
    let
      val (lhs, rest) = parsePrefix toks
      fun loop left ts =
        case ts of
            Token (Op text, _) :: afterOp =>
              (case precedence text of
                   SOME p =>
                     if p < minPrec then (left, ts)
                     else
                       let
                         val (right, afterRight) = parseBinary (p + 1) afterOp
                       in
                         loop (EBinary (text, left, right)) afterRight
                       end
                 | NONE => (left, ts))
          | _ => (left, ts)
    in
      loop lhs rest
    end

  and parsePrefix toks =
    case toks of
        Token (Op text, _) :: rest =>
          if text = "!" orelse text = "-" then
            let val (rhs, afterRhs) = parsePrefix rest
            in (EUnary (text, rhs), afterRhs) end
          else parsePostfix toks
      | _ => parsePostfix toks

  and parsePostfix toks =
    let
      val (base, rest) = parsePrimary toks
      fun loop expr ts =
        case tokenKind ts of
            LParen =>
              let val (args, afterArgs) = parseArgList ts
              in loop (ECall (expr, args)) afterArgs end
          | Dot =>
              let
                val afterDot = expect Dot ts
                val (field, afterField) = expectIdent afterDot
              in
                loop (EField (expr, field)) afterField
              end
          | LBracket =>
              let
                val afterOpen = expect LBracket ts
                val (index, afterIndex) = parseExpr afterOpen
                val afterClose = expect RBracket afterIndex
              in
                loop (EIndex (expr, index)) afterClose
              end
          | KwAs =>
              let
                val afterAs = expect KwAs ts
                val (ty, afterTy) = parseType afterAs
              in
                loop (EAs (expr, ty)) afterTy
              end
          | _ => (expr, ts)
    in
      loop base rest
    end

  and parseArgList toks =
    let val ts1 = expect LParen toks
    in
      case accept RParen ts1 of
          SOME rest => ([], rest)
        | NONE =>
            let
              val (first, rest) = parseExpr ts1
              fun loop ts acc =
                case accept Comma ts of
                    SOME afterComma =>
                      (case accept RParen afterComma of
                           SOME afterClose => (List.rev acc, afterClose)
                         | NONE =>
                             let val (e, afterE) = parseExpr afterComma
                             in loop afterE (e :: acc) end)
                  | NONE => (List.rev acc, expect RParen ts)
            in
              loop rest [first]
            end
    end

  and parsePrimary toks =
    case toks of
        Token (Ident name, _) :: rest => (EVar name, rest)
      | Token (Constructor name, _) :: rest => (EConstructor name, rest)
      | Token (IntLit value, _) :: rest => (ELit (LInt value), rest)
      | Token (RealLit text, _) :: rest => (ELit (LReal text), rest)
      | Token (StringLit text, _) :: rest => (ELit (LString text), rest)
      | Token (ByteLit value, _) :: rest => (ELit (LByte value), rest)
      | Token (BoolLit value, _) :: rest => (ELit (LBool value), rest)
      | Token (KwVoid, _) :: rest => (ELit LVoid, rest)
      | Token (Question, _) :: rest => (EHole, rest)
      | Token (LBracket, _) :: rest => parseListLiteral rest
      | Token (LParen, _) :: rest => parseParenExpr rest
      | Token (LBrace, _) :: _ => parseBlock toks
      | _ => fail toks ("expected expression, found " ^ kindName (tokenKind toks))

  and parseListLiteral toks =
    case accept RBracket toks of
        SOME rest => (EList [], rest)
      | NONE =>
          let
            val (first, rest) = parseExpr toks
            fun loop ts acc =
              case accept Comma ts of
                  SOME afterComma =>
                    (case accept RBracket afterComma of
                         SOME afterClose => (EList (List.rev acc), afterClose)
                       | NONE =>
                           let val (e, afterE) = parseExpr afterComma
                           in loop afterE (e :: acc) end)
                | NONE => (EList (List.rev acc), expect RBracket ts)
          in
            loop rest [first]
          end

  and parseParenExpr toks =
    let
      val (first, rest) = parseExpr toks
    in
      case accept Comma rest of
          SOME afterComma =>
            let
              val (second, afterSecond) = parseExpr afterComma
              fun loop ts acc =
                case accept Comma ts of
                    SOME afterComma2 =>
                      (case accept RParen afterComma2 of
                           SOME afterClose => (ETuple (List.rev acc), afterClose)
                         | NONE =>
                             let val (e, afterE) = parseExpr afterComma2
                             in loop afterE (e :: acc) end)
                  | NONE => (ETuple (List.rev acc), expect RParen ts)
            in
              loop afterSecond [second, first]
            end
        | NONE => (first, expect RParen rest)
    end

  and parsePattern toks =
    case toks of
        Token (Ident "_", _) :: rest => (PWildcard, rest)
      | Token (Ident name, _) :: rest => (PVar name, rest)
      | Token (Constructor name, _) :: rest =>
          (case accept LParen rest of
               SOME afterOpen =>
                 let
                   val (args, afterArgs) = parsePatternArgs afterOpen
                 in
                   (PConstructor (name, args), afterArgs)
                 end
             | NONE => (PConstructor (name, []), rest))
      | Token (IntLit value, _) :: rest => (PLit (LInt value), rest)
      | Token (StringLit text, _) :: rest => (PLit (LString text), rest)
      | Token (BoolLit value, _) :: rest => (PLit (LBool value), rest)
      | Token (LParen, _) :: rest =>
          let
            val (first, afterFirst) = parsePattern rest
            val afterComma = expect Comma afterFirst
            val (second, afterSecond) = parsePattern afterComma
            fun loop ts acc =
              case accept Comma ts of
                  SOME afterComma2 =>
                    (case accept RParen afterComma2 of
                         SOME afterClose => (PTuple (List.rev acc), afterClose)
                       | NONE =>
                           let val (p, afterP) = parsePattern afterComma2
                           in loop afterP (p :: acc) end)
                | NONE => (PTuple (List.rev acc), expect RParen ts)
          in
            loop afterSecond [second, first]
          end
      | _ => fail toks ("expected pattern, found " ^ kindName (tokenKind toks))

  and parsePatternArgs toks =
    case accept RParen toks of
        SOME rest => ([], rest)
      | NONE =>
          let
            val (first, rest) = parsePattern toks
            fun loop ts acc =
              case accept Comma ts of
                  SOME afterComma =>
                    (case accept RParen afterComma of
                         SOME afterClose => (List.rev acc, afterClose)
                       | NONE =>
                           let val (p, afterP) = parsePattern afterComma
                           in loop afterP (p :: acc) end)
                | NONE => (List.rev acc, expect RParen ts)
          in
            loop rest [first]
          end

  and parseType toks =
    case toks of
        Token (Op "<", _) :: afterLt =>
          let
            val (vars, afterVars) = parseTypeVarList afterLt
            val (body, afterBody) = parseTypeArrow afterVars
          in
            (TyForall (vars, body), afterBody)
          end
      | _ => parseTypeArrow toks

  and parseTypeVar toks =
    case toks of
        Token (Ident name, _) :: rest => (name, rest)
      | _ => fail toks ("expected type parameter name, found " ^ kindName (tokenKind toks))

  and parseTypeVarList toks =
    let
      val (first, rest) = parseTypeVar toks
      fun loop ts acc =
        case accept Comma ts of
            SOME afterComma =>
              let val (name, afterName) = parseTypeVar afterComma
              in loop afterName (name :: acc) end
          | NONE =>
              (case ts of
                   Token (Op ">", _) :: afterGt => (List.rev acc, afterGt)
                 | _ => fail ts "expected > after type parameters")
    in
      loop rest [first]
    end

  and parseTypeArrow toks =
    let
      val (left, rest) = parseTypePrimary toks
    in
      case accept FatArrow rest of
          SOME afterArrow =>
            let
              val (ret, afterRet) = parseTypeArrow afterArrow
              val args = case left of TyTuple xs => xs | _ => [left]
            in
              (TyArrow (args, ret), afterRet)
            end
        | NONE => (left, rest)
    end

  and parseTypePrimary toks =
    let
      val (base, rest) =
        case toks of
            Token (Constructor name, _) :: after => (TyName name, after)
          | Token (Ident name, _) :: after => (TyVar name, after)
          | Token (LParen, _) :: after => parseParenType after
          | _ => fail toks ("expected type expression, found " ^ kindName (tokenKind toks))
      fun loop ty ts =
        case ts of
            Token (Op "<", _) :: afterLt =>
              let val (args, afterArgs) = parseTypeArgs afterLt
              in loop (TyApp (ty, args)) afterArgs end
          | _ => (ty, ts)
    in
      loop base rest
    end

  and parseParenType toks =
    case accept RParen toks of
        SOME rest => (TyTuple [], rest)
      | NONE =>
    let
      val (first, rest) = parseType toks
    in
      case accept Comma rest of
          SOME afterComma =>
            let
              val (second, afterSecond) = parseType afterComma
              fun loop ts acc =
                case accept Comma ts of
                    SOME afterComma2 =>
                      (case accept RParen afterComma2 of
                           SOME afterClose => (TyTuple (List.rev acc), afterClose)
                         | NONE =>
                             let val (ty, afterTy) = parseType afterComma2
                             in loop afterTy (ty :: acc) end)
                  | NONE => (TyTuple (List.rev acc), expect RParen ts)
            in
              loop afterSecond [second, first]
            end
        | NONE => (first, expect RParen rest)
    end

  and parseTypeArgs toks =
    let
      val (first, rest) = parseType toks
      fun loop ts acc =
        case accept Comma ts of
            SOME afterComma =>
              let val (ty, afterTy) = parseType afterComma
              in loop afterTy (ty :: acc) end
          | NONE =>
              (case ts of
                   Token (Op ">", _) :: afterGt => (List.rev acc, afterGt)
                 | _ => fail ts "expected > after type arguments")
    in
      loop rest [first]
    end

  fun parseString source =
    parseProgram (Lexer.scan source)
end
