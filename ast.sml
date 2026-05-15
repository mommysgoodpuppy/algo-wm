structure Ast =
struct
  datatype literal =
      LInt of int
    | LReal of string
    | LString of string
    | LByte of int
    | LBool of bool
    | LVoid

  datatype ty =
      TyName of string
    | TyVar of string
    | TyTuple of ty list
    | TyArrow of ty list * ty
    | TyApp of ty * ty list
    | TyForall of string list * ty

  datatype pattern =
      PWildcard
    | PVar of string
    | PLit of literal
    | PConstructor of string * pattern list
    | PTuple of pattern list

  datatype expr =
      EVar of string
    | EConstructor of string
    | ELit of literal
    | ETuple of expr list
    | EList of expr list
    | ECall of expr * expr list
    | EField of expr * string
    | EIndex of expr * expr
    | EUnary of string * expr
    | EBinary of string * expr * expr
    | ELambda of param list * expr
    | ELet of binding list * expr
    | EIf of expr * expr * expr
    | EBlock of stmt list * expr option
    | EAs of expr * ty
    | EHole
  and param = Param of pattern * ty option
  and binding = Binding of string * param list * ty option * expr
  and stmt =
      SLet of binding list
    | SExpr of expr

  datatype decl =
      DLet of binding list

  type program = decl list

  fun join sep xs =
    case xs of
        [] => ""
      | x :: rest => List.foldl (fn (s, acc) => acc ^ sep ^ s) x rest

  fun paren s = "(" ^ s ^ ")"

  fun stringOfLiteral lit =
    case lit of
        LInt n => Int.toString n
      | LReal s => s
      | LString s => "\"" ^ String.toString s ^ "\""
      | LByte n => "'" ^ Int.toString n ^ "'"
      | LBool true => "true"
      | LBool false => "false"
      | LVoid => "void"

  fun stringOfTy ty =
    let
      fun go t =
        case t of
            TyName name => name
          | TyVar name => name
          | TyTuple tys => paren (join ", " (List.map go tys))
          | TyArrow (args, ret) => paren (join ", " (List.map go args)) ^ " => " ^ go ret
          | TyApp (base, args) => go base ^ "<" ^ join ", " (List.map go args) ^ ">"
          | TyForall (vars, body) => "<" ^ join ", " vars ^ ">" ^ go body
    in
      go ty
    end

  fun stringOfPattern pat =
    case pat of
        PWildcard => "_"
      | PVar name => name
      | PLit lit => stringOfLiteral lit
      | PConstructor (name, []) => name
      | PConstructor (name, args) => name ^ paren (join ", " (List.map stringOfPattern args))
      | PTuple pats => paren (join ", " (List.map stringOfPattern pats))

  fun stringOfParam (Param (pat, NONE)) = stringOfPattern pat
    | stringOfParam (Param (pat, SOME ty)) = stringOfPattern pat ^ ": " ^ stringOfTy ty

  fun stringOfBinding (Binding (name, params, resultTy, body)) =
    let
      val ps = case params of [] => "" | _ => paren (join ", " (List.map stringOfParam params))
      val rt = case resultTy of NONE => "" | SOME ty => ": " ^ stringOfTy ty
    in
      name ^ ps ^ rt ^ " = " ^ stringOfExpr body
    end
  and stringOfStmt stmt =
    case stmt of
        SLet bindings => "let " ^ join " and " (List.map stringOfBinding bindings) ^ ";"
      | SExpr expr => stringOfExpr expr ^ ";"
  and stringOfExpr expr =
    case expr of
        EVar name => name
      | EConstructor name => name
      | ELit lit => stringOfLiteral lit
      | ETuple exprs => paren (join ", " (List.map stringOfExpr exprs))
      | EList exprs => "[" ^ join ", " (List.map stringOfExpr exprs) ^ "]"
      | ECall (fnExpr, args) => stringOfExpr fnExpr ^ paren (join ", " (List.map stringOfExpr args))
      | EField (recordExpr, field) => stringOfExpr recordExpr ^ "." ^ field
      | EIndex (base, index) => stringOfExpr base ^ "[" ^ stringOfExpr index ^ "]"
      | EUnary (opText, rhs) => opText ^ stringOfExpr rhs
      | EBinary (opText, lhs, rhs) => paren (stringOfExpr lhs ^ " " ^ opText ^ " " ^ stringOfExpr rhs)
      | ELambda (params, body) => paren (join ", " (List.map stringOfParam params)) ^ " => " ^ stringOfExpr body
      | ELet (bindings, body) => "let " ^ join " and " (List.map stringOfBinding bindings) ^ " in " ^ stringOfExpr body
      | EIf (cond, yesExpr, noExpr) => "if (" ^ stringOfExpr cond ^ ") " ^ stringOfExpr yesExpr ^ " else " ^ stringOfExpr noExpr
      | EBlock (stmts, result) =>
          let
            val ss = List.map stringOfStmt stmts
            val all = case result of NONE => ss | SOME e => ss @ [stringOfExpr e]
          in
            "{ " ^ join " " all ^ " }"
          end
      | EAs (inner, ty) => paren (stringOfExpr inner ^ " as " ^ stringOfTy ty)
      | EHole => "?"

  fun stringOfDecl decl =
    case decl of
        DLet bindings => "let " ^ join " and " (List.map stringOfBinding bindings) ^ ";"

  fun stringOfProgram decls =
    join "\n" (List.map stringOfDecl decls)
end
