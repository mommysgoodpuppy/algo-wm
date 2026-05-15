type name = string

datatype expr =
    Var of name (*variable*)
    | Call of expr * expr list (*application*)
    | Fun of name list * expr (*abstraction*)
    | Let of name * expr * expr (*let*)

type id = int
type level = int

datatype ty =
	TConst of name           (* type constant: `int` or `bool` *)
	| TApp of ty * ty list     (* type application: `list[int]` *)
	| TArrow of ty list * ty   (* function type: `(int, int) -> int` *)
	| TVar of tvar ref         (* type variable *)

and tvar =
	Unbound of id * level
	| Link of ty
	| Generic of id;

infix 6 ++
val op ++ = op ^

fun string_of_expr expr : string =
    let fun f is_simple e = case e of
        Var name => name
        | Call (fn_expr,arg_list) =>
            f true fn_expr ++ "(" ++ String.concatWith ", "(List.map (f false) arg_list) ++")"
        | Fun (param_list, body_expr) =>
            let val fun_str =
                "fun " ++ String.concatWith " " param_list ++ " => " ++ f false body_expr
            in
                if is_simple then "(" ++ fun_str ++ ")" else fun_str
            end
        | Let (var_name, value_expr, body_expr) =>
            let val let_str =
                "let " ++ var_name ++ " = " ++ f false value_expr ++ " in " ++ f false body_expr
            in
                if is_simple then "(" ++ let_str ++ ")" else let_str
            end
    in
        f false expr
    end

fun str_make n c = CharVector.tabulate (n, fn _ => c)

fun string_of_ty ty : string =
    let
        val id_name_map = HashTable.mkTable (HashString.hashString, op =) (10, Fail "Not found")
        val count = ref 0
        fun incr r = (r := !r + 1)

        fun next_name () =
            let
                val i = !count
            in
                incr count;
                implode [chr (97 + i mod 26)] ++
                (if i >= 26 then Int.toString (i div 26) else "")
            end
        
        fun f is_simple e = case e of
            TConst name => name
            | TApp(ty, ty_arg_list) =>
                f true ty ++ "[" ++ String.concatWith ", " (List.map (f false) ty_arg_list ) ++ "]"
            | TArrow(param_ty_list, return_ty) =>
                let val arrow_ty_str =
                case param_ty_list of
                    [param_ty] =>
                        let
                            val param_ty_str = f true param_ty
                            val return_ty_str = f false return_ty
                        in
                            param_ty_str ++ " => " ++ return_ty_str
                        end
                    | _ =>
                        let
                            val param_ty_list_str = String.concatWith ", " (List.map (f false) param_ty_list)
                            val return_ty_str = f false return_ty
						in
						    "(" ++ param_ty_list_str ^ ") -> " ++ return_ty_str
						end
        in
            if is_simple then "(" ++ arrow_ty_str ++ ")" else arrow_ty_str
        end
        | TVar {contents = Generic id} => begin
            try
            HashTable.find id_name_map id
            with Not_found =>
                let name = next_name () in
                HashTable.insert id_name_map id name ;
                name
                end
        
    in



end

