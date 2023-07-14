open AstML
open Format

let fmt_string = pp_print_string
let fmt_variable fmt { basic_ident; _ } = fmt_string fmt basic_ident
let fmt_with_string str = pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt str)
let fmt_with_space pp fmt l = fmt_with_string " " pp fmt l
let fmt_with_comma pp fmt l = fmt_with_string ", " pp fmt l
let fmt_with_semicolon pp fmt l = fmt_with_string "; " pp fmt l
let fmt_with_mult pp fmt l = fmt_with_string "* " pp fmt l

let rec fmt_type fmt t =
  let fmt_string = fmt_string fmt in
  match t.etype with
  | TypeInt -> fmt_string "int"
  | TypeBool -> fmt_string "bool"
  | TypeUnit -> fmt_string "()"
  | TypeTuple type_ls -> fprintf fmt "(%a)" (fmt_with_mult fmt_type) type_ls
  | TypeLambda { arg; return_type } ->
    fprintf fmt "(%a -> %a)" fmt_type arg fmt_type return_type
  | TypeVar vartype -> fmt_string ("'" ^ vartype)
  | TypeDefined t -> fmt_string t
  | TypeConstructor construct ->
    fprintf
      fmt
      "(%a %a)"
      (fmt_with_space fmt_type)
      construct.parameters
      fmt_type
      construct.to_build
  | TypeMonadic (eff, t) ->
    fprintf fmt "M[%a](%a)" fmt_eff eff fmt_type t

and fmt_eff fmt = function
  | Ground -> ()
  | State (t,Ground) -> fprintf fmt "ST(%a)" fmt_type t
  | Except (t,Ground) -> fprintf fmt "EXN(%a)" fmt_type t
 | State (t,eff) -> fprintf fmt "ST(%a);%a" fmt_type t fmt_eff eff
 | Except (t,eff) -> fprintf fmt "EXN(%a);%a" fmt_type t fmt_eff eff



;;

let fmt_unary_op fmt op =
  pp_print_string
    fmt
    (match op with
    | Autobill.Lcbpv.Not -> "!"
    | Autobill.Lcbpv.Opp -> "-")
;;

let fmt_binary_op fmt op =
  pp_print_string
    fmt
    (match op with
    | Autobill.Lcbpv.Add -> "+"
    | Autobill.Lcbpv.Mult -> "*"
    | Autobill.Lcbpv.Subs -> "-"
    | Autobill.Lcbpv.Div -> "/"
    | Autobill.Lcbpv.Mod -> "%"
    | Autobill.Lcbpv.And -> "and"
    | Autobill.Lcbpv.Or -> "or"
    | Autobill.Lcbpv.Int_Eq -> "="
    | Autobill.Lcbpv.Int_Leq -> "<="
    | Autobill.Lcbpv.Int_Lt -> "<")
;;

let fmt_litteral fmt = function
  | Integer i -> pp_print_int fmt i
  | Boolean b -> pp_print_bool fmt b
  | Unit -> pp_print_string fmt "()"
;;

let rec fmt_pattern fmt ptt =
  match ptt.pnode with
  | LitteralPattern litteral -> fmt_litteral fmt litteral
  | VarPattern string -> pp_print_string fmt string
  | WildcardPattern -> pp_print_char fmt '_'
  | TuplePattern pattern_ls -> fprintf fmt "(%a)" (fmt_with_comma fmt_pattern) pattern_ls
  | ConstructorPattern { constructor_ident; content } ->
    fprintf
      fmt
      "(%a(%a))"
      fmt_string
      constructor_ident
      (fmt_with_comma fmt_pattern)
      content
;;

let rec fmt_expr fmt exp =
  match exp.enode with
  | Litteral litteral -> fmt_litteral fmt litteral
  | Variable variable -> fmt_variable fmt variable
  | Call { func; arg } -> fprintf fmt "(%a %a)" fmt_expr func fmt_expr arg
  | CallUnary { op; arg = Some arg } -> fprintf fmt "(%a%a)" fmt_unary_op op fmt_expr arg
  | CallUnary { op; arg = None } -> fprintf fmt "(%a)" fmt_unary_op op
  | CallBinary { op; args = first :: second :: _ } ->
    fprintf fmt "(%a %a %a)" fmt_expr first fmt_binary_op op fmt_expr second
  | CallBinary { op; args = [] } -> fprintf fmt "(%a)" fmt_binary_op op
  | CallBinary { op; args = [ arg ] } ->
    fprintf fmt "(%a %a)" fmt_binary_op op fmt_expr arg
  | Sequence expr_ls -> fprintf fmt "(%a)" (fmt_with_semicolon fmt_expr) expr_ls
  | Binding { var; init; content } ->
    fprintf
      fmt
      "@[let %a = @[%a@] in @[%a@] @]"
      fmt_variable
      var
      fmt_expr
      init
      fmt_expr
      content
  | Lambda { arg; body } ->
    fprintf fmt "@[ fun %a -> @[%a@] @]" fmt_variable arg fmt_expr body
  | Tuple expr_ls -> fprintf fmt "(%a)" (fmt_with_comma fmt_expr) expr_ls
  | Construct { constructor_ident; to_group } ->
    fprintf fmt "(%a(%a))" fmt_string constructor_ident (fmt_with_comma fmt_expr) to_group
  | FunctionRec { var; arg; body } ->
    fprintf
      fmt
      "@[let rec %a %a = @[%a@] in @[%a@] @]"
      fmt_variable
      var
      fmt_variable
      arg
      fmt_expr
      body
      fmt_variable
      var
  | Match { to_match; cases } ->
    fprintf
      fmt
      "(match %a with @[%a@])"
      fmt_expr
      to_match
      (pp_print_list ~pp_sep:pp_print_cut fmt_case)
      cases
  | Do s -> fprintf fmt "do %a" fmt_block s
  | BindMonadic (x, f, eff) -> fprintf fmt "bind[%a] %a %a" fmt_eff eff fmt_expr x fmt_expr f
  | Return (e, eff) -> fprintf fmt "pure[%a] %a" fmt_eff eff fmt_expr e
  | If (e1, e2, e3) -> fprintf fmt "if! %a then %a else %a" fmt_expr e1 fmt_expr e2 fmt_expr e3
  | Get -> fprintf fmt "get"
  | Set x -> fprintf fmt "set %a" fmt_expr x
  | RunState (e1, e2) -> fprintf fmt "runST %a %a" fmt_expr e1 fmt_expr e2
  | LiftState (e, eff) -> fprintf fmt "liftST[%a] %a" fmt_eff eff fmt_expr e
  | ThrowEx e -> fprintf fmt "throw %a" fmt_expr e
  | LiftEx (e, eff) -> fprintf fmt "liftEXN[%a] %a" fmt_eff eff fmt_expr e
  | RunCatch e -> fprintf fmt "runEXN %a" fmt_expr e
  | ForM (e1, e2) -> fprintf fmt "forM %a %a" fmt_expr e1 fmt_expr e2

and fmt_block fmt b = fprintf fmt "@[<v 0>{@[<v 2>@,%a@]@,}@]" fmt_block_inner b

and fmt_block_inner fmt b = match b.snode with
  | Stmt_pure e
  | Stmt_return e -> fprintf fmt "pure %a" fmt_expr e
  | Stmt_let (x, s, s')
    -> fprintf fmt "let %a = do %a;@,%a"
         fmt_variable x
         fmt_block s
         fmt_block_inner s'
  | Stmt_if (e, s1, s2)
    -> fprintf fmt "if %a then {@[<v 2>@,%a@]@,} else{@[<v 2>@,%a@]@,};"
         fmt_expr e
         fmt_block_inner s1
         fmt_block_inner s2
  | Stmt_mut (x, e, s)
    -> fprintf fmt "let mut %a = %a;@,%a"
         fmt_variable x
         fmt_expr e
         fmt_block_inner s
  | Stmt_mut_change (x, e, s) ->
    fprintf fmt "%a := %a;@,%a"
      fmt_variable x
      fmt_expr e
      fmt_block_inner s

  (* | Stmt_get -> _ *)
  (* | Stmt_set _ -> _ *)
  (* | Stmt_early_return _ -> _ *)
  (* | Stmt_lift_st _ -> _ *)
  (* | Stmt_throw _ -> _ *)
  (* | Stmt_lift _ -> _ *)

  | Stmt_break -> fprintf fmt "break"
  | Stmt_continue -> fprintf fmt "continue"
  | Stmt_for (x, e, s)
    -> fprintf fmt "for %a in %a do %a"
         fmt_variable x
         fmt_expr e
         fmt_block s


and fmt_case fmt case =
  fprintf fmt "@[| %a -> %a @]" fmt_pattern case.pattern fmt_expr case.consequence
;;

let fmt_construtors fmt newConstr =
  fprintf
    fmt
    "@[| %a of %a @]"
    fmt_string
    newConstr.constructor_ident
    (fmt_with_mult fmt_type)
    newConstr.c_of
;;

let fmt_def fmt d =
  match d.dnode with
  | VariableDef { var; init } -> fprintf fmt "let %a = %a" fmt_variable var fmt_expr init
  | TypeDef { basic_ident; parameters; constructors } ->
    fprintf
      fmt
      "type %a %a = @ %a"
      (fmt_with_space fmt_string)
      (List.map (fun x -> "'" ^ x) parameters)
      fmt_string
      basic_ident
      (pp_print_list ~pp_sep:pp_print_cut fmt_construtors)
      constructors
;;

let fmt_prog_node fmt = function
  | Def def -> fmt_def fmt def
  | Expr expr -> fmt_expr fmt expr
;;

let fmt_with_double_semicolon pp fmt l = fmt_with_string ";;@ " pp fmt l

let fmp_prog fmt prog =
  pp_open_vbox fmt 0;
  fmt_with_double_semicolon fmt_prog_node fmt prog;
  pp_print_cut fmt ();
  pp_close_box fmt ()
;;
