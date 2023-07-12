(*TODO Creation d'operateurs infixes *)
(*TODO Creation de records ? *)
(*TODO Lazyness *)
(*TODO Recursion Mutuelle (Type et Fonction)  *)
(*TODO Type Synonym *)

open AstML
open Autobill.Lcbpv
open Autobill.Misc

exception Error of string

let generate_variable pos = HelpersML.generate_name (), pos

let trans_boolean = function
  | true -> True
  | false -> False
;;

let trans_var x = x.basic_ident, x.vloc
let trans_var_ls = List.map trans_var

let trans_litl = function
  | Integer i -> Expr_Int i
  | Boolean b -> Expr_Constructor (trans_boolean b, [])
  | Unit -> Expr_Constructor (Unit, [])
;;

let rec make_unary_closure op loc =
  let arg = { basic_ident = HelpersML.generate_name (); vloc = loc } in
  let expr_var = Some { enode = Variable arg; eloc = loc } in
  let e, _ =
    trans_expr
      (HelpersML.func_curryfy
         [ arg ]
         { enode = CallUnary { arg = expr_var; op }; eloc = loc })
  in
  e

and make_binary_closure args op eloc =
  let arg1 = { basic_ident = HelpersML.generate_name (); vloc = eloc } in
  let expr_var1 = { enode = Variable arg1; eloc } in
  let closure, _ =
    trans_expr
      (match args with
       | [] ->
         let arg2 = { basic_ident = HelpersML.generate_name (); vloc = eloc } in
         let expr_var2 = { enode = Variable arg2; eloc } in
         HelpersML.func_curryfy
           [ arg1; arg2 ]
           { enode = CallBinary { args = [ expr_var1; expr_var2 ]; op }; eloc }
       | [ hd ] ->
         HelpersML.func_curryfy
           [ arg1 ]
           { enode = CallBinary { args = [ expr_var1; hd ]; op }; eloc }
       | _ -> HelpersML.err "Unexpected number of arguments on binary closure" eloc)
  in
  closure

and sub_x var (s : statement) : statement =
  match s.snode with
  | Stmt_pure e -> { s with snode = Stmt_lift e }
  | Stmt_return _ -> s (* TODO *)
  | Stmt_let (var', s1, s2) ->
    assert (var' <> var);
    { s with
      snode =
        Stmt_let
          ( var'
          , sub_x var s1
          , { sloc = s.sloc
            ; snode = Stmt_let (var, { sloc = s.sloc; snode = Stmt_get }, sub_x var s2)
            } )
    }
  | Stmt_if (e, s1, s2) -> { s with snode = Stmt_if (e, sub_x var s1, sub_x var s2) }
  | Stmt_mut (var', e, s) ->
    assert (var' <> var);
    { s with snode = Stmt_mut (var', e, sub_x var s) }
  | Stmt_mut_change (var', e) -> if var = var' then { s with snode = Stmt_set e } else s
  | Stmt_for (var', e, s) ->
    { s with
      snode =
        Stmt_for
          ( var'
          , e
          , { sloc = s.sloc
            ; snode = Stmt_let (var, { sloc = s.sloc; snode = Stmt_get }, sub_x var s)
            } )
    }
  | _ -> s

and remove_mut_change_set stmt =
  match stmt.snode with
  | Stmt_mut_change_set (var, e, s) ->
    { snode =
        Stmt_let
          ( { basic_ident = "_"; vloc = stmt.sloc }
          , { snode = Stmt_mut_change (var, e); sloc = stmt.sloc }
          , remove_mut_change_set s )
    ; sloc = stmt.sloc
    }
  | Stmt_pure _ -> stmt
  | Stmt_return _ -> stmt
  | Stmt_let (var, s1, s2) ->
    { stmt with
      snode = Stmt_let (var, remove_mut_change_set s1, remove_mut_change_set s2)
    }
  | Stmt_if (e, s1, s2) ->
    { stmt with snode = Stmt_if (e, remove_mut_change_set s1, remove_mut_change_set s2) }
  | Stmt_mut (var, e, s) ->
    { stmt with snode = Stmt_mut (var, e, remove_mut_change_set s) }
  | Stmt_mut_change _ -> stmt
  | Stmt_break -> stmt
  | Stmt_continue -> stmt
  | Stmt_for (var, e, s) ->
    { stmt with snode = Stmt_for (var, e, remove_mut_change_set s) }
  | _ -> assert false

and trans_do stmt =
  let rec transL stmt =
    match stmt.snode with
    | Stmt_pure e -> { stmt with snode = Stmt_lift e }
    | Stmt_return _ -> assert false (*No early return in for loop *)
    | Stmt_let (var, s1, s2) -> { stmt with snode = Stmt_let (var, transL s1, transL s2) }
    | Stmt_mut (var, e, s) -> { stmt with snode = Stmt_mut (var, e, transL s) }
    | Stmt_if (e, s1, s2) -> { stmt with snode = Stmt_if (e, transL s1, transL s2) }
    | Stmt_for (var, e, s) -> { stmt with snode = Stmt_for (var, e, transL s) }
    | _ -> stmt
  in
  let rec transC stmt =
    match stmt.snode with
    | Stmt_break -> stmt
    | Stmt_continue ->
      { stmt with
        snode =
          Stmt_lift
            { eloc = stmt.sloc
            ; enode = ThrowEx { eloc = stmt.sloc; enode = Litteral Unit }
            }
      }
    | Stmt_pure e -> { stmt with snode = Stmt_lift e }
    | Stmt_return _ -> assert false (* No early return in for loop *) 
    | Stmt_let (var, s1, s2) -> { stmt with snode = Stmt_let (var, transC s1, transC s2) }
    | Stmt_mut (var, e, s) -> { stmt with snode = Stmt_mut (var, e, transC s) }
    | Stmt_if (e, s1, s2) -> { stmt with snode = Stmt_if (e, transC s1, transC s2) }
    | Stmt_for (var, e, s) -> { stmt with snode = Stmt_for (var, e, transC s) }
    | _ -> stmt
  in
  let rec transB stmt =
    match stmt.snode with
    | Stmt_break ->
      { stmt with snode = Stmt_throw { eloc = stmt.sloc; enode = Litteral Unit } }
    | Stmt_return _ -> assert false (* No early return in for loop*)
    | Stmt_pure e -> { stmt with snode = Stmt_lift e }
    | Stmt_let (var, s1, s2) -> { stmt with snode = Stmt_let (var, transB s1, transB s2) }
    | Stmt_if (e, s1, s2) -> { stmt with snode = Stmt_if (e, transB s1, transB s2) }
    | Stmt_for (var, e, s) -> { stmt with snode = Stmt_for (var, e, transL s) }
    | _ -> stmt
  in
  let rec transD stmt eff =
    match stmt.snode with
    | Stmt_pure e -> e
    | Stmt_return e -> { enode = Return (e, eff); eloc = stmt.sloc }
    | Stmt_let (var, s1, s2) ->
      { enode =
          BindMonadic
            ( transD s1 eff
            , { enode = Lambda { arg = var; body = transD s2 eff }; eloc = stmt.sloc }
            , eff )
      ; eloc = stmt.sloc
      }
    | Stmt_if (e, s1, s2) ->
      { enode = If (e, transD s1 eff, transD s2 eff); eloc = stmt.sloc }
    | Stmt_mut (var, e, s) ->
      { eloc = stmt.sloc
      ; enode =
          Binding
            { var
            ; init = e
            ; content =
                { eloc = stmt.sloc
                ; enode =
                    RunState
                      ( transD
                          (sub_x var s)
                          (State ({ etype = TypeUnit; tloc = stmt.sloc }, eff))
                      , { eloc = stmt.sloc; enode = Variable var } )
                }
                (* State + effets de ^ *)
            }
      }
    | Stmt_mut_change (_, _) -> assert false (* Should disappear with sub_x*)
    | Stmt_mut_change_set _ -> assert false
    | Stmt_get -> { eloc = stmt.sloc; enode = Get }
    | Stmt_set e -> { eloc = stmt.sloc; enode = Set e }
    | Stmt_lift_st e -> { eloc = stmt.sloc; enode = LiftState (e, eff) }
    | Stmt_throw e -> { eloc = stmt.sloc; enode = ThrowEx e }
    | Stmt_lift e -> { eloc = stmt.sloc; enode = LiftEx (e, eff) }
    | Stmt_for (var, e, s) ->
      { eloc = stmt.sloc
      ; enode =
          RunCatch
            { eloc = stmt.sloc
            ; enode =
                ForM
                  ( e
                  , { eloc = stmt.sloc
                    ; enode =
                        Lambda
                          { arg = var
                          ; body =
                              { eloc = stmt.sloc
                              ; enode =
                                  RunCatch
                                    (transD
                                       (transC (transB s))
                                       (Except
                                          ( { etype = TypeUnit; tloc = stmt.sloc }
                                          , Except
                                              ({ etype = TypeUnit; tloc = stmt.sloc }, eff)
                                          )))
                              }
                          }
                    } )
            }
      }
    | Stmt_break -> assert false
    | Stmt_continue -> assert false
  in
  let rec transR stmt =
    match stmt.snode with
    | Stmt_return e -> { stmt with snode = Stmt_throw e }
    | Stmt_pure e -> { stmt with snode = Stmt_lift e }
    | Stmt_let (var, s1, s2) -> { stmt with snode = Stmt_let (var, transR s1, transR s2) }
    | Stmt_if (e, s1, s2) -> { stmt with snode = Stmt_if (e, transR s1, transR s2) }
    | Stmt_mut (var, e, s) -> { stmt with snode = Stmt_mut (var, e, transR s) }
    | Stmt_mut_change (_, _) -> stmt
    | Stmt_mut_change_set _ -> stmt
    | Stmt_break -> stmt
    | Stmt_continue -> stmt
    | Stmt_for (var, e, s) -> { stmt with snode = Stmt_for (var, e, transR s) }
    | _ -> assert false (* not reachable yet *)
  in
  { enode =
      RunCatch
        (transD
           (transR (remove_mut_change_set stmt))
           (Except ({ etype = TypeUnit; tloc = stmt.sloc }, Ground)))
  ; eloc = stmt.sloc
  }

and trans_expr e =
  ( (match e.enode with
     | Litteral l -> trans_litl l
     | Variable v -> Expr_Var (trans_var v)
     | Tuple tpl -> Expr_Constructor (Tuple, trans_expr_ls tpl)
     | CallUnary { op; arg = Some arg } -> Expr_Mon_Prim (op, trans_expr arg)
     | CallUnary { op; arg = None } -> make_unary_closure op e.eloc
     | CallBinary { op; args = first :: second :: _ } ->
       Expr_Bin_Prim (op, trans_expr first, trans_expr second)
     | CallBinary { op; args } -> make_binary_closure args op e.eloc
     | Construct construct ->
       Expr_Constructor
         (Cons_Named construct.constructor_ident, trans_expr_ls construct.to_group)
     | Binding bind ->
       Expr_Block
         (Blk
            ( [ Ins_Let (trans_var bind.var, trans_expr bind.init), bind.var.vloc ]
            , trans_expr bind.content
            , e.eloc ))
     | Match mat -> Expr_Match (trans_expr mat.to_match, trans_match_case_ls mat.cases)
     | Sequence expr_ls ->
       let last, rem = HelpersML.list_getlast_rem expr_ls in
       Expr_Block
         (Blk
            ( List.map
                (fun e -> Ins_Let (generate_variable e.eloc, trans_expr e), e.eloc)
                rem
            , trans_expr last
            , last.eloc ))
     | Call { func; arg } ->
       let openvar = generate_variable e.eloc in
       let returnvar = generate_variable e.eloc in
       let call =
         ( Expr_Method ((Expr_Var openvar, e.eloc), (Call, e.eloc), [ trans_expr arg ])
         , e.eloc )
       in
       Expr_Block
         (Blk
            ( [ Ins_Open (openvar, Exp, trans_expr func), func.eloc
              ; Ins_Force (returnvar, call), func.eloc
              ]
            , (Expr_Var returnvar, e.eloc)
            , e.eloc ))
     | Lambda { arg; body } ->
       Expr_Closure
         ( Exp
         , ( Expr_Get
               [ GetPatTag
                   ( (Call, e.eloc)
                   , [ trans_var arg ]
                   , (Expr_Thunk (trans_expr body), body.eloc)
                   , body.eloc )
               ]
           , e.eloc ) )
     | FunctionRec { var; arg; body } ->
       Expr_Closure
         ( Exp
         , ( Expr_Rec
               ( trans_var var
               , ( Expr_Get
                     [ GetPatTag
                         ( (Call, e.eloc)
                         , [ trans_var arg ]
                         , (Expr_Thunk (trans_expr body), body.eloc)
                         , body.eloc )
                     ]
                 , e.eloc ) )
           , e.eloc ) )
     | Do stmt ->
       let pre_expr, _ = trans_expr (trans_do stmt) in
       pre_expr
     | BindMonadic _ -> assert false
     | Return _ -> assert false
     | If _ -> assert false
     | Get -> assert false
     | Set _ -> assert false
     | RunState _ -> assert false
     | LiftState _ -> assert false
     | ThrowEx _ -> assert false
     | RunCatch _ -> assert false
     | LiftEx _ -> assert false
     | ForM _ -> assert false)
  , e.eloc )

and trans_match_case case =
  let conseq = trans_expr case.consequence in
  let conseq_loc = case.consequence.eloc in
  let ptt_loc = case.pattern.ploc in
  match case.pattern.pnode with
  | LitteralPattern litt ->
    (match litt with
     | Integer x -> MatchPatTag (Int_Litt x, [], conseq, conseq_loc)
     | Boolean x -> MatchPatTag (trans_boolean x, [], conseq, conseq_loc)
     | Unit -> MatchPatTag (Unit, [], conseq, conseq_loc))
  | TuplePattern _ -> MatchPatTag (Tuple, getPatternVariable case, conseq, conseq_loc)
  | ConstructorPattern ptt ->
    MatchPatTag
      (Cons_Named ptt.constructor_ident, getPatternVariable case, conseq, conseq_loc)
  | VarPattern x -> MatchPatVar ((x, ptt_loc), conseq, conseq_loc)
  | WildcardPattern -> MatchPatVar (generate_variable ptt_loc, conseq, conseq_loc)

and getPatternVariable case =
  let step pt =
    match pt.pnode with
    | VarPattern x -> x, pt.ploc
    | WildcardPattern -> generate_variable pt.ploc
    | _ ->
      HelpersML.err "DeepMatch Pattern Unhandled : Pattern Containig Non Variable" pt.ploc
  in
  match case.pattern.pnode with
  | TuplePattern ptt -> List.map step ptt
  | ConstructorPattern ptt -> List.map step ptt.content
  | _ -> HelpersML.err "DeepMatch Pattern Unhandled" case.pattern.ploc

and trans_match_case_ls ls = List.map trans_match_case ls
and trans_expr_ls ls = List.map trans_expr ls

let rec trans_type t =
  ( (match t.etype with
     | TypeMonadic _ -> assert false
     | TypeInt -> Typ_App ((Typ_Int, t.tloc), [])
     | TypeBool -> Typ_App ((Typ_Bool, t.tloc), [])
     | TypeUnit -> Typ_App ((Typ_Unit, t.tloc), [])
     | TypeTuple x -> Typ_App ((Typ_Tuple, t.tloc), trans_type_ls x)
     | TypeDefined defined -> Typ_Var (String.capitalize_ascii defined)
     | TypeVar vartype -> Typ_Var (String.capitalize_ascii vartype)
     | TypeConstructor x -> Typ_App (trans_type x.to_build, trans_type_ls x.parameters)
     | TypeLambda { arg; return_type } ->
       Typ_App
         ( (Typ_Closure Exp, t.tloc)
         , [ ( Typ_App
                 ( (Typ_Fun, t.tloc)
                 , ( Typ_App ((Typ_Thunk, return_type.tloc), [ trans_type return_type ])
                   , return_type.tloc )
                   :: [ trans_type arg ] )
             , t.tloc )
           ] ))
  , t.tloc )

and trans_type_ls ls = List.map trans_type ls

let rec trans_newconstructor_case case = case.constructor_ident, trans_type_ls case.c_of
and trans_newconstructor_case_ls ls = List.map trans_newconstructor_case ls

type temp =
  | NewTypeDef of program_item
  | NewGlobal of instruction

let trans_def def =
  let loc = def.dloc in
  match def.dnode with
  | TypeDef newtype ->
    NewTypeDef
      (Typ_Def
         ( String.capitalize_ascii newtype.basic_ident
         , List.map (fun elem -> String.capitalize_ascii elem, Pos) newtype.parameters
         , Def_Datatype (trans_newconstructor_case_ls newtype.constructors)
         , loc ))
  | VariableDef newglb ->
    NewGlobal (Ins_Let (trans_var newglb.var, trans_expr newglb.init), loc)
;;

let trans_prog_node (glbvarls, program_items, last_expr) node =
  match node with
  | Def d ->
    (match trans_def d with
     | NewTypeDef newtype -> glbvarls, newtype :: program_items, last_expr
     | NewGlobal newglb -> newglb :: glbvarls, program_items, last_expr)
  | Expr e ->
    let newVarName = HelpersML.generate_name () in
    let varloc = e.eloc in
    ( (Ins_Let ((newVarName, varloc), trans_expr e), varloc) :: glbvarls
    , program_items
    , (Expr_Var (newVarName, varloc), varloc) )
;;

let trans_prog p =
  let glbVar, progItemLs, last_expr =
    List.fold_left trans_prog_node ([], [], (Expr_Int 0, dummy_pos)) p
  in
  Prog (List.rev progItemLs @ [ Do (Blk (List.rev glbVar, last_expr, dummy_pos)) ])
;;
