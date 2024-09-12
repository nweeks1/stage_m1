type vartype = string

type pre_etype =
  | TypeInt
  | TypeBool
  | TypeUnit
  | TypeTuple of etype list
  | TypeLambda of
      { arg : etype
      ; return_type : etype
      }
  | TypeVar of vartype
  | TypeDefined of vartype
  | TypeConstructor of
      { to_build : etype
      ; parameters : etype list
      }
  | TypeMonadic of effect * etype

and effect =
  | Ground
  | State of etype * effect
  | Except of etype * effect

and etype =
  { etype : pre_etype
  ; tloc : Autobill.Misc.position
  }

type variable =
  { basic_ident : string
  ; vloc : Autobill.Misc.position
  }

and prog = prog_node list

and prog_node =
  | Def of def
  | Expr of expr

and def =
  { dnode : pre_def
  ; dloc : Autobill.Misc.position
  }

and pre_def =
  | VariableDef of
      { var : variable
      ; init : expr
      }
  | TypeDef of
      { basic_ident : string
      ; parameters : vartype list
      ; constructors : newconstructor_case list
      }

and newconstructor_case =
  { constructor_ident : string
  ; c_of : etype list
  ; loc : Autobill.Misc.position
  }

and litteral =
  | Integer of int
  | Boolean of bool
  | Unit

and expr =
  { enode : pre_expr
  ; eloc : Autobill.Misc.position
  }

and pre_expr =
  | Litteral of litteral
  | Variable of variable
  | CallUnary of
      { op : Autobill.Lcbpv.prim_mon_op
      ; arg : expr option
      }
  | CallBinary of
      { op : Autobill.Lcbpv.prim_bin_op
      ; args : expr list
      }
  | Call of
      { func : expr
      ; arg : expr
      }
  | Sequence of expr list
  | Binding of
      { var : variable
      ; init : expr
      ; content : expr
      }
  | Lambda of
      { arg : variable
      ; body : expr
      }
  | Tuple of expr list
  | Construct of
      { constructor_ident : string
      ; to_group : expr list
      }
  | FunctionRec of
      { var : variable
      ; arg : variable
      ; body : expr
      }
  | Match of
      { to_match : expr
      ; cases : match_case list
      }
  | Do of statement
  | BindMonadic of expr * expr * effect
  | Return of expr * effect
  | If of expr * expr * expr
  | Get
  | Set of expr
  | RunState of expr * expr
  | LiftState of expr * effect
  | ThrowEx of expr
  | LiftEx of expr * effect
  | RunCatch of expr
  | ForM of expr * expr

and match_case =
  { pattern : pattern
  ; consequence : expr
  ; cloc : Autobill.Misc.position
  }

and pattern =
  { pnode : pre_pattern
  ; ploc : Autobill.Misc.position
  }

and pre_pattern =
  | LitteralPattern of litteral
  | VarPattern of string
  | WildcardPattern
  | TuplePattern of pattern list (* Pas Profond *)
  | ConstructorPattern of
      { constructor_ident : string
      ; content : pattern list (* Pas Profond *)
      }

and statement =
  { snode : pre_statement
  ; sloc : Autobill.Misc.position
  }

and pre_statement =
  | Stmt_pure of expr
  | Stmt_return of expr
  | Stmt_let of variable * statement * statement
  | Stmt_if of expr * statement * statement
  | Stmt_mut of variable * expr * statement
  | Stmt_mut_change of variable * expr
  | Stmt_mut_change_set of variable * expr * statement
  | Stmt_get
  | Stmt_set of expr
  | Stmt_early_return of expr
  | Stmt_lift_st of expr
  | Stmt_throw of expr
  | Stmt_lift of expr
  | Stmt_break
  | Stmt_continue
  | Stmt_for of variable * expr * statement
