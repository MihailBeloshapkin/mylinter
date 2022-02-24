open Base
open Caml.Format
open Zanuda_core
open Utils

let lint_id = "ifThenElseCount"
let lint_source = LINT.FPCourse

let describe_itself () =
  describe_as_clippy_json
    lint_id
    ~impl:LINT.Untyped
    ~docs:
      {|
### What it does
Calculates number of different kinds of expressions.
|}
;;

open Parsetree
open Ast_iterator

type input = Ast_iterator.iterator

(** Get statostics from the Ocaml AST*)
let run _ fallback =
  { fallback with
    expr =
      (fun self currentExpr ->
        let loc = currentExpr.pexp_loc in
        let filename = loc.Location.loc_start.Lexing.pos_fname in
      
        match currentExpr.pexp_desc with
        | Pexp_let (_, vb, ex) -> 
        self.expr self ex;
        List.iter ~f: (fun bind -> self.expr self bind.pvb_expr) vb;
        StatisticsCollector.add LetExp
        | Pexp_ifthenelse (cond, th, None) -> 
        self.expr self cond;                                      
        self.expr self th;
        StatisticsCollector.add IfThenElse
        | Pexp_match (ex, li) -> 
        StatisticsCollector.add Match;
        self.expr self ex;
        List.iter ~f: (fun c -> self.expr self c.pc_rhs) li
        | _ -> fallback.expr self currentExpr)
  }
;;