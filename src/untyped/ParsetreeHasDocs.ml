open Base
open Caml.Format
open Zanuda_core
open Utils

let lint_id = "no_docs_parsetree"

let describe_itself () =
  describe_as_clippy_json
    lint_id
    ~docs:
      {|
### What it does
It checks that file `Parsetree.mli` has documentation comments for all constructors. Usually files like this are used to describe abstract syntax tree (AST) of a language. In this case it's recommended to annotate every constructor with a documentation about meaning of the constructors, for example, which real syntax if supposed to be parsed to this part of AST.

As example of this kind of documentation you can consult [OCaml 4.13 parse tree](https://github.com/ocaml/ocaml/blob/4.13/parsing/parsetree.mli#L282)
  |}
;;

open Parsetree
open Ast_iterator

type input = Ast_iterator.iterator

let is_doc_attribute attr = String.equal "ocaml.doc" attr.attr_name.txt
let msg ppf name = fprintf ppf "Constructor '%s' has no documentation attribute" name

let report ~filename cname ~loc =
  let module M = struct
    let txt ppf () = Report.txt ~loc ~filename ppf msg cname

    let rdjsonl ppf () =
      Report.rdjsonl
        ~loc
        ppf
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        msg
        cname
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

let run { Compile_common.source_file; _ } fallback =
  if Config.verbose () then printfn "Trying lint '%s' on file '%s'" lint_id source_file;
  if String.is_suffix ~suffix:"arsetree.mli" source_file
     || String.is_suffix ~suffix:"ast.mli" source_file
  then
    { fallback with
      constructor_declaration =
        (fun self cd ->
          let loc = cd.pcd_loc in
          let filename = loc.Location.loc_start.Lexing.pos_fname in
          if not (List.exists cd.pcd_attributes ~f:is_doc_attribute)
          then CollectedLints.add ~loc (report ~filename cd.pcd_name.txt ~loc);
          fallback.constructor_declaration self cd)
    }
  else fallback
;;
