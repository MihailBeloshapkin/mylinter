open Base

module Options = struct
  type t =
    { mutable outfile : string option
    ; mutable outgolint : string option
    ; mutable out_rdjsonl : string option
          (* Spec: https://github.com/reviewdog/reviewdog/tree/master/proto/rdf#rdjson *)
    ; mutable infile : string
          (* Below options to manage file paths. Not sure are they really required *)
    ; mutable workspace : string option
    ; mutable prefix_to_cut : string option
    ; mutable prefix_to_add : string option
    }

  let opts =
    { outfile = None
    ; outgolint = None
    ; out_rdjsonl = None
    ; infile = ""
    ; workspace = None
    ; prefix_to_cut = None
    ; prefix_to_add = None
    }
  ;;

  let set_out_file s = opts.outfile <- Some s
  let set_out_golint s = opts.outgolint <- Some s
  let set_out_rdjsonl s = opts.out_rdjsonl <- Some s
  let set_workspace s = opts.workspace <- Some s
  let set_prefix_to_cut s = opts.prefix_to_cut <- Some s
  let set_prefix_to_add s = opts.prefix_to_add <- Some s
  let prefix_to_cut () = opts.prefix_to_cut
  let prefix_to_add () = opts.prefix_to_add
  let outfile () = opts.outfile
  let out_golint () = opts.outgolint
  let out_rdjsonl () = opts.out_rdjsonl
  let infile () = opts.infile
  let set_in_file s = opts.infile <- s
end

let recover_filepath s =
  let filepath = s in
  let filepath =
    match Options.prefix_to_cut () with
    | Some s -> String.drop_prefix filepath (String.length s)
    | None -> filepath
  in
  let filepath =
    match Options.prefix_to_add () with
    | Some s -> Format.sprintf "%s%s" s filepath
    | None -> filepath
  in
  filepath
;;