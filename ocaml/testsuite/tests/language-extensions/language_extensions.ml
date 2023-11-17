(* TEST
   include ocamlcommon
   flags = "-I ${ocamlsrcdir}/parsing"
*)

(* Change these two variables to change which extension is being tested *)
let extension            = Language_extension.Comprehensions
let extension_expression = "[x for x = 1 to 10]"

let extension_name = Language_extension.to_string extension
let extension_parsed_expression =
  Parse.expression (Lexing.from_string extension_expression)
  (* Currently, parsing always succeeds and we only fail during typechecking *)

let report ~name ~text =
  Printf.printf "# %s [%s %s]:\n%s\n\n"
    name
    extension_name
    (if Language_extension.is_enabled extension then "enabled" else "disabled")
    text

let typecheck_with_extension ?(full_name = false) name =
  let success =
    match Typecore.type_expression (Lazy.force Env.initial)
            extension_parsed_expression
    with
    | _ -> true
    | exception (Jane_syntax_parsing.Error.Error _) -> false
  in
  report
    ~name:(if full_name
           then name
           else "\"" ^ extension_name ^ "\" extension " ^ name)
    ~text:(if success
           then "Successfully typechecked \"" ^ extension_expression ^ "\""
           else "<extension error>")
;;

let should_succeed name what f =
  report ~name ~text:(match f () with
    | () ->
        "Succeeded at " ^ what
    | exception Arg.Bad msg ->
        "FAILED at " ^ what ^ ", with the following error\n:" ^ msg)
;;

let should_fail name f =
  report ~name ~text:(match f () with
    | () -> "<succeeded INCORRECTLY>"
    | exception Arg.Bad msg -> "Failed as expected: " ^ msg)
;;

let try_disallowing_extensions name =
  should_succeed
    name
    "disallowing all extensions"
    Language_extension.disallow_extensions
;;

type goal = Fail | Succeed

let with_goal goal ~name ~what test = match goal with
  | Fail    -> should_fail    name      test
  | Succeed -> should_succeed name what test

let when_disallowed goal f_str f =
  let can_or_can't = match goal with
    | Fail    -> "can't"
    | Succeed -> "can"
  in
  let f_code = "[" ^ f_str ^ "]" in
  with_goal goal
    ~name:(can_or_can't ^ " call " ^ f_code ^ " when extensions are disallowed")
    ~what:("redundantly calling " ^ f_code)
    (fun () -> f extension)
;;

let lift_with with_fn extension = with_fn extension Fun.id;;

(* Test the ground state *)

typecheck_with_extension "in its default state";

(* Disable all extensions for testing *)

Language_extension.disable_all ();
typecheck_with_extension ~full_name:true "no extensions enabled";

(* Test globally toggling a language extension *)

Language_extension.enable extension ();
typecheck_with_extension "enabled";

Language_extension.enable extension ();
typecheck_with_extension "still enabled";

Language_extension.disable extension;
typecheck_with_extension "disabled";

Language_extension.disable extension;
typecheck_with_extension "still disabled";

Language_extension.set extension ~enabled:true;
typecheck_with_extension "enabled via [set]";

Language_extension.enable extension ();
typecheck_with_extension "still enabled, via [set] and [enable]";

Language_extension.set extension ~enabled:false;
typecheck_with_extension "disabled via [set]";

Language_extension.disable extension;
typecheck_with_extension "still disabled, via [set] and [disable]";

(* Test locally toggling a language extension *)

(* Globally disable the language extension (idempotent, given the prior tests,
   but it's more robust to do this explicitly) *)
Language_extension.disable extension;

Language_extension.with_enabled extension () (fun () ->
  typecheck_with_extension "enabled locally and disabled globally");

Language_extension.with_disabled extension (fun () ->
  typecheck_with_extension "disabled locally and globally");

Language_extension.with_set extension ~enabled:true (fun () ->
  typecheck_with_extension
    "enabled locally via [with_set] and disabled globally");

Language_extension.with_set extension ~enabled:false (fun () ->
  typecheck_with_extension "disabled locally via [with_set] and also globally");

(* Globally enable the language extension *)
Language_extension.enable extension ();

Language_extension.with_disabled extension (fun () ->
  typecheck_with_extension "disabled locally and enabled globally");

Language_extension.with_enabled extension () (fun () ->
  typecheck_with_extension "enabled locally and globally");

Language_extension.with_set extension ~enabled:false (fun () ->
  typecheck_with_extension
    "disabled locally via [with_set] and enabled globally");

Language_extension.with_set extension ~enabled:true (fun () ->
  typecheck_with_extension "disabled locally via [with_set] and also globally");

(* Test behavior of layouts extensions *)
Language_extension.(enable Layouts Beta);;
Language_extension.(enable Layouts Alpha);;
report ~name:"Enable two layouts"
  ~text:(if Language_extension.is_at_least Layouts Alpha
              && Language_extension.is_at_least Layouts Beta
              && Language_extension.is_at_least Layouts Stable
         then "Succeeded"
         else "Failed");;

Language_extension.disable Layouts;;
report ~name:"Disable layouts"
  ~text:(if Language_extension.is_at_least Layouts Alpha
            || Language_extension.is_at_least Layouts Beta
            || Language_extension.is_at_least Layouts Stable
         then "Failed"
         else "Succeeded");;

Language_extension.(enable Layouts Alpha);;
Language_extension.(enable Layouts Beta);;
report ~name:"Enable two layouts, in reverse order"
  ~text:(if Language_extension.is_at_least Layouts Alpha
              && Language_extension.is_at_least Layouts Beta
              && Language_extension.is_at_least Layouts Stable
         then "Succeeded"
         else "Failed");;

(* Test disallowing extensions *)

try_disallowing_extensions
  "can disallow extensions while extensions are enabled";

try_disallowing_extensions
  "can disallow extensions while extensions are already disallowed";

(* Test that disallowing extensions prevents other functions from working *)

when_disallowed Fail "set ~enabled:true"
  (Language_extension.set ~enabled:true);

when_disallowed Succeed "set ~enabled:false"
  (Language_extension.set ~enabled:false);

when_disallowed Fail "enable"
  (fun x -> Language_extension.enable x ());

when_disallowed Succeed "disable"
  Language_extension.disable;

when_disallowed Fail "with_set ~enabled:true"
  (Language_extension.with_set ~enabled:true |> lift_with);

when_disallowed Succeed "with_set ~enabled:false"
  (Language_extension.with_set ~enabled:false |> lift_with);

when_disallowed Fail "with_enabled"
  ((fun x -> Language_extension.with_enabled x ()) |> lift_with);

when_disallowed Succeed "with_disabled"
  (Language_extension.with_disabled |> lift_with);

(* Test explicitly (rather than just via [report]) that [is_enabled] returns
   [false] now that we've disallowed all extensions *)
report
  ~name:"[is_enabled] returns [false] when extensions are disallowed"
  ~text:("\"" ^ extension_name ^ "\" is " ^
         if Language_extension.is_enabled extension
         then "INCORRECTLY enabled"
         else "correctly disabled");

(* Test that language extensions round-trip via string *)
List.iter
  (fun (Language_extension.Exist.Pack x) ->
     let str = Language_extension.to_string x in
     let Pack x' =
       match Language_extension.of_string str with
       | None -> failwith str
       | Some x' -> x'
     in
     if not (Language_extension.equal x x') then failwith str)
  Language_extension.Exist.all;
