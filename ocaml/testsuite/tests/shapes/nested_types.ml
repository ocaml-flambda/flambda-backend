(* TEST
 flags = "-dshape";
 expect;
*)

module M : sig

  exception Exn of { lbl_exn : int }
  type l = { lbl : int }
  type ext = ..
  type ext += Ext of { lbl_ext : int }
  type t = C of { lbl_cstr : int }
end = struct
  exception Exn of { lbl_exn : int }
  type l = { lbl : int }
  type ext = ..
  type ext += Ext of { lbl_ext : int }
  type t = C of { lbl_cstr : int }
end
[%%expect{|
{
 "M"[module] ->
   {<.35>
    "Exn"[extension constructor] -> {<.17>
                                     "lbl_exn"[label] -> <.16>;
                                     };
    "Ext"[extension constructor] -> {<.23>
                                     "lbl_ext"[label] -> <.22>;
                                     };
    "ext"[type] -> <.21>;
    "l"[type] -> {<.19>
                  "lbl"[label] -> <.20>;
                  };
    "t"[type] ->
      {<.25>
       "C"[constructor] -> {<.27>
                            "lbl_cstr"[label] -> <.26>;
                            };
       };
    };
 }
module M :
  sig
    exception Exn of { lbl_exn : int; }
    type l = { lbl : int; }
    type ext = ..
    type ext += Ext of { lbl_ext : int; }
    type t = C of { lbl_cstr : int; }
  end
|}]
