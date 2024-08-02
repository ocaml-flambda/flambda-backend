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
   {<.38>
    "Exn"[extension constructor] -> {<.20>
                                     "lbl_exn"[label] -> <.19>;
                                     };
    "Ext"[extension constructor] -> {<.26>
                                     "lbl_ext"[label] -> <.25>;
                                     };
    "ext"[type] -> <.24>;
    "l"[type] -> {<.22>
                  "lbl"[label] -> <.23>;
                  };
    "t"[type] ->
      {<.28>
       "C"[constructor] -> {<.30>
                            "lbl_cstr"[label] -> <.29>;
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
