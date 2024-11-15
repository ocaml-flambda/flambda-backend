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
   {<.55>
    "Exn"[extension constructor] -> {<.3>
                                     "lbl_exn"[label] -> <.2>;
                                     };
    "Ext"[extension constructor] -> {<.17>
                                     "lbl_ext"[label] -> <.16>;
                                     };
    "ext"[type] -> <.15>;
    "l"[type] -> {<.5>
                  "lbl"[label] -> <.6>;
                  };
    "t"[type] ->
      {<.19>
       "C"[constructor] -> {<.21>
                            "lbl_cstr"[label] -> <.20>;
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
