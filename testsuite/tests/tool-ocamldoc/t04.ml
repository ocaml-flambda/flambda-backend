(* TEST
<<<<<<< HEAD
 plugins = "odoc_test.ml";
 flags = "-I ${ocamlsrcdir}/ocamldoc -I ${ocamlsrcdir}/typing";
 ocamldoc;
||||||| 121bedcfd2
   plugins="odoc_test.ml"
   * ocamldoc
    flags="-I ${ocamlsrcdir}/ocamldoc"
=======
 plugins = "odoc_test.ml";
 flags = "-I ${ocamlsrcdir}/ocamldoc";
 ocamldoc;
>>>>>>> 5.2.0
*)

(** Testing display of inline record.

   @test_types_display
 *)


module A = struct
  type a = A of {lbl:int}

end

module type E = sig
  exception E of {lbl:int}

end


module E_bis= struct
  exception E of {lbl:int}
end
