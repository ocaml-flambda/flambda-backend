let read_unit_info file : Instantiator.unit_info =
  let unit_info, _crc = Compilenv.read_unit_info file in
  let { Cmx_format.ui_unit; ui_arg_descr; ui_format; _ } = unit_info in
  { Instantiator.ui_unit; ui_arg_descr; ui_format; }

let instantiate unix ~src ~args targetcmx ~flambda2 =
  Instantiator.instantiate ~src ~args targetcmx
    ~expected_extension:".cmx"
    ~read_unit_info
    ~compile:(Optcompile.instance unix ~flambda2 ~keep_symbol_tables:false)
