module Key = Functoria_key

let warn_error =
  let doc = "Enable -warn-error when compiling OCaml sources." in
  let doc = Key.Arg.info ~docv:"BOOL" ~doc ["warn-error"] in
  let key = Key.Arg.(opt ~stage:`Configure bool false doc) in
  Key.create "warn_error" key

let vote =
  let doc = "Vote." in
  let doc = Key.Arg.info ~docv:"VOTE" ~doc ["vote"] in
  let key = Key.Arg.(opt ~stage:`Configure string "cat" doc) in
  Key.create "vote" key

module C = struct
  let prelude = "open Pervasives"
  let name = "test_app"
  let version = "1.0"
  let create jobs = Functoria.impl @@ object
      inherit Functoria.base_configurable
      method ty = Functoria.job
      method name = "test_app"
      method module_name = "Test_app"
      method! keys = [
        Functoria_key.(abstract vote);
        Functoria_key.(abstract warn_error);
      ]
      method! packages = Key.pure [Functoria.package "lwt"]
      method! deps = List.map Functoria.abstract jobs
    end
end

include Functoria_app.Make(C)
