open Test_app

let main = Functoria.(foreign "Test" job)

let () = register "noop" [main]
