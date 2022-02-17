(* función para calcular tiempos de ejecución: *)

let crono f x y =
    let t = Sys.time () in
    let _ = f x y in
    Sys.time () -. t
;;
