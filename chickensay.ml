(* Inspired by cowsay, but way less good. *)

#use "util.ml"

let format str len =
        let sublists = split (fun c -> c = ' ' || c = '\n' || c = '\t' || c = '\r') (explode str)
        in
        let trimmed_sublists = List.filter (fun lst -> lst != []) sublists
        in
        let words = List.map String.trim (List.map implode trimmed_sublists)
        in
        let f accum word =
                match accum, word with
                | h :: t, w when ((String.length h) + (String.length w)) >= len -> w :: accum
                | h :: t, w when h = "" -> w :: t
                | h :: t, w -> ( h ^ " " ^ w ) :: t
                | _, w -> [w]
        in
        let lines = List.rev (List.fold_left f [""] words)
        in
        let border = if List.length lines = 1 then "<>" else "||"
        in
        let extra_space line = if List.length lines = 1 then "" else String.make (len - String.length line) ' '
        in
        let lines_with_borders = List.map (fun line -> (String.make 1 border.[0]) ^ " " ^ line ^ (extra_space line) ^ " " ^ (String.make 1 border.[1])) lines
        in
        String.concat "\n" (List.filter (fun s -> s <> "") lines_with_borders)

let get_chickensay msg len =
        let replicate lst times =
                let rec cons_mult n item lst =
                        if n = 0 then lst else (item :: cons_mult (n - 1) item lst)
                in
                let rec f accum = function
                        | [] -> accum
                        | h :: t -> cons_mult times h (if t = [] then accum else f accum t)
                in
                f [] lst
        in
        let border = implode (replicate ['-'] ((if String.length msg < len then String.length msg else len) + 2))
        in
        border ^ "\n" ^ (format msg len) ^ "\n" ^ border ^ "\n" ^ (implode (read_file "the_chicken.txt"))

let chickensay msg =
        print_endline ("\n\n\n" ^ get_chickensay msg 40)
