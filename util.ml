let read_file (file_name : string) : char list =
        let ic = open_in file_name
        in
        let rec read_chars ic =
            try
                let next_char = input_char ic
                in
                next_char :: read_chars ic
            with
                _ -> []
        in
        read_chars ic

let implode (cs : char list) : string =
        String.concat "" (List.map (String.make 1) cs)

let explode (s : string) : char list =
        let l = String.length s
        in
        let rec f i =
                if i = l then [] else s.[i] :: f (i+1)
        in
        f 0

let split p lst : char list list =
	let accum = ([], [])
        in
        let aux (sublists, current) elem =
                if p elem then (List.rev current :: sublists, [])
                else (sublists, elem :: current)
        in
        let (lsts, curr) = List.fold_left aux accum lst
        in
        List.rev (List.rev curr :: lsts)
