(*
 * Deacronymification in OCaml: 
 * Combining "why would you ever want to do that" with "why would you *ever* want to do that" since 2/24/18.
 * Example usage: 
 * # deacronym "genesis_words.txt" "H.E.L.L.O.";;
 * - : bytes = "Him Even Land Left Over"  
 *)

#use "util.ml"

let wordlist file_name =
        let chs = read_file file_name
        in
        let words_intermediate =
                let sublists = split (fun c -> c = ' ' || c = '\n') chs
                in
                List.filter (fun lst -> lst != []) sublists
        in
        List.filter (fun w -> String.length w > 3) (List.map implode words_intermediate)

let wordmap wordlist = List.map (fun c -> (c, (List.filter (fun s -> s.[0] = c) wordlist))) ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z']

let deacronymifier wordmap s =
        let is_alpha = function
                | ('A'..'Z')|('a'..'z') -> true
                | _ -> false
        in
        let slots = List.filter is_alpha (explode s)
        in
        let letters = List.map Char.lowercase slots
        in
        let get_random_word c =
                let get_second (x, y) = y
               	in
                let alphabet_index chr = Char.code chr - Char.code 'a'
                in
                let lst = get_second (List.nth wordmap (alphabet_index c))
                in
                let result = 
			if List.length lst > 0 
				then List.nth lst (Random.int (List.length lst)) 
			else 
				((String.make 1 c) ^ "oodle")                
		in
                let no_punctuation_result = List.filter is_alpha (explode result)
                in
                implode ((Char.uppercase (List.hd no_punctuation_result)) :: List.tl no_punctuation_result)
        in
        let results = List.map get_random_word letters
        in
        String.concat " " results

let deacronym file_name = deacronymifier (wordmap (wordlist file_name))
