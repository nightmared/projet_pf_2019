open Decl

type ('shift, 'reset) res =
	| Get of (('shift -> ('shift, 'reset) res))
	| Send of ((unit -> ('shift, 'reset) res) * 'shift)
	| Yield of ((unit -> ('shift, 'reset) res))
	| Exit;;

module GreenThreads (M: sig type shift end) =
struct
	let p: (M.shift, 'reset) res Delimcc.prompt = Delimcc.new_prompt ()

	let rec scheduler tasks val_init = match tasks with
		| [] -> ()
		| proc::q -> (match Delimcc.push_prompt p proc with
			(* Get et Send sont immédiatement ré-entrants et démarquent la session critique,
			 * bien qu'il n'y a pas d'exécution //, uniqument concurrente (pour l'instant) *)
			| Get old_proc -> scheduler ((fun () -> old_proc val_init)::q) val_init
			| Send (old_proc, v) -> scheduler (old_proc::q) v
			(* Yield donne la main au processus suivant de la chaîne 'tasks' *)
			| Yield old_proc -> scheduler (q@[old_proc]) val_init
			| Exit -> scheduler q val_init);;

	let get () = Delimcc.shift p (fun k -> Get (k));;
	let send v = Delimcc.shift p (fun k -> Send (k, v));;
	let yield () = Delimcc.shift p (fun k -> Yield (k));;
	let exit () = Delimcc.shift p (fun k -> Exit);;
end


(*
* module GreenThreadsBool = GreenThreads (struct type shift = terrain end);;
*
* let sendmsg msg = begin
* 	for i = 1 to 10 do
* 		let t = GreenThreadsBool.get () in
* 		let _ = List.iter (fun (Brique ((x, y), _, _)) -> print_endline (msg^" "^(string_of_int x)^" "^(string_of_int y))) t in
* 		GreenThreadsBool.send (Ui.gen_brique 1 4 640 480::t);
* 		GreenThreadsBool.yield ();
* 	done;
* 	end;;
*
* GreenThreadsBool.scheduler [(fun () -> sendmsg "ping"; GreenThreadsBool.exit ()); (fun () -> sendmsg "pong"; GreenThreadsBool.exit ())] [];
*)
