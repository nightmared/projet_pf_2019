open Decl

type ('shift, 'reset) res =
	| Get of (('shift -> ('shift, 'reset) res))
	| Yield of ((unit -> ('shift, 'reset) res) * 'shift)
	| Exit;;

module GreenThreads (M: sig type shift end) =
struct
	let p: (M.shift, 'reset) res Delimcc.prompt = Delimcc.new_prompt ()

	let rec scheduler tasks val_init = match tasks with
		| [] -> ()
		| proc::q -> (match Delimcc.push_prompt p proc with
			| Get old_proc -> scheduler ((fun () -> old_proc val_init)::q) val_init
			| Yield (old_proc, v) -> scheduler (q@[old_proc]) v
			| Exit -> scheduler q val_init);;

	let get () = Delimcc.shift p (fun k -> Get (k));;
	let yield v = Delimcc.shift p (fun k -> Yield (k, v));;
	let exit () = Delimcc.shift p (fun k -> Exit);;
end


module GreenThreadsBool = GreenThreads (struct type shift = terrain end);;

let sendmsg msg = begin
	for i = 1 to 10 do
		print_endline msg;
		let t = GreenThreadsBool.get () in
		let _ = List.iter (fun (Brique ((x, y), _, _)) -> print_endline ((string_of_int x)^" "^(string_of_int y))) t in 
		GreenThreadsBool.yield (Ui.gen_brique 1 4 640 480::t);
	done;
	end;;

GreenThreadsBool.scheduler [(fun t -> sendmsg "ping"; GreenThreadsBool.exit ()); (fun t -> sendmsg "pong"; GreenThreadsBool.exit ())] [];
