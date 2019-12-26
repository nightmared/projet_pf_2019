open Decl

type ('shift, 'reset) res =
  | New of ((bool -> ('shift, 'reset) res) * bool * bool)
  | Yield of (('shift -> ('shift, 'reset) res) * 'shift)
  | Exit;;

module GreenThreads (M: sig type shift end) =
  struct
    type shift = M.shift

    let p: (shift, 'reset) res Delimcc.prompt = Delimcc.new_prompt ()

    let scheduler proc_init val_init =
      let rec loop queue = match queue with
        | [] -> ()
        | proc::q -> (match Delimcc.push_prompt p proc with
          | New (proc, v, other_v) -> loop ((fun () -> proc v)::q@[fun () -> proc other_v])
          | Yield (old_proc, v) -> loop (q@[fun () -> old_proc v])
          | Exit -> loop q)
        in loop [fun () -> proc_init val_init];;

    let yield v = Delimcc.shift p (fun k -> Yield (k, v));;
    let fork () = Delimcc.shift p (fun k -> New (k, true, false));;
    let exit () = Delimcc.shift p (fun k -> Exit);;
  end


module GreenThreadsBool = GreenThreads (struct type shift = terrain end);;

let sendmsg msg = begin
    for i = 1 to 10 do
      print_endline msg;
      GreenThreadsBool.yield ();
    done;
  end;;

GreenThreadsBool.scheduler (fun _ -> if GreenThreadsBool.fork () then sendmsg "ping" else sendmsg "pong!"; GreenThreadsBool.exit ()) true;
