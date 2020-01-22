type 'shift res =
  | Get of (('shift -> 'shift res))
  | Send of ((unit -> 'shift res) * 'shift)
  | Yield of ((unit -> 'shift res))
  | StopScheduler
  | Exit

(* fréquence d'exécution de l'objet *)
type frequency = float option

module GreenThreads (M: sig type shift end) = struct
  let p: M.shift res Delimcc.prompt = Delimcc.new_prompt ()

  let get () = Delimcc.shift p (fun k -> Get (k))
  let send v = Delimcc.shift p (fun k -> Send (k, v))
  let yield () = Delimcc.shift p (fun k -> Yield (k))
  let exit () = Delimcc.shift p (fun _ -> Exit)
  let stop_scheduler () = Delimcc.shift p (fun _ -> StopScheduler)
  (* Exécute une nouvelle méthode qui remplace la fonction qui vient de "yield" *)
  let continue k = Delimcc.shift p (fun _ -> Yield (k))

  let gen_waiter proc freq = match freq with
   | None -> proc
   | Some f -> (fun () ->
     let start_time = Unix.gettimeofday () in begin
       while Unix.gettimeofday () -. start_time < 1./. f do
         yield ();
       done;
       proc ()
     end)

   let rec gen_looper proc = fun () ->
     (let _ = proc () in continue (gen_looper proc))

  (* Ordonance une liste de tâches avec une fréquence de rafraichissement indivuelle *)
  let rec scheduler tasks val_init = match tasks with
    | [] -> ()
    | proc::q -> (match Delimcc.push_prompt p proc with
      (* Get et Send sont immédiatement ré-entrants et démarquent la session critique,
       * bien qu'il n'y a pas d'exécution //, uniqument concurrente (pour l'instant) *)
      | Get old_proc -> scheduler ((fun () -> old_proc val_init)::q) val_init
      | Send (old_proc, v) -> scheduler (old_proc::q) v
      (* Yield donne la main au processus suivant de la chaîne 'tasks' *)
      | Yield old_proc -> scheduler (q@[old_proc]) val_init
      | StopScheduler -> ()
      | Exit -> scheduler q val_init)

  (* génère une liste de processus qui sont exécutés en boucle et à une certain fréquence *)
  let rec gen_scheduler_list l = match l with
    | [] -> []
    | (internal_proc, freq)::q -> (gen_looper (gen_waiter internal_proc freq))::(gen_scheduler_list q)

end
