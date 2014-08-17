open Eliom_lib
open Eve_api

let drup = Eve_api.charkey
    ~keyId:3613130
    ~vCode:"jLJyRWbEAGaya60H8l3YJiOzxmUpNbOX7HQgBohP9ZGf2TqW7m5N9wimMKVmFdLs"
    ~charId:90872953

let call () =
  Ocsigen_messages.warning "Updating Corporation standings..." ;
  lwt x = apply_api ~https:false tq Character.contactList drup () in
  Lwt.return x


let table =
  Ocsipersist.make_persistent_lazy_lwt
    ~store:Common.store
    ~name:"corporation_standing"
    ~default:(fun () ->
      Ocsigen_messages.warning "Init Corporation standings." ;
      call ()
      >|= Response.cast)

let get () =
  table >>= Ocsipersist.get

let set x =
  table >>= (fun t -> Ocsipersist.set t x)

let update r =
  let open Response in
  lwt () = set r in
  Ocsigen_messages.warning "Corporation standings updated!" ;
  Lwt.return `KeepGoing

let error (x,s) =
  let err_result = handle_error ~log:Ocsigen_messages.errlog (x,s) in
  match err_result with
    | `Delay _ | `KeepGoing | `Retry as r -> r
    | r ->
        ( Ocsigen_messages.errlog "Stop updating standings." ; r )


let to_seconds d =
  CalendarLib.Calendar.(Time.Period.to_seconds (Period.to_time d))

let thread : _ Lwt.t =
  let error x = Lwt.wrap (fun () -> error x) in
  let log d = Eliom_lib.debug "Next update will be in %i s" (to_seconds d) in
  try_lwt
    periodic_update ~log ~call ~error ~update ()
  with e ->
    Ocsigen_messages.errlog (Printexc.get_backtrace ());
    Ocsigen_messages.unexpected_exception e "corpo" ; raise e
