open Eve_api

let store = Ocsipersist.open_store "eve_standing"

let current_tz_to_string () = let open CalendarLib.Time_Zone in
  match current () with
    | Local -> Printf.sprintf "UTC%i" (to_gmt ())
    | UTC -> "UTC"
    | UTC_Plus i -> Printf.sprintf "UTC%i" i


let get_update_s c =
  let date = c.Response.cachedUntil in
  let date_s = CalendarLib.Printer.Calendar.to_string date in
  Printf.sprintf "Next update will be the %s %s" date_s (current_tz_to_string ())
