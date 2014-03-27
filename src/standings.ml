open Eliom_lib
open Eliom_content
open Html5.F

open Eve_api

let main_service =
  Eliom_service.Http.service ~path:[] ~get_params:Eliom_parameter.unit ()

let get_image size typeid x =
  if typeid = 2 then
    Lwt.return (Image.corp size x)
  else if typeid = 16159 then
    Lwt.return (Image.ally size x)
  else
    Lwt.return (Image.char size x)

let color standing =
       if standing > 5   then "excelent"
  else if standing > 0   then "good"
  else if standing = 0   then "neutral"
  else if standing >= -5 then "bad"
  else                        "terrible"

let format_contact head standings =
  let format_line (c,standing,cTypeId) =
    lwt src = get_image `T32 cTypeId c.id in
    let state = color standing in
    Lwt.return (
      tr ~a:[a_class [state]] [
        td ~a:[a_class ["icon"]] [img ~src ~alt:c.name ()] ;
        td [pcdata c.name] ;
        td ~a:[a_class ["standing"]] [pcdata (string_of_int standing)] ]
    )
  in
  let thead =
    thead [ tr [ th ~a:[a_colspan 3] [pcdata head] ]]
  in
  lwt standings = Lwt_list.map_p format_line standings in
  let standings_html = match standings  with
    | [] -> div [pcdata "No standing"]
    | h::l ->
        tablex ~a:[a_class [head]] ~thead [tbody (h::l)]
  in Lwt.return standings_html

let () =
  Eliom_registration.Html5.register
    ~service:main_service
    (fun () () ->
       lwt contacts = Qstanding.get () in
       let (chan, corp, ally) = contacts.Response.data in
       lwt corp = format_contact "Corporation" corp in
       lwt ally = format_contact "Alliance" ally in
       Lwt.return
         (Eliom_tools.F.html
            ~title:"standings"
            ~css:[["standings.css"]]
            (body [
                div ~a:[a_class ["wrap"]] [
                  h1 [pcdata "My wonderfull standings"];
                  div ~a:[a_class ["standings"]] [corp ; ally] ;
                  div ~a:[a_class ["push"]] [] ;
                ] ;
                div ~a:[a_class ["update"]] [pcdata (Common.get_update_s contacts)]
               ])
         ))
