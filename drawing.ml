open Graphics
open Plateau
open Decl

let draw_rect (x, y) (w, h) = draw_rect x y w h
let fill_rect (x,y) (w, h) = fill_rect x y w h
let moveto (x,y) = moveto x y;;

let draw_string_centered text = 
  let w,h = text_size text in
  let x, y = current_point () in 
  moveto (x - w/2, y- h/2);
  draw_string text

let dessiner_terrain (liste_blocs : terrain) =
  List.iter (fun (brique : brique) ->
    set_color brique.properties.color;
    fill_rect (int_of_float2 brique.position) (int_of_float2 (brique_width,brique_height));
    moveto (brique |> brique_to_aabb |> centre_rectangle |> int_of_float2);
    (match brique.properties.bonus with 
    | None -> ()
    | Some OneMoreLife -> 
      begin 
        set_color black;
        set_font "-*-fixed-medium-r-semicondensed--15-*-*-*-*-*-iso8859-1";
        draw_string_centered "+1";
      end
    );
    set_color foreground;
    draw_rect (int_of_float2 brique.position) (int_of_float2 (brique_width,brique_height));
  ) liste_blocs;;

let dessiner_balle (balle: balle) =
  let (x, y) = int_of_float2 (balle.pos)
  in draw_circle x y (int_of_float balle_radius);;

let dessiner_raquette (raquette: raquette) = 
  draw_rect (int_of_float2 raquette.position) (int_of_float2 (raquette_width, raquette_height));;

let dessiner_score score (w, h) = 
  moveto ((int_of_float w)/50,(int_of_float h)/50);
  set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
  draw_string "Score : ";
  draw_string (string_of_int score);;

let dessiner_nb_vies nb_vie (w, h) = 
  moveto ((int_of_float w) - 150, (int_of_float h)/50);
  set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
  draw_string "# Vies : ";
  draw_string (string_of_int nb_vie);;