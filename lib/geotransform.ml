(** GeoTIFF coordinate transformation parsing.

    Supports two transformation models:
    1. ModelTiepoint (33922) + ModelPixelScale (33550): simple tie + scale
    2. ModelTransformation (34264): full 4x4 affine matrix

    Provides raster<->model coordinate conversion. *)

type tiepoint = {
  raster_x : float;
  raster_y : float;
  raster_z : float;
  model_x : float;
  model_y : float;
  model_z : float;
}

type pixel_scale = {
  scale_x : float;
  scale_y : float;
  scale_z : float;
}

type t = {
  tiepoints : tiepoint list;
  pixel_scale : pixel_scale option;
  transformation : float array option;  (* 4x4 = 16 doubles *)
}

let empty = {
  tiepoints = [];
  pixel_scale = None;
  transformation = None;
}

let parse_tiepoints doubles =
  let n = Array.length doubles in
  let num_tiepoints = n / 6 in
  List.init num_tiepoints (fun i ->
    let off = i * 6 in
    { raster_x = doubles.(off);
      raster_y = doubles.(off + 1);
      raster_z = doubles.(off + 2);
      model_x = doubles.(off + 3);
      model_y = doubles.(off + 4);
      model_z = doubles.(off + 5);
    }
  )

let parse_pixel_scale doubles =
  if Array.length doubles >= 3 then
    Some { scale_x = doubles.(0); scale_y = doubles.(1); scale_z = doubles.(2) }
  else
    None

let parse_transformation doubles =
  if Array.length doubles >= 16 then
    Some (Array.sub doubles 0 16)
  else
    None

let raster_to_model t rx ry =
  match t.transformation with
  | Some m ->
    (* Full 4x4 matrix transformation *)
    let mx = m.(0) *. rx +. m.(1) *. ry +. m.(3) in
    let my = m.(4) *. rx +. m.(5) *. ry +. m.(7) in
    Some (mx, my)
  | None ->
    match t.tiepoints, t.pixel_scale with
    | tp :: _, Some ps ->
      (* Tiepoint + pixel scale *)
      let mx = tp.model_x +. (rx -. tp.raster_x) *. ps.scale_x in
      let my = tp.model_y -. (ry -. tp.raster_y) *. ps.scale_y in
      Some (mx, my)
    | _ -> None

let model_to_raster t mx my =
  match t.transformation with
  | Some m ->
    (* Invert the 2x2 submatrix *)
    let det = m.(0) *. m.(5) -. m.(1) *. m.(4) in
    if abs_float det < 1e-15 then None
    else
      let dx = mx -. m.(3) in
      let dy = my -. m.(7) in
      let rx = (m.(5) *. dx -. m.(1) *. dy) /. det in
      let ry = (m.(0) *. dy -. m.(4) *. dx) /. det in
      Some (rx, ry)
  | None ->
    match t.tiepoints, t.pixel_scale with
    | tp :: _, Some ps ->
      if abs_float ps.scale_x < 1e-15 || abs_float ps.scale_y < 1e-15 then None
      else
        let rx = tp.raster_x +. (mx -. tp.model_x) /. ps.scale_x in
        let ry = tp.raster_y -. (my -. tp.model_y) /. ps.scale_y in
        Some (rx, ry)
    | _ -> None

let bounding_box t ~width ~height =
  match raster_to_model t 0.0 0.0 with
  | None -> None
  | Some (x0, y0) ->
    match raster_to_model t (float_of_int width) (float_of_int height) with
    | None -> None
    | Some (x1, y1) ->
      let min_x = min x0 x1 in
      let max_x = max x0 x1 in
      let min_y = min y0 y1 in
      let max_y = max y0 y1 in
      Some (min_x, min_y, max_x, max_y)
