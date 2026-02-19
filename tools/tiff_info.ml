(** CLI tool for dumping TIFF/GeoTIFF metadata. *)

let compression_name = function
  | 1 -> "None"
  | 5 -> "LZW"
  | 8 -> "Deflate"
  | 32773 -> "PackBits"
  | 32946 -> "Deflate (old)"
  | n -> Printf.sprintf "Unknown (%d)" n

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: tiff_info <file.tiff>\n";
    exit 1
  end;
  let filename = Sys.argv.(1) in
  Printf.printf "File: %s\n" filename;
  let infos = Tiff.ifd_info filename in
  Printf.printf "Number of IFDs: %d\n\n" (List.length infos);
  List.iteri (fun i info ->
    Printf.printf "IFD %d:\n" i;
    Printf.printf "  Width: %d\n" info.Tiff.ifd_width;
    Printf.printf "  Height: %d\n" info.Tiff.ifd_height;
    Printf.printf "  Compression: %s\n" (compression_name info.Tiff.ifd_compression);
    Printf.printf "  Samples per pixel: %d\n" info.Tiff.ifd_samples_per_pixel;
    Printf.printf "  Bits per sample: %d\n" info.Tiff.ifd_bits_per_sample;
    Printf.printf "  Tiled: %b\n" info.Tiff.ifd_is_tiled;
    Printf.printf "  NewSubfileType: %d%s\n" info.Tiff.ifd_new_subfile_type
      (if info.Tiff.ifd_new_subfile_type land 1 = 1 then " (reduced resolution)"
       else "");
    Printf.printf "\n"
  ) infos;
  (* Try to read GeoTIFF metadata *)
  begin try
    let geo = Tiff.read_geo filename in
    begin match geo.geo.geokeys with
    | None -> Printf.printf "No GeoKeys found.\n"
    | Some gk ->
      Printf.printf "GeoTIFF Metadata:\n";
      Printf.printf "  GeoKey version: %d.%d.%d\n"
        gk.Tiff.Geokeys.version gk.Tiff.Geokeys.key_revision
        gk.Tiff.Geokeys.minor_revision;
      Printf.printf "  Number of keys: %d\n"
        (List.length gk.Tiff.Geokeys.keys);
      begin match Tiff.Geokeys.model_type gk with
      | Some 1 -> Printf.printf "  Model type: Projected\n"
      | Some 2 -> Printf.printf "  Model type: Geographic\n"
      | Some 3 -> Printf.printf "  Model type: Geocentric\n"
      | Some n -> Printf.printf "  Model type: %d\n" n
      | None -> ()
      end;
      begin match Tiff.Geokeys.raster_type gk with
      | Some 1 -> Printf.printf "  Raster type: PixelIsArea\n"
      | Some 2 -> Printf.printf "  Raster type: PixelIsPoint\n"
      | Some n -> Printf.printf "  Raster type: %d\n" n
      | None -> ()
      end;
      begin match Tiff.epsg_code geo with
      | Some code -> Printf.printf "  EPSG code: %d\n" code
      | None -> ()
      end
    end;
    begin match Tiff.bounding_box geo with
    | Some (min_x, min_y, max_x, max_y) ->
      Printf.printf "  Bounding box: (%.6f, %.6f) - (%.6f, %.6f)\n"
        min_x min_y max_x max_y
    | None -> ()
    end
  with Failure msg ->
    Printf.printf "Could not read GeoTIFF metadata: %s\n" msg
  end
