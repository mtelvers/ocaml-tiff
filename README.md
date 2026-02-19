# ocaml-tiff

A pure OCaml library for reading and writing TIFF and GeoTIFF files. No C dependencies.

## Features

- **Read and write** TIFF images as RGB24 or RGBA32
- **GeoTIFF support** — GeoKeys, coordinate transformations, EPSG codes, bounding boxes
- **Native raster access** — preserve full precision (uint8/uint16/int16/int32/float32/float64) for scientific data
- **Compression** — None, PackBits, LZW, Deflate (all implemented in pure OCaml)
- **Image layouts** — strip-based and tile-based
- **Photometric interpretations** — WhiteIsZero, BlackIsZero, RGB, Palette, CMYK, YCbCr
- **Bit depths** — 1, 2, 4, 8, 16 bits per sample
- **Multi-IFD files** — Cloud Optimized GeoTIFF (COG) overview support
- **ICC colour profiles**
- **Horizontal differencing predictor** for improved LZW/Deflate compression

## Installation

```
opam install tiff
```

Or pin from source:

```
opam pin add tiff .
```

## Quick start

### Reading and writing images

```ocaml
(* Read a TIFF file *)
let img = Tiff.read "photo.tiff" in
Printf.printf "Size: %dx%d\n" img.width img.height;

(* Access a pixel *)
let r, g, b = Tiff.get_pixel img 10 20 in
Printf.printf "R=%d G=%d B=%d\n" r g b;

(* Write it back *)
Tiff.write "output.tiff" img
```

### Creating images

```ocaml
let w = 100 and h = 100 in
let pixels =
  Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (w * h * 3)
in
(* Fill pixels... *)
let img = Tiff.create_image w h pixels in
Tiff.write "new.tiff" img
```

### Compression options

```ocaml
(* LZW-compressed, tiled *)
Tiff.write_with_options
  { compression = Compress_lzw; tile_size = Some 256; predictor = true }
  "tiled.tiff" img;

(* PackBits *)
Tiff.write_with_options
  { compression = Compress_packbits; tile_size = None; predictor = false }
  "packbits.tiff" img
```

### GeoTIFF metadata

```ocaml
let geo = Tiff.read_geo "map.tiff" in

(* EPSG code *)
(match Tiff.epsg_code geo with
 | Some code -> Printf.printf "EPSG: %d\n" code
 | None -> ());

(* Bounding box *)
(match Tiff.bounding_box geo with
 | Some (min_x, min_y, max_x, max_y) ->
   Printf.printf "Bounds: (%.6f, %.6f) - (%.6f, %.6f)\n"
     min_x min_y max_x max_y
 | None -> ());

(* Pixel to geographic coordinates *)
(match Tiff.raster_to_model geo 100.0 200.0 with
 | Some (lon, lat) -> Printf.printf "Lon=%.6f Lat=%.6f\n" lon lat
 | None -> ())
```

### Native raster data

For scientific data where you need full precision rather than 8-bit RGB:

```ocaml
let raster = Tiff.read_raster "elevation.tiff" in
match raster.data with
| Float32 arr ->
  Printf.printf "Elevation at (0,0): %f\n"
    (Bigarray.Array1.get arr 0)
| _ -> ()
```

## Command-line tool

The `tiff_info` tool dumps TIFF and GeoTIFF metadata:

```
$ tiff_info map.tiff
File: map.tiff
Number of IFDs: 3

IFD 0:
  Width: 4096
  Height: 4096
  Compression: LZW
  Samples per pixel: 3
  Bits per sample: 8
  Tiled: true

GeoTIFF Metadata:
  Model type: Projected
  Raster type: PixelIsArea
  EPSG code: 3857
  Bounding box: (-180.000000, -85.051129) - (180.000000, 85.051129)
```

Build and run:

```
dune exec tools/tiff_info.exe -- file.tiff
```

## Building from source

```
dune build
```

Run the tests:

```
dune runtest
```

Generate documentation:

```
dune build @doc
```

## API documentation

The full API is documented in [`lib/tiff.mli`](lib/tiff.mli). Key entry points:

| Function | Description |
|----------|-------------|
| `Tiff.read` | Read a TIFF file to an RGB24/RGBA32 image |
| `Tiff.write` | Write an image as TIFF (LZW, strip-based) |
| `Tiff.write_with_options` | Write with specific compression/tiling |
| `Tiff.read_geo` | Read with GeoTIFF metadata |
| `Tiff.read_raster` | Read preserving native data types |
| `Tiff.create_image` | Create an RGB24 image from pixel data |
| `Tiff.create_rgba_image` | Create an RGBA32 image from pixel data |
| `Tiff.get_pixel` / `Tiff.set_pixel` | Per-pixel access |
| `Tiff.bounding_box` | Geographic bounds of a GeoTIFF |
| `Tiff.epsg_code` | EPSG coordinate reference system code |
| `Tiff.raster_to_model` | Pixel coordinates to geographic coordinates |
| `Tiff.ifd_count` / `Tiff.ifd_info` | Inspect multi-IFD files |

## Requirements

- OCaml >= 4.14
- dune >= 3.0

## License

MIT
