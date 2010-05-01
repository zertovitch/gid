---------------------------------
-- GID - Generic Image Decoder --
---------------------------------
--
-- Purpose:
-- The Generic Image Decoder is a package for decoding a broad
-- variety of image formats, from any data stream, to any kind
-- of in-memory bitmap. Unconditionally portable code, independent
-- of operating system, processor and choice of compiler.
--
-- Image types supported:
-- bla bla bla
--
-- [License]

with Ada.Streams, Ada.Strings.Bounded;

package GID is

  type Image_descriptor is private;

  ---------------------------------------------------
  -- 1) Load the image header from the data stream --
  ---------------------------------------------------

  procedure Load_image_header (
    image: out Image_descriptor;
    from :     Ada.Streams.Root_Stream_Type'Class
  );
  unknown_image_type: exception;

  ------------------------------------------------------
  -- 2) Use dimensions to reserve an in-memory bitmap --
  ------------------------------------------------------

  function Pixel_width (image: Image_descriptor) return Positive;
  function Pixel_height (image: Image_descriptor) return Positive;

  -----------------------------------------
  -- 3) Load and decode the image itself --
  -----------------------------------------

  type Fundamental_color_range is new Integer range 0..255;
  type Color is record
    red, green, blue : Fundamental_color_range;
  end record;
  type Opacity_range is new Integer range 0..255;
  -- Opacity_range'First: fully tranparent: pixel is invisible
  -- Opacity_range'Last : fully opaque: pixel replaces background pixel

  generic
    with procedure Put_Pixel (
      x, y  : Natural;
      rgb   : Color;
      alpha : Opacity_range
     );
  procedure Load_image_contents (
    image: in Image_descriptor;
    from :    Ada.Streams.Root_Stream_Type'Class
  );

  ---------------------------------------
  -- Some informations about the image --
  ---------------------------------------

  type Image_format_type is (BMP, GIF, JPEG, PNG, TGA);

  function Image_format (image: Image_descriptor) return Image_format_type;

  function Image_detailed_format (image: Image_descriptor) return String;
  -- example: "GIF89a, interlaced"

  web: constant String:= "http://sf.net/projects/gen-img-dec/";
  -- hopefully the latest version is at that URL...

private

  type Palette is array(Natural range <>) of Color;
  type p_Palette is access Palette;

  package Bounded_255 is
    new Ada.Strings.Bounded.Generic_Bounded_Length(255);

  type Image_descriptor is record
    width, height    : Positive;
    opt_pal          : p_Palette;
    img_type         : Image_format_Type;
    detailed_img_type: Bounded_255.Bounded_String;
  end record;

end GID;