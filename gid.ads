---------------------------------
-- GID - Generic Image Decoder --
---------------------------------
--
-- Purpose:
--
-- The Generic Image Decoder is a package for decoding a broad
-- variety of image formats, from any data stream, to any kind
-- of in-memory bitmap. Unconditionally portable code, independent
-- of operating system, processor, endianess and choice of compiler.
--
-- Image types supported:
-- bla bla bla
--
--  Copyright (c) Gautier de Montmollin 2010
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.
--
-- NB: this is the MIT License, as found 2-May-2010 on the site
-- http://www.opensource.org/licenses/mit-license.php

with Ada.Streams, Ada.Strings.Bounded;

package GID is

  type Image_descriptor is private;

  ---------------------------------------------------
  -- 1) Load the image header from the data stream --
  ---------------------------------------------------

  procedure Load_image_header (
    image   : out Image_descriptor;
    from    :     Ada.Streams.Root_Stream_Type'Class;
    try_tga :     Boolean:= False
  );

  -- try_tga: if no known signature is found, assume it is the TGA
  -- format (which hasn't a signature) and try to load an image
  -- of this format

  unknown_image_format: exception;

  ------------------------------------------------------------------
  -- 2) Use dimensions to reserve an in-memory bitmap (if needed) --
  ------------------------------------------------------------------

  function Pixel_width (image: Image_descriptor) return Positive;
  function Pixel_height (image: Image_descriptor) return Positive;

  -----------------------------------------
  -- 3) Load and decode the image itself --
  -----------------------------------------

  generic
    type Fundamental_color_range is range <>;
    -- range for each fundamental color (red, green or blue)
    -- usually 0..255 (TrueColor, PC graphics, etc.);
    -- in some high-end apps/devices/formats: 0..65535
    --
    type Opacity_range is range <>;
    -- Opacity_range'First: fully tranparent: pixel is invisible
    -- Opacity_range'Last : fully opaque    : pixel replaces background pixel
    --
    with procedure Put_Pixel (
      x, y             : Natural;
      red, green, blue : Fundamental_color_range;
      alpha            : Opacity_range
    );
    pragma Inline(Put_Pixel);
    --
  procedure Load_image_contents (
    image: in Image_descriptor;
    from :    Ada.Streams.Root_Stream_Type'Class
  );

  unsupported_image_format,
  unsupported_image_subformat: exception;

  ---------------------------------------
  -- Some informations about the image --
  ---------------------------------------

  type Image_format_type is (BMP, FITS, GIF, JPEG, PNG, TGA);

  function Image_format (image: Image_descriptor) return Image_format_type;

  function Image_detailed_format (image: Image_descriptor) return String;
  -- example: "GIF89a, interlaced"

  web: constant String:= "http://sf.net/projects/gen-img-dec/";
  -- hopefully the latest version is at that URL...

private

  package Bounded_255 is
    new Ada.Strings.Bounded.Generic_Bounded_Length(255);

  type Image_descriptor is record
    width, height    : Positive;
    img_type         : Image_format_Type;
    detailed_img_type: Bounded_255.Bounded_String;
  end record;

end GID;