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

with Ada.Calendar, Ada.Streams, Ada.Strings.Bounded;

package GID is

  type Image_descriptor is private;

  ---------------------------------------------------
  -- 1) Load the image header from the data stream --
  ---------------------------------------------------

  procedure Load_image_header (
    image   :    out Image_descriptor;
    from    : in out Ada.Streams.Root_Stream_Type'Class;
    try_tga :        Boolean:= False
  );

  -- try_tga: if no known signature is found, assume it might be
  -- the TGA format (which hasn't a signature) and try to load an
  -- image of this format

  unknown_image_format,
  known_but_unsupported_image_format,
  unsupported_image_subformat: exception;

  -----------------------------------------------------------------
  -- 2) If needed, use dimensions to reserve an in-memory bitmap --
  -----------------------------------------------------------------

  function Pixel_width (image: Image_descriptor) return Positive;
  function Pixel_height (image: Image_descriptor) return Positive;

  --------------------------------------------------------------------
  -- 3) Load and decode the image itself. If the image is animated, --
  --    call Load_image_contents until next_frame is 0.0            --
  --------------------------------------------------------------------

  type Primary_color_mode is (bits_8_mode, bits_16_mode);
  -- coding of primary colors (red, green or blue)
  -- 8-bit is usual: TrueColor, PC graphics, etc.;
  -- it is 16-bit in some high-end apps/devices/formats

  subtype Primary_color_range is Natural;
  -- We assume that Natural is large enough to contain
  -- all possible Primary_color_range in the generic parts of GID

  generic
    primary_color_coding: Primary_color_mode;
    --
    type Opacity_range is range <>;
    -- Opacity_range'First: fully transparent: pixel is invisible
    -- Opacity_range'Last : fully opaque     : pixel replaces
    --   background pixel
    --
    with procedure Set_X_Y (x, y: Natural);
    pragma Inline(Set_X_Y);
    -- After Set_X_Y, next pixel is meant to be displayed at position (x,y)
    with procedure Put_Pixel (
      red, green, blue : Primary_color_range;
      alpha            : Opacity_range
    );
    pragma Inline(Put_Pixel);
    -- When Put_Pixel is called twice without a Set_X_Y inbetween,
    -- the pixel must be displayed on the next X position after the last one.
    -- [Rationale: if the image lands into an array, the address calculation
    --  can be made only at the beginning of each line]
  procedure Load_image_contents (
    image     : in     Image_descriptor;
    next_frame:    out Ada.Calendar.Day_Duration
      -- real time lapse foreseen between the first image
      -- and the image right after this one; 0.0 if no next frame
  );

  ---------------------------------------
  -- Some informations about the image --
  ---------------------------------------

  type Image_format_type is
    ( -- Bitmap formats
      BMP, FITS, GIF, JPEG, PNG, TGA, TIFF
    );

  function Image_format (image: Image_descriptor) return Image_format_type;

  function Image_detailed_format (image: Image_descriptor) return String;
  -- example: "GIF89a, interlaced"

  function Bits_per_pixel (image: Image_descriptor) return Positive;

  --------------------------------------------------------------
  -- Information about this package - e.g. for an "about" box --
  --------------------------------------------------------------

  version   : constant String:= "0.1";
  reference : constant String:= "xx-yyy-2010";
  web: constant String:= "http://sf.net/projects/gen-img-dec/";
  -- hopefully the latest version is at that URL...

private

  type U8  is mod 2 ** 8;   for U8'Size  use 8;
  type U16 is mod 2 ** 16;  for U16'Size use 16;
  type U32 is mod 2 ** 32;  for U32'Size use 32;

  package Bounded_255 is
    new Ada.Strings.Bounded.Generic_Bounded_Length(255);

  type Stream_Access is access all Ada.Streams.Root_Stream_Type'Class;

  type RGB_Color is record
    red, green, blue : Primary_color_range;
  end record;

  type Color_table is array (Integer range <>) of RGB_Color;

  type p_Color_table is access Color_table;

  type Image_descriptor is record
    format             : Image_format_type;
    detailed_format    : Bounded_255.Bounded_String; -- for humans only!
    width, height      : Positive;
    bits_per_pixel     : Positive;
    stream             : Stream_Access;
    palette            : p_Color_table:= null;
  end record;

end GID;
