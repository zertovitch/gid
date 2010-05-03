---------------------------------
-- GID - Generic Image Decoder --
---------------------------------
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

package body GID is

  -----------------------
  -- Load_image_header --
  -----------------------

  procedure Load_image_header (
    image   :    out Image_descriptor;
    from    : in out Ada.Streams.Root_Stream_Type'Class;
    try_tga :        Boolean:= False
  )
  is
    use Bounded_255;
    --
    -- Crude image signature detection
    --
    procedure Read_image_signature is
      c: Character;
      signature: String(1..5); -- without the initial
    begin
      Character'Read(from'Access, c);
      case c is
        when 'B' =>
          Character'Read(from'Access, c);
          if c='M' then
            image.detailed_img_format:= To_Bounded_String("BMP");
            image.img_format:= BMP;
            return;
          end if;
        when 'S' =>
          String'Read(from'Access, signature);
          if signature = "IMPLE"  then
            image.detailed_img_format:= To_Bounded_String("FITS");
            image.img_format:= FITS;
            return;
          end if;
        when 'G' =>
          String'Read(from'Access, signature);
          if signature = "IF87a" or signature = "IF89a" then
            image.detailed_img_format:= To_Bounded_String('G' & signature & ", ");
            image.img_format:= GIF;
            return;
          end if;
        when Character'Val(16#FF#) =>
          Character'Read(from'Access, c);
          if c=Character'Val(16#D8#) then
            image.detailed_img_format:= To_Bounded_String("JPEG");
            image.img_format:= JPEG;
            return;
          end if;
        when Character'Val(16#89#) =>
          String'Read(from'Access, signature(1..3));
          if signature(1..3) = "PNG" then
            image.detailed_img_format:= To_Bounded_String("PNG");
            image.img_format:= PNG;
            return;
          end if;
        when others =>
          if try_tga then
            null;
          else
            raise unknown_image_format;
          end if;
      end case;
      raise unknown_image_format;
    end;

  begin
    Read_image_signature;
    -- !! width,... !!
  end Load_image_header;

  -----------------
  -- Pixel_width --
  -----------------

  function Pixel_width (image: Image_descriptor) return Positive is
  begin
    return image.width;
  end Pixel_width;

  ------------------
  -- Pixel_height --
  ------------------

  function Pixel_height (image: Image_descriptor) return Positive is
  begin
    return image.height;
  end Pixel_height;

  -------------------------
  -- Load_image_contents --
  -------------------------

  procedure Load_image_contents
   (image     : in  Image_descriptor;
    from      :     Ada.Streams.Root_Stream_Type'Class;
    next_frame: out Ada.Calendar.Day_Duration)
  is
  begin
    --  Generated stub: replace with real body!
    raise Program_Error;
  end Load_image_contents;

  ------------------
  -- Image_format --
  ------------------

  function Image_format (image: Image_descriptor) return Image_format_type is
  begin
    return image.img_format;
  end Image_format;

  ---------------------------
  -- Image_detailed_format --
  ---------------------------

  function Image_detailed_format (image: Image_descriptor) return String is
  begin
    return Bounded_255.To_String(image.detailed_img_format);
  end Image_detailed_format;

end GID;
