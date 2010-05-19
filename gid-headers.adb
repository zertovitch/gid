---------------------------------
-- GID - Generic Image Decoder --
---------------------------------
--
-- Private child of GID, with helpers for identifying
-- image formats and reading header informations.
--

with GID.Color_tables,
     GID.Decoding_PNG;

with Ada.Exceptions, Ada.Unchecked_Deallocation;

package body GID.Headers is

  use Ada.Exceptions;

  -------------------------------------------------------
  -- The very first: read signature to identify format --
  -------------------------------------------------------

  procedure Load_signature (
    image   :    out Image_descriptor;
    try_tga :        Boolean:= False

  )
  is
    use Bounded_255;
    c, d: Character;
    FITS_challenge: String(1..5); -- without the initial
    GIF_challenge : String(1..5); -- without the initial
    PNG_challenge : String(1..7); -- without the initial
    PNG_signature: constant String:=
      "PNG" & ASCII.CR & ASCII.LF & ASCII.SUB & ASCII.LF;
    procedure Dispose is
      new Ada.Unchecked_Deallocation(Color_table, p_Color_table);
  begin
    Dispose(image.palette);
    image.next_frame:= 0.0;
    Character'Read(image.stream, c);
    image.first_byte:= Character'Pos(c);
    case c is
      when 'B' =>
        Character'Read(image.stream, c);
        if c='M' then
          image.detailed_format:= To_Bounded_String("BMP");
          image.format:= BMP;
          return;
        end if;
      when 'S' =>
        String'Read(image.stream, FITS_challenge);
        if FITS_challenge = "IMPLE"  then
          image.detailed_format:= To_Bounded_String("FITS");
          image.format:= FITS;
          return;
        end if;
      when 'G' =>
        String'Read(image.stream, GIF_challenge);
        if GIF_challenge = "IF87a" or GIF_challenge = "IF89a" then
          image.detailed_format:= To_Bounded_String('G' & GIF_challenge & ", ");
          image.format:= GIF;
          return;
        end if;
      when 'I' | 'M' =>
        Character'Read(image.stream, d);
        if c=d then
          if c = 'I' then
            image.detailed_format:= To_Bounded_String("TIFF, little-endian");
          else
            image.detailed_format:= To_Bounded_String("TIFF, big-endian");
          end if;
          image.format:= TIFF;
          return;
        end if;
      when Character'Val(16#FF#) =>
        Character'Read(image.stream, c);
        if c=Character'Val(16#D8#) then
          image.detailed_format:= To_Bounded_String("JPEG");
          image.format:= JPEG;
          return;
        end if;
      when Character'Val(16#89#) =>
        String'Read(image.stream, PNG_challenge);
        if PNG_challenge = PNG_signature  then
          image.detailed_format:= To_Bounded_String("PNG");
          image.format:= PNG;
          return;
        end if;
      when others =>
        if try_tga then
          image.detailed_format:= To_Bounded_String("TGA");
          image.format:= TGA;
          return;
        else
          raise unknown_image_format;
        end if;
    end case;
    raise unknown_image_format;
  end Load_signature;

  generic
    type Number is mod <>;
  procedure Read_Intel_x86_number(
    n    :    out Number;
    from : in     Stream_Access
  );
    pragma Inline(Read_Intel_x86_number);

  generic
    type Number is mod <>;
  procedure Big_endian_number(
    n    :    out Number;
    from : in     Stream_Access
  );
    pragma Inline(Big_endian_number);

  procedure Read_Intel_x86_number(
    n    :    out Number;
    from : in     Stream_Access
  )
  is
    b: U8;
    m: Number:= 1;
  begin
    n:= 0;
    for i in 1..Number'Size/8 loop
      U8'Read(from, b);
      n:= n + m * Number(b);
      m:= m * 256;
    end loop;
  end Read_Intel_x86_number;

  procedure Big_endian_number(
    n    :    out Number;
    from : in     Stream_Access
  )
  is
    b: U8;
  begin
    n:= 0;
    for i in 1..Number'Size/8 loop
      U8'Read(from, b);
      n:= n * 256 + Number(b);
    end loop;
  end Big_endian_number;

  procedure Read_Intel is new Read_Intel_x86_number( U16 );
  procedure Read_Intel is new Read_Intel_x86_number( U32 );
  procedure Big_endian is new Big_endian_number( U32 );

  ----------------------------------------------------------
  -- Loading of various format's headers (past signature) --
  ----------------------------------------------------------

  ----------------
  -- BMP header --
  ----------------

  procedure Load_BMP_header (image: in out Image_descriptor) is
    file_size, offset, info_header, n, dummy: U32;
    pragma Warnings(off, dummy);
    w, dummy16: U16;
  begin
    --   Pos= 3, read the file size
    Read_Intel(file_size, image.stream);
    --   Pos= 7, read four bytes, unknown
    Read_Intel(dummy, image.stream);
    --   Pos= 11, read four bytes offset, file top to bitmap data.
    --            For 256 colors, this is usually 36 04 00 00
    Read_Intel(offset, image.stream);
    --   Pos= 15. The beginning of Bitmap information header.
    --   Data expected:  28H, denoting 40 byte header
    Read_Intel(info_header, image.stream);
    --   Pos= 19. Bitmap width, in pixels.  Four bytes
    Read_Intel(n, image.stream);
    image.width:=  Natural(n);
    --   Pos= 23. Bitmap height, in pixels.  Four bytes
    Read_Intel(n, image.stream);
    image.height:= Natural(n);
    --   Pos= 27, skip two bytes.  Data is number of Bitmap planes.
    Read_Intel(dummy16, image.stream); -- perform the skip
    --   Pos= 29, Number of bits per pixel
    --   Value 8, denoting 256 color, is expected
    Read_Intel(w, image.stream);
    case w is
      when 1 | 4 | 8 | 24 =>
        null;
      when others =>
        Raise_exception(
          unsupported_image_subformat'Identity,
          "bit depth =" & U16'Image(w)
        );
    end case;
    image.bits_per_pixel:= Integer(w);
    --   Pos= 31, read four bytes
    Read_Intel(n, image.stream);          -- Type of compression used
    -- BI_RLE8 = 1
    -- BI_RLE4 = 2
    if n /= 0 then
      Raise_exception(
        unsupported_image_subformat'Identity,
        "RLE compression"
      );
    end if;
    --
    Read_Intel(dummy, image.stream); -- Pos= 35, image size
    Read_Intel(dummy, image.stream); -- Pos= 39, horizontal resolution
    Read_Intel(dummy, image.stream); -- Pos= 43, vertical resolution
    Read_Intel(n, image.stream); -- Pos= 47, number of palette colors
    if image.bits_per_pixel <= 8 then
      if n = 0 then
        image.palette:= new Color_Table(0..2**image.bits_per_pixel-1);
      else
        image.palette:= new Color_Table(0..Natural(n)-1);
      end if;
    end if;
    Read_Intel(dummy, image.stream); -- Pos= 51, number of important colors
    --   Pos= 55 (36H), - start of palette
    Color_tables.Load_palette(image);
  end Load_BMP_header;

  procedure Load_FITS_header (image: in out Image_descriptor) is
  begin
    raise known_but_unsupported_image_format; -- !!
  end Load_FITS_header;

  ----------------
  -- GIF header --
  ----------------

  procedure Load_GIF_header (image: in out Image_descriptor) is
    -- GIF - logical screen descriptor
    screen_width, screen_height          : U16;
    packed, background, aspect_ratio_code : U8;
    global_palette: Boolean;
  begin
    Read_Intel(screen_width, image.stream);
    Read_Intel(screen_height, image.stream);
    image.width:= Natural(screen_width);
    image.height:= Natural(screen_height);
    U8'Read(image.stream, packed);
    --  Global Color Table Flag       1 Bit
    --  Color Resolution              3 Bits
    --  Sort Flag                     1 Bit
    --  Size of Global Color Table    3 Bits
    global_palette:= (packed and 16#80#) /= 0;
    image.bits_per_pixel:= Natural((packed and 16#7F#)/16#10#) + 1;
    -- Indicative:
    -- iv) [...] This value should be set to indicate the
    --     richness of the original palette
    U8'Read(image.stream, background);
    U8'Read(image.stream, aspect_ratio_code);
    if global_palette then
      image.subformat_id:= 1+(Natural(packed and 16#07#));
      -- palette's bits per pixels, usually <= image's
      --
      --  if image.subformat_id > image.bits_per_pixel then
      --    Raise_exception(
      --      error_in_image_data'Identity,
      --      "GIF: global palette has more colors than the image" &
      --       image.subformat_id'img & image.bits_per_pixel'img
      --    );
      --  end if;
      image.palette:= new Color_Table(0..2**(image.subformat_id)-1);
      Color_tables.Load_palette(image);
    end if;
  end Load_GIF_header;

  procedure Load_JPEG_header (image: in out Image_descriptor) is
  begin
    raise known_but_unsupported_image_format; -- !!
  end Load_JPEG_header;

  ----------------
  -- PNG header --
  ----------------

  procedure Load_PNG_header (image: in out Image_descriptor) is
    use Decoding_PNG;
    ch: Chunk_head;
    n, dummy: U32;
    pragma Warnings(off, dummy);
    b, color_type: U8;
    palette: Boolean:= False;
  begin
    Read(image, ch);
    if ch.kind /= IHDR then
      Raise_exception(
        error_in_image_data'Identity,
        "Expected 'IHDR' chunk in PNG stream"
      );
    end if;
    Big_endian(n, image.stream);
    image.width:=  Natural(n);
    Big_endian(n, image.stream);
    image.height:= Natural(n);
    U8'Read(image.stream, b);
    image.bits_per_pixel:= Integer(b);
    U8'Read(image.stream, color_type);
    image.subformat_id:= Integer(color_type);
    case color_type is
      when 0 =>
        image.greyscale:= True;
      when 2 =>
        image.bits_per_pixel:= 3 * image.bits_per_pixel; -- RGB
      when 3 =>
        palette:= True;
      when 4 =>
        image.bits_per_pixel:= 2 * image.bits_per_pixel; -- Grey & Alpha
        image.greyscale:= True;
        image.transparency:= True;
      when 6 =>
        image.bits_per_pixel:= 4 * image.bits_per_pixel; -- RGBA
        image.transparency:= True;
      when others =>
        Raise_exception(
          error_in_image_data'Identity,
          "Unknown PNG color type"
        );
    end case;
    U8'Read(image.stream, b);
    if b /= 0 then
      Raise_exception(
        error_in_image_data'Identity,
        "Unknown PNG compression; ISO/IEC 15948:2003" &
        " knows only 'method 0' (deflate)"
      );
    end if;
    U8'Read(image.stream, b);
    if b /= 0 then
      Raise_exception(
        error_in_image_data'Identity,
        "Unknown PNG filtering; ISO/IEC 15948:2003 knows only 'method 0'"
      );
    end if;
    U8'Read(image.stream, b);
    image.interlaced:= b = 1; -- Adam7
    Big_endian(dummy, image.stream); -- Chunk's CRC
    if palette then
      loop
        Read(image, ch);
        case ch.kind is
          when IEND =>
            raise error_in_image_data; -- must be a palette
          when PLTE =>
            if ch.length rem 3 /= 0 then
              raise error_in_image_data; -- must be a multiple of 3
            end if;
            image.palette:= new Color_Table(0..Integer(ch.length/3)-1);
            Color_tables.Load_palette(image);
            Big_endian(dummy, image.stream); -- Chunk's CRC
            exit;
          when others =>
            -- skip chunk data and CRC
            for i in 1..ch.length + 4 loop
              U8'Read(image.stream, b);
            end loop;
        end case;
      end loop;
    end if;
  end Load_PNG_header;

  ------------------------
  -- TGA (Targa) header --
  ------------------------

  procedure Load_TGA_header (image: in out Image_descriptor) is
    TGA_type: Byte_Array(0..3);
    info    : Byte_Array(0..5);
    dummy   : Byte_Array(1..8);
    image_type: Integer;
  begin
    -- read in colormap info and image type
    TGA_type(0):= image.first_byte;
    Byte_Array'Read( image.stream, TGA_type(1..3) );
    -- seek past the header and useless info
    Byte_Array'Read( image.stream, dummy );
    Byte_Array'Read( image.stream, info );

    if TGA_type(1) /= U8'Val(0) then
      raise unsupported_image_subformat;
        -- palette;
    end if;

    -- Image type:
    --      1 = 8-bit palette style
    --      2 = Direct [A]RGB image
    --      3 = grayscale
    --      9 = RLE version of Type 1
    --     10 = RLE version of Type 2
    --     11 = RLE version of Type 3

    image_type:= U8'Pos(TGA_type(2));
    image.RLE_encoded:= image_Type >= 9;
    if image.RLE_encoded then
      image_type:= image_type - 8;
    end if;
    if image_type /= 2 and image_type /= 3 then
      raise unsupported_image_subformat;
        -- "TGA type =" & Integer'Image(image_type);
    end if;

    image.width  := U8'Pos(info(0)) + U8'Pos(info(1)) * 256;
    image.height := U8'Pos(info(2)) + U8'Pos(info(3)) * 256;
    image.bits_per_pixel := U8'Pos(info(4));

    -- make sure we are loading a supported TGA_type
    case image.bits_per_pixel is
      when 32 | 24 | 8 =>
        null;
      when others =>
        raise unsupported_image_subformat;
        -- bpp
    end case;
  end Load_TGA_header;

  procedure Load_TIFF_header (image: in out Image_descriptor) is
  begin
    raise known_but_unsupported_image_format; -- !!
  end Load_TIFF_header;

end;
