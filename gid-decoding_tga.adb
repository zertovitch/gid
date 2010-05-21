with GID.Buffering;                     use GID.Buffering;

package body GID.Decoding_TGA is

  ----------
  -- Load --
  ----------

  procedure Load (image: in Image_descriptor) is
    stream_buf: Input_buffer;
    -- Run Length Encoding --
    RLE_pixels_remaining: Natural:= 0;
    is_run_packet: Boolean;

    type Pixel is record
      br, bg, bb, ba: U8;
    end record;

    pix, pix_mem: Pixel;

    generic
      bpp: Positive;
    procedure Get_pixel;
    pragma Inline(Get_Pixel);
    --
    procedure Get_pixel is
    begin
      case bpp is
        when 32 => -- BGRA
          Get_Byte(stream_buf, pix.bb);
          Get_Byte(stream_buf, pix.bg);
          Get_Byte(stream_buf, pix.br);
          Get_Byte(stream_buf, pix.ba);
        when 24 => -- BGR
          Get_Byte(stream_buf, pix.bb);
          Get_Byte(stream_buf, pix.bg);
          Get_Byte(stream_buf, pix.br);
        when 8  => -- Gray
          Get_Byte(stream_buf, pix.bg);
          pix.br:= pix.bg;
          pix.bb:= pix.bg;
        when others =>
          null;
      end case;
    end Get_pixel;

    generic
      bpp: Positive;
    procedure RLE_Pixel;
    pragma Inline(RLE_Pixel);
    --
    procedure RLE_Pixel is
      tmp: U8;
      procedure Get_pixel_for_RLE is new Get_pixel(bpp);
    begin
      if RLE_pixels_remaining = 0 then -- load RLE code
        Get_Byte(stream_buf, tmp );
        Get_pixel_for_RLE;
        RLE_pixels_remaining:= U8'Pos(tmp and 16#7F#);
        is_run_packet:= (tmp and 16#80#) /= 0;
        if is_run_packet then
          pix_mem:= pix;
        end if;
      else
        if is_run_packet then
          pix:= pix_mem;
        else
          Get_pixel_for_RLE;
        end if;
        RLE_pixels_remaining:= RLE_pixels_remaining - 1;
      end if;
    end RLE_Pixel;

    procedure Output_Pixel is
    pragma Inline(Output_Pixel);
    begin
      case Primary_color_range'Modulus is
        when 256 =>
          Put_Pixel(
            Primary_color_range(pix.br),
            Primary_color_range(pix.bg),
            Primary_color_range(pix.bb),
            Primary_color_range(pix.ba)
          );
        when 65_536 =>
          Put_Pixel(
            16#101#  * Primary_color_range(pix.br),
            16#101#  * Primary_color_range(pix.bg),
            16#101#  * Primary_color_range(pix.bb),
            16#101#  * Primary_color_range(pix.ba)
            -- 16#101# because max intensity FF goes to FFFF
          );
        when others =>
          raise invalid_primary_color_range;
      end case;
    end Output_Pixel;

    procedure Get_RGBA is -- 32 bits
      procedure Get_pixel_32 is new Get_pixel(32);
    begin
      for y in 0..image.height-1 loop
        Set_X_Y(0, y);
        for x in 0..image.width-1 loop
          Get_pixel_32;
          Output_Pixel;
        end loop;
        Feedback(((y+1)*100)/image.height);
      end loop;
    end Get_RGBA;

    procedure Get_RGB is -- 24 bits
      procedure Get_pixel_24 is new Get_pixel(24);
    begin
      for y in 0..image.height-1 loop
        Set_X_Y(0, y);
        for x in 0..image.width-1 loop
          Get_pixel_24;
          Output_Pixel;
        end loop;
        Feedback(((y+1)*100)/image.height);
      end loop;
    end Get_RGB;

    procedure Get_Gray is
      procedure Get_pixel_8  is new Get_pixel(8);
    begin
      for y in 0..image.height-1 loop
        Set_X_Y(0, y);
        for x in 0..image.width-1 loop
          Get_pixel_8;
          Output_Pixel;
        end loop;
        Feedback(((y+1)*100)/image.height);
      end loop;
    end Get_Gray;

    procedure RLE_pixel_32 is new RLE_pixel(32);
    procedure RLE_pixel_24 is new RLE_pixel(24);
    procedure RLE_pixel_8  is new RLE_pixel(8);

  begin
    pix.ba:= 255; -- opaque is default
    Attach_Stream(stream_buf, image.stream);
    if image.RLE_encoded then
      RLE_pixels_remaining:= 0;
      for y in 0..image.height-1 loop
        Set_X_Y(0, y);
        case image.bits_per_pixel is
          when 32 =>
            for x in 0..image.width-1 loop
              RLE_Pixel_32;
              Output_Pixel;
            end loop;
          when 24 =>
            for x in 0..image.width-1 loop
              RLE_Pixel_24;
              Output_Pixel;
            end loop;
          when 8  =>
            for x in 0..image.width-1 loop
              RLE_Pixel_8;
              Output_Pixel;
            end loop;
          when others => null;
        end case;
        Feedback(((y+1)*100)/image.height);
      end loop;
    else
      case image.bits_per_pixel is
        when 32 =>
          Get_RGBA;
        when 24 =>
          Get_RGB;
        when 8  =>
          Get_Gray;
        when others => null;
      end case;
    end if;
  end Load;

end GID.Decoding_TGA;
