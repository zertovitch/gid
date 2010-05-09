with GID.Buffering;                     use GID.Buffering;

package body GID.Decoding_BMP is

  procedure Load (image: in Image_descriptor) is
    b01, b, br, bg, bb: U8:= 0;
    x, x_max, y: Natural;
    --
    procedure Fill_palettized is
      pragma Inline(Fill_palettized);
    begin
      case primary_color_coding_2 is
        when bits_8_mode =>
          Put_Pixel_2(
            image.palette(Integer(b)).Red,
            image.palette(Integer(b)).Green,
            image.palette(Integer(b)).Blue,
            255
          );
        when bits_16_mode =>
          Put_Pixel_2(
            256 * image.palette(Integer(b)).Red,
            256 * image.palette(Integer(b)).Green,
            256 * image.palette(Integer(b)).Blue,
            65_535
          );
      end case;
    end Fill_palettized;
    --
    stream_buf: Input_buffer;
    pair: Boolean;
    bit: Natural range 0..7;
    --
    line_bits: constant Float:= Float(image.width * image.bits_per_pixel);
    padded_line_size: constant Positive:= 4 * Integer(Float'Ceiling(line_bits / 32.0));
    unpadded_line_size: constant Positive:= Integer(Float'Ceiling(line_bits / 8.0));
    -- (in bytes)
  begin
    Attach_Stream(stream_buf, image.stream);
    y:= 0;
    while y <= image.height-1 loop
      x:= 0;
      x_max:= image.width-1;
      case image.bits_per_pixel is
        when 1 => -- B/W
          bit:= 0;
          Set_X_Y_2(x,y);
          while x <= x_max loop
            if bit=0 then
              Get_Byte(stream_buf, b01);
            end if;
            b:= (b01 and 16#80#) / 16#80#;
            Fill_palettized;
            b01:= b01 * 2; -- cannot overflow.
            if bit=7 then
              bit:= 0;
            else
              bit:= bit + 1;
            end if;
            x:= x + 1;
          end loop;
        when 4 => -- 16 colour image
          pair:= True;
          Set_X_Y_2(x,y);
          while x <= x_max loop
            if pair then
              Get_Byte(stream_buf, b01);
              b:= (b01 and 16#F0#) / 16#10#;
            else
              b:= (b01 and 16#0F#);
            end if;
            pair:= not pair;
            Fill_palettized;
            x:= x + 1;
          end loop;
        when 8 => -- 256 colour image
          Set_X_Y_2(x,y);
          while x <= x_max loop
            Get_Byte(stream_buf, b);
            Fill_palettized;
            x:= x + 1;
          end loop;
        when 24 => -- RGB, 256 colour per primary colour
          Set_X_Y_2(x,y);
          while x <= x_max loop
            Get_Byte(stream_buf, bb);
            Get_Byte(stream_buf, bg);
            Get_Byte(stream_buf, br);
            case primary_color_coding_2 is
              when bits_8_mode =>
                Put_Pixel_2(
                  Integer(br),
                  Integer(bg),
                  Integer(bb),
                  255
                );
              when bits_16_mode =>
                Put_Pixel_2(
                  256 * Integer(br),
                  256 * Integer(bg),
                  256 * Integer(bb),
                  65_535
                );
            end case;
            x:= x + 1;
          end loop;
        when others =>
          null;
      end case;
      for i in unpadded_line_size + 1 .. padded_line_size loop
        Get_Byte(stream_buf, b);
      end loop;
      y:= y + 1;
    end loop;
  end Load;

end GID.Decoding_BMP;
