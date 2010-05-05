with GID.Buffering, GID.Color_tables;

package body GID.Decoding_BMP is

  use GID.Buffering;
  use GID.Color_tables;

  procedure Load (image: Image_descriptor) is
     stream_buf: Input_buffer;
     b01, b: U8:= 0;
     pair: Boolean:= True;
     bit: Natural range 0..7:= 0;
     --
     x, x_max, y: Natural;
     --
     palette: Color_table(0 .. 2**image.bits_per_pixel - 1);
     --
     procedure Fill_palettized is
       pragma Inline(Fill_palettized);
     begin
       Put_Pixel_2(
         x,y,
         Primary_color_range_2(palette(Integer(b)).Red),
         Primary_color_range_2(palette(Integer(b)).Green),
         Primary_color_range_2(palette(Integer(b)).Blue),
         Opacity_range_2'Last
       );
     end Fill_palettized;
     --
  begin
    Load_palette(image, palette);
    --
    Attach_Stream(stream_buf, image.stream);
    y:= 0;
    while y <= image.height-1 loop
      x:= 0;
      x_max:= image.width-1;
      case image.bits_per_pixel is
        when 1 => -- B/W
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
          while x <= x_max loop
            Get_Byte(stream_buf, b);
            Fill_palettized;
            x:= x + 1;
          end loop;
        when others =>
          null;
      end case;
      y:= y + 1;
    end loop;
  end Load;

end GID.Decoding_BMP;
