package body GID.Color_tables is

  procedure Load_palette (image: in out Image_descriptor) is
    c, d: U8;
  begin
    if image.palette = null then
      return;
    end if;
    declare
      palette: Color_Table renames image.palette.all;
    begin
      for i in palette'Range loop
        case image.format is
          when BMP =>
            -- order is BGRx
            U8'Read(image.stream, Palette(i).Blue);
            U8'Read(image.stream, Palette(i).Green);
            U8'Read(image.stream, Palette(i).Red);
            U8'Read(image.stream, c);
            -- x discarded
          when GIF | PNG =>
            -- order is RGB
            U8'Read(image.stream, Palette(i).Red);
            U8'Read(image.stream, Palette(i).Green);
            U8'Read(image.stream, Palette(i).Blue);
          when TGA =>
            case image.subformat_id is -- = palette's bit depth
              when 8 => -- Grey
                U8'Read(image.stream, c);
                Palette(i).Red  := c;
                Palette(i).Green:= c;
                Palette(i).Blue := c;
              when 15 | 16 => -- RGB, 5 bit per channel
                U8'Read(image.stream, c);
                U8'Read(image.stream, d);
                Palette(i).Red  := d / 4;
                Palette(i).Green:= (d and 3) + c / 32;
                Palette(i).Blue := c and 31;
              when 24 | 32 => -- RGB | RGBA, 8 bit per channel
                U8'Read(image.stream, Palette(i).Blue);
                U8'Read(image.stream, Palette(i).Green);
                U8'Read(image.stream, Palette(i).Red);
              when others =>
                null;
            end case;
          when others =>
            null; -- !!
        end case;
      end loop;
    end;
  end Load_palette;

end GID.Color_tables;
