package body GID.Color_tables is

  procedure Load_palette (
    image     : in     Image_descriptor;
    palette   :    out Color_table
  )
  is
    c: U8;
  begin
    for i in palette'Range loop
      case image.format is
        when BMP =>
          -- order is BGRx
          U8'Read(image.stream, c);
          Palette(i).Blue:= Primary_color_range(c);
          U8'Read(image.stream, c);
          Palette(i).Green:= Primary_color_range(c);
          U8'Read(image.stream, c);
          Palette(i).Red:= Primary_color_range(c);
          U8'Read(image.stream, c);
          -- discarded
        when GIF =>
          -- order is RGB
          U8'Read(image.stream, c);
          Palette(i).Red:= Primary_color_range(c);
          U8'Read(image.stream, c);
          Palette(i).Green:= Primary_color_range(c);
          U8'Read(image.stream, c);
          Palette(i).Blue:= Primary_color_range(c);
        when others =>
          null; -- !!
      end case;
    end loop;
  end Load_palette;

end GID.Color_tables;
