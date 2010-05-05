--
-- Color tables, known as "palettes"
--

private package GID.Color_tables is

  subtype Primary_color_range is Natural;
  -- We assume that Natural is large enough to contain
  -- all possible Primary_color_range in the generic parts of GID

  type RGB_Color is record
    red, green, blue : Primary_color_range;
  end record;

  type Color_table is array (Integer range <>) of RGB_Color;

  procedure Load_palette (
    image     : in     Image_descriptor;
    palette   :    out Color_table
  );

end GID.Color_tables;
