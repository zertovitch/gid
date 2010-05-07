--
-- Color tables, known as "palettes"
--

private package GID.Color_tables is

  procedure Load_palette (image: in out Image_descriptor);
  -- if image.palette = null, nothing happens.

end GID.Color_tables;
