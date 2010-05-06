private package GID.Decoding_BMP is

  generic
    primary_color_coding_2: Primary_color_mode;
    type Opacity_range_2 is range <>;
    with procedure Put_Pixel_2 (
      x, y             : Natural;
      red, green, blue : Natural;
      alpha            : Opacity_range_2
    );
    pragma Inline(Put_Pixel_2);
  procedure Load (image: in Image_descriptor);

end GID.Decoding_BMP;
