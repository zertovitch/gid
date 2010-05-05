private package GID.Decoding_BMP is

  generic
    type Primary_color_range_2 is range <>;
    type Opacity_range_2 is range <>;
    with procedure Put_Pixel_2 (
      x, y             : Natural;
      red, green, blue : Primary_color_range_2;
      alpha            : Opacity_range_2
    );
    pragma Inline(Put_Pixel_2);
  procedure Load (image: Image_descriptor);

end GID.Decoding_BMP;
