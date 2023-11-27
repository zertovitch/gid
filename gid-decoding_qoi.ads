--  QOI - The "Quite OK Image" format for fast, lossless image compression
--
--  https://github.com/phoboslab/qoi
--
--  Dominic Szablewski - https://phoboslab.org

private package GID.Decoding_QOI is

  --------------------
  -- Image decoding --
  --------------------

  generic
    type Primary_color_range is mod <>;
    with procedure Set_X_Y (x, y : Natural);
    with procedure Put_Pixel (
      red, green, blue : Primary_color_range;
      alpha            : Primary_color_range
    );
    with procedure Feedback (percents : Natural);
  --
  procedure Load (image : in out Image_Descriptor);

end GID.Decoding_QOI;
