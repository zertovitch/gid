private package GID.Decoding_BMP is

  type BMP_Compression is
    (BI_RGB,         --  Uncompressed
     BI_RLE8,
     BI_RLE4,
     BI_BITFIELDS,   --  Uncompressed
     BI_JPEG,
     BI_PNG);

  --------------------
  -- Image decoding --
  --------------------

  generic
    type Primary_Color_Range is mod <>;
    with procedure Set_X_Y (x, y : Natural);
    with procedure Put_Pixel
      (red, green, blue : Primary_Color_Range;
       alpha            : Primary_Color_Range);
    with procedure Feedback (percents : Natural);
  --
  procedure Load (image : in out Image_Descriptor);

end GID.Decoding_BMP;
