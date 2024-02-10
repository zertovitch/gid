private package GID.Decoding_GIF is

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
    mode : Display_Mode;
  --
  procedure Load
    (image      : in out Image_Descriptor;
     next_frame :    out Ada.Calendar.Day_Duration);

end GID.Decoding_GIF;
