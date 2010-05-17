-- !! buffering: funny thing: avoid buffering too much.
--    Perhaps, each sub-block and not fixed-size buffer.

private package GID.Decoding_GIF is

  generic
    type Primary_color_range is mod <>;
    with procedure Set_X_Y (x, y: Natural);
      pragma Inline(Set_X_Y);
    with procedure Put_Pixel (
      red, green, blue : Primary_color_range;
      alpha            : Primary_color_range
    );
      pragma Inline(Put_Pixel);
    with procedure Feedback (percents: Natural);
    mode: Display_mode;
  procedure Load (
    image     : in out Image_descriptor;
    next_frame:    out Ada.Calendar.Day_Duration
  );

private

  full_trace: constant Boolean:= True;

end GID.Decoding_GIF;
