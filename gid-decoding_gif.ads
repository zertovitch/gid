-- !! transp
-- !! endiann.
-- !! buffering
-- !! anim. / 89a extensions
-- !! <256 col (palette, mask etc.)

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
    image     : in     Image_descriptor;
    next_frame: in out Ada.Calendar.Day_Duration
  );

private

  full_trace: constant Boolean:= True;

end GID.Decoding_GIF;