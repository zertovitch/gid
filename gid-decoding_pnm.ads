--  Portable pixmap format (PPM)
--  Portable graymap format (PGM)
--  Portable bitmap format (PBM)

private package GID.Decoding_PNM is

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

  function Get_Token (
    stream      : Stream_Access;
    needs_EOL   : Boolean := False;
    single_char : Boolean := False
  )
  return String;

  function Get_Integer (
    stream      : Stream_Access;
    needs_EOL   : Boolean := False;
    single_char : Boolean := False
  )
  return Integer;

  function Get_Positive_32 (
    stream      : Stream_Access;
    needs_EOL   : Boolean := False;
    single_char : Boolean := False
  )
  return Positive_32;

end GID.Decoding_PNM;
