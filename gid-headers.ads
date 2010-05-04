---------------------------------
-- GID - Generic Image Decoder --
---------------------------------
--
-- Private child of GID, with helpers for identifying
-- image formats and reading header informations.
--
private package GID.Headers is

  --
  -- Crude image signature detection
  --
  procedure Load_signature (
    image   :    out Image_descriptor;
    from    : in out Ada.Streams.Root_Stream_Type'Class;
    try_tga :        Boolean:= False
  );


  --
  -- Loading of various format's headers
  --

  procedure Load_GIF_header (
    image   :    out Image_descriptor;
    from    : in out Ada.Streams.Root_Stream_Type'Class
  );

end;