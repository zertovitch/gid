private package GID.Decoding_JPG is

  type JPEG_marker is
  (
    SOI      , --  Start Of Image
    --
    SOF_0    , --  Start Of Frame - Baseline DCT
    SOF_1    , --  Extended sequential DCT
    SOF_2    , --  Progressive DCT
    SOF_3    , --  Lossless (sequential)
    SOF_5    , --  Differential sequential DCT
    SOF_6    , --  Differential progressive DCT
    SOF_7    , --  Differential lossless (sequential)
    SOF_8    , --  Reserved for JPEG extensions
    SOF_9    , --  Extended sequential DCT
    SOF_10   , --  Progressive DCT
    SOF_11   , --  Lossless (sequential)
    SOF_13   , --  Differential sequential DCT
    SOF_14   , --  Differential progressive DCT
    SOF_15   , --  Differential lossless (sequential)
    --
    DHT      , --  Define Huffman Table
    DAC      , --  Define Arithmetic Coding
    DQT      , --  Define Quantization Table
    --
    APP0     , --  JFIF - JFIF JPEG image - AVI1 - Motion JPEG (MJPG)
    APP1     , --  EXIF Metadata, TIFF IFD format, JPEG Thumbnail (160x120)
    APP2     , --  ICC color profile, FlashPix
    APP13    , --  Photoshop Save As: IRB, 8BIM, IPTC
    APP14    , --  Copyright Entries
    --
    COM      , --  Comments
    SOS      , --  Start of Scan
    EOI        --  End of Image
  );

  type Segment_head is record
    length : U16;
    kind   : JPEG_marker;
  end record;

  procedure Read( image: in out image_descriptor; sh: out Segment_head);

  --------------------
  -- Image decoding --
  --------------------

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
  --
  procedure Load (
    image     : in out Image_descriptor;
    next_frame:    out Ada.Calendar.Day_Duration
  );

private

  -- Primitive tracing using Ada.Text_IO, for debugging.
  --
  type Trace_type is (none, some, full);

  trace: constant Trace_type:= full; -- <== Choice here

  no_trace  : constant Boolean:= trace=none;
  full_trace: constant Boolean:= trace=full;
  some_trace: constant Boolean:= trace>=some;

end GID.Decoding_JPG;
