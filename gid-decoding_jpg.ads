private package GID.Decoding_JPG is

  use JPEG_Defs;

  type JPEG_Marker is
    (SOI,  --  Start Of Image
     --
     SOF_0,   --  Start Of Frame - Baseline DCT
     SOF_1,   --  Extended sequential DCT
     SOF_2,   --  Progressive DCT
     SOF_3,   --  Lossless (sequential)
     SOF_5,   --  Differential sequential DCT
     SOF_6,   --  Differential progressive DCT
     SOF_7,   --  Differential lossless (sequential)
     SOF_8,   --  Reserved for JPEG extensions
     SOF_9,   --  Extended sequential DCT
     SOF_10,  --  Progressive DCT
     SOF_11,  --  Lossless (sequential)
     SOF_13,  --  Differential sequential DCT
     SOF_14,  --  Differential progressive DCT
     SOF_15,  --  Differential lossless (sequential)
     --
     DHT,  --  Define Huffman Table
     DAC,  --  Define Arithmetic Coding
     DQT,  --  Define Quantization Table
     DRI,  --  Define Restart Interval
     --
     RST_0,  --  Restart
     RST_1,
     RST_2,
     RST_3,
     RST_4,
     RST_5,
     RST_6,
     RST_7,
     --
     APP_0,  --  JFIF - JFIF JPEG image - AVI1 - Motion JPEG (MJPG)
     APP_1,  --  EXIF Metadata, TIFF IFD format, JPEG Thumbnail (160x120)
     APP_2,  --  ICC color profile, FlashPix
     APP_3,
     APP_4,
     APP_5,
     APP_6,
     APP_7,
     APP_8,
     APP_9,
     APP_10,
     APP_11,
     APP_12,  --  Picture Info
     APP_13,  --  Photoshop Save As: IRB, 8BIM, IPTC
     APP_14,  --  Copyright Entries
     --
     COM,   --  Comments
     SOS,   --  Start of Scan
     EOI);  --  End of Image

  YCbCr_set  : constant Compo_Set_Type := (Y | Cb | Cr => True, others => False);
  Y_Grey_set : constant Compo_Set_Type := (Y => True, others => False);
  CMYK_set   : constant Compo_Set_Type := (Y | Cb | Cr | I => True, others => False);

  type Segment_Head is record
    length : U16;
    kind   : JPEG_Marker;
  end record;

  procedure Read
    (image           : in out Image_Descriptor;
     known_marker    : in     Boolean;
     buffered_marker : in     U8;
     head            :    out Segment_Head);

  procedure Skip_Segment_Data
    (image : in out Image_Descriptor;
     head  : in     Segment_Head);

  --  SOF - Start Of Frame (the real header)
  procedure Read_SOF (image : in out Image_Descriptor; sh : Segment_Head);

  procedure Read_DHT (image : in out Image_Descriptor; data_length : Natural);
  procedure Read_DQT (image : in out Image_Descriptor; data_length : Natural);
  procedure Read_DRI (image : in out Image_Descriptor);

  procedure Read_EXIF (image : in out Image_Descriptor; data_length : Natural);

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
    --  mode: Display_mode; -- nice -> progressive nicely displayed
  --
  procedure Load (image : in out Image_Descriptor);

end GID.Decoding_JPG;
