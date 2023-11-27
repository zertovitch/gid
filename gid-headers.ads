---------------------------------
-- GID - Generic Image Decoder --
---------------------------------
--
--  Private child of GID, with helpers for identifying
--  image formats and reading header informations.
--
private package GID.Headers is

  --
  --  Crude image signature detection
  --
  procedure Load_signature
    (image   : in out Image_Descriptor;
     try_tga :        Boolean          := False);

  --
  --  Loading of various format's headers (past signature)
  --

  procedure Load_BMP_header  (image : in out Image_Descriptor);
  procedure Load_FITS_header (image : in out Image_Descriptor);
  procedure Load_GIF_header  (image : in out Image_Descriptor);
  procedure Load_JPEG_header (image : in out Image_Descriptor);
  procedure Load_PNG_header  (image : in out Image_Descriptor);
  procedure Load_PNM_header  (image : in out Image_Descriptor);
  procedure Load_QOI_header  (image : in out Image_Descriptor);
  procedure Load_TGA_header  (image : in out Image_Descriptor);
  procedure Load_TIFF_header (image : in out Image_Descriptor);

end GID.Headers;
