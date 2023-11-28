---------------------------------
-- GID - Generic Image Decoder --
---------------------------------
--
--  Private child of GID, with helpers for identifying
--  image formats and reading header informations.
--
private package GID.Headers is

  --
  --  Crude image signature detection.
  --  Targa (.tga) has no signature.
  --
  procedure Load_Signature
    (image   : in out Image_Descriptor;
     try_tga :        Boolean          := False);

  --
  --  Loading of various format's headers (past signature)
  --

  procedure Load_BMP_Header  (image : in out Image_Descriptor);
  procedure Load_FITS_Header (image : in out Image_Descriptor);
  procedure Load_GIF_Header  (image : in out Image_Descriptor);
  procedure Load_JPEG_Header (image : in out Image_Descriptor);
  procedure Load_PNG_Header  (image : in out Image_Descriptor);
  procedure Load_PNM_Header  (image : in out Image_Descriptor);
  procedure Load_QOI_Header  (image : in out Image_Descriptor);
  procedure Load_TGA_Header  (image : in out Image_Descriptor);
  procedure Load_TIFF_Header (image : in out Image_Descriptor);

end GID.Headers;
