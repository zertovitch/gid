--
-- Convert any image file to BMP file
--
-- Simple test for the GID (Generic Image Decoder) package
--

with GID;

with Ada.Calendar;
with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Interfaces;

procedure To_BMP is

  procedure Blurb is
  begin
    Put_Line(Standard_Error, "To_BMP * Converts any image file to a BMP file");
    Put_Line(Standard_Error, "Simple test for the GID (Generic Image Decoder) package");
    Put_Line(Standard_Error, "Package version " & GID.version & " dated " & GID.reference);
    Put_Line(Standard_Error, "URL: " & GID.web);
    New_Line(Standard_Error);
    Put_Line(Standard_Error, "Syntax:");
    Put_Line(Standard_Error, "to_bmp [-] [-<background_image_name>] <image_1> [<image_2>...]");
    New_Line(Standard_Error);
    Put_Line(Standard_Error, "Option: '-': don't output image (testing only)");
    New_Line(Standard_Error);
    Put_Line(Standard_Error, "Output: "".dib"" is added the full input name(s)");
    Put_Line(Standard_Error, "  Reason of "".dib"": unknown synonym of "".bmp"";");
    Put_Line(Standard_Error, "  just do ""del *.dib"" for cleanup");
    New_Line(Standard_Error);
  end Blurb;

  -- Image used as background for displaying images having transparency
  background_image_name: Unbounded_String:= Null_Unbounded_String;

  use Interfaces;

  type Byte_Buffer is array(Integer range <>) of Unsigned_8;
  type p_Byte_Buffer is access Byte_Buffer;
  procedure Dispose is new Ada.Unchecked_Deallocation(Byte_Buffer, p_Byte_Buffer);

  img_buf, bkg_buf: p_Byte_Buffer:= null;

  -- Load image into a 24-bit truecolor raw bitmap
  procedure Load_raw_image(
    image : in     GID.Image_descriptor;
    buffer: in out p_Byte_Buffer
  )
  is
    image_width: constant Positive:= GID.Pixel_Width(image);
    padded_line_size: constant Positive:=
      4 * Integer(Float'Ceiling(Float(image_width) * 3.0 / 4.0));
    -- (in bytes)
    type Opacity_range is range 0..255;
    --
    -- White background version
    --
    procedure Put_Pixel_with_white_bkg (
      x, y             : Natural;
      red, green, blue : Natural;
      alpha            : Opacity_range
    )
    is
    pragma Inline(Put_Pixel_with_white_bkg);
      idx: Natural;
    begin
      idx:= 3 * x + padded_line_size * y;
      if alpha = Opacity_range'Last then
        buffer(idx)  := Unsigned_8(blue);
        buffer(idx+1):= Unsigned_8(green);
        buffer(idx+2):= Unsigned_8(red);
      else
        -- !! blending TBD
        buffer(idx)  := Unsigned_8(blue);
        buffer(idx+1):= Unsigned_8(green);
        buffer(idx+2):= Unsigned_8(red);
      end if;
    end Put_Pixel_with_white_bkg;
    -- Here, the exciting thing: the instanciation of
    -- GID.Load_image_contents. We load here the image
    -- into a 24-bit bitmap (because we provide a Put_Pixel
    -- that does that with the pixels), but we could do plenty
    -- of other things instead, like display the image on a GUI.
    procedure BMP24_Load_with_white_bkg is
      new GID.Load_image_contents(
        GID.bits_8_mode,
        Opacity_range,
        Put_Pixel_with_white_bkg
      );
    next_frame_dummy: Ada.Calendar.Day_Duration;
  begin
    Dispose(buffer);
    buffer:= new Byte_Buffer(0..padded_line_size * GID.Pixel_height(image) - 1);

    if background_image_name = Null_Unbounded_String then
      BMP24_Load_with_white_bkg(image, next_frame_dummy);
    else
      BMP24_Load_with_white_bkg(image, next_frame_dummy);
      -- with bkg image TBD !!
    end if;
    --  -- Test: white rectangle with a red half-frame.
    --  buffer.all:= (others => 255);
    --  for x in 0..GID.Pixel_width(image)-1 loop
    --    Put_Pixel_with_white_bkg(x,0,255,0,0,255);
    --  end loop;
    --  for y in 0..GID.Pixel_height(image)-1 loop
    --    Put_Pixel_with_white_bkg(0,y,255,0,0,255);
    --  end loop;
  end Load_raw_image;

  procedure Process(name: String; as_background, test_only: Boolean) is
    f: Ada.Streams.Stream_IO.File_Type;
    i: GID.Image_descriptor;
    --
    generic
      type Number is mod <>;
    procedure Write_Intel_x86_number(n: in Number);

    procedure Write_Intel_x86_number(n: in Number) is
      m: Number:= n;
      bytes: constant Integer:= Number'Size/8;
    begin
      for i in 1..bytes loop
        Unsigned_8'Write(Stream(f), Unsigned_8(m and 255));
        m:= m / 256;
      end loop;
    end Write_Intel_x86_number;
    procedure Write_Intel is new Write_Intel_x86_number( Unsigned_16 );
    procedure Write_Intel is new Write_Intel_x86_number( Unsigned_32 );
    --
    type BITMAPFILEHEADER is record
      bfType     : Unsigned_16;
      bfSize     : Unsigned_32;
      bfReserved1: Unsigned_16;
      bfReserved2: Unsigned_16;
      bfOffBits  : Unsigned_32;
    end record;
    -- No packing needed
    BITMAPFILEHEADER_Bytes: constant:= 14;

    type BITMAPINFOHEADER is record
      biSize         : Unsigned_32;
      biWidth        : Unsigned_32;
      biHeight       : Unsigned_32;
      biPlanes       : Unsigned_16;
      biBitCount     : Unsigned_16;
      biCompression  : Unsigned_32;
      biSizeImage    : Unsigned_32;
      biXPelsPerMeter: Unsigned_32;
      biYPelsPerMeter: Unsigned_32;
      biClrUsed      : Unsigned_32;
      biClrImportant : Unsigned_32;
    end record;
    -- No packing needed
    BITMAPINFOHEADER_Bytes: constant:= 40;

    FileInfo  : BITMAPINFOHEADER;
    FileHeader: BITMAPFILEHEADER;
  begin
    --
    -- Load the image in its original format
    --
    Open(f, In_File, name);
    Put_Line(Standard_Error, "Processing " & name & "...");
    begin
      GID.Load_image_header(i, Stream(f).all, try_tga => True);
      Put_Line(Standard_Error,
        "  Image format: " & GID.Image_format_type'Image(GID.Image_format(i))
      );
      Put_Line(Standard_Error,
        "  Image detailed format: " & GID.Image_detailed_format(i)
      );
      Put_Line(Standard_Error,
        "  Dimensions in pixels: " &
        Integer'Image(GID.Pixel_Width(i)) & " x" &
        Integer'Image(GID.Pixel_Height(i))
      );
      Put_Line(Standard_Error,
        "  Color depth: " &
        Integer'Image(GID.Bits_per_pixel(i)) & " bits," &
        Integer'Image(2**GID.Bits_per_pixel(i)) & " colors"
      );
      --
      if as_background then
        Load_raw_image(i, bkg_buf);
      else
        Load_raw_image(i, img_buf);
      end if;
      Close(f);
      if as_background or test_only then
        return;
      end if;
    exception
      when GID.unknown_image_format =>
        Put_Line(Standard_Error, "  Image format is unknown!");
		return;
    end;
    --
    -- Now, save the image as BMP 24-bit (code from GL.IO...)
    --
    FileHeader.bfType := 16#4D42#; -- 'BM'
    FileHeader.bfOffBits := BITMAPINFOHEADER_Bytes + BITMAPFILEHEADER_Bytes;
    FileHeader.bfReserved1:= 0;
    FileHeader.bfReserved2:= 0;

    FileInfo.biSize       := BITMAPINFOHEADER_Bytes;
    FileInfo.biWidth      := Unsigned_32(GID.Pixel_width(i));
    FileInfo.biHeight     := Unsigned_32(GID.Pixel_height(i));
    FileInfo.biPlanes     := 1;
    FileInfo.biBitCount   := 24;
    FileInfo.biCompression:= 0;
    FileInfo.biSizeImage  := Unsigned_32(img_buf.all'Length);
    FileInfo.biXPelsPerMeter:= 0;
    FileInfo.biYPelsPerMeter:= 0;
    FileInfo.biClrUsed      := 0;
    FileInfo.biClrImportant := 0;

    FileHeader.bfSize := FileHeader.bfOffBits + FileInfo.biSizeImage;

    Create(f, Out_File, name & ".dib");
    -- BMP Header, endian-safe:
    Write_Intel(FileHeader.bfType);
    Write_Intel(FileHeader.bfSize);
    Write_Intel(FileHeader.bfReserved1);
    Write_Intel(FileHeader.bfReserved2);
    Write_Intel(FileHeader.bfOffBits);
    --
    Write_Intel(FileInfo.biSize);
    Write_Intel(FileInfo.biWidth);
    Write_Intel(FileInfo.biHeight);
    Write_Intel(FileInfo.biPlanes);
    Write_Intel(FileInfo.biBitCount);
    Write_Intel(FileInfo.biCompression);
    Write_Intel(FileInfo.biSizeImage);
    Write_Intel(FileInfo.biXPelsPerMeter);
    Write_Intel(FileInfo.biYPelsPerMeter);
    Write_Intel(FileInfo.biClrUsed);
    Write_Intel(FileInfo.biClrImportant);
    -- BMP raw BGR image:
    Byte_Buffer'Write(Stream(f), img_buf.all);
    Close(f);
  end Process;

  test_only: Boolean:= False;

begin
  if Argument_Count=0 then
    Blurb;
    return;
  end if;
  for i in 1..Argument_Count loop
    declare
      arg: constant String:= Argument(i);
    begin
      if arg /= "" and then arg(arg'First)='-' then
        declare
          opt: constant String:= arg(arg'First+1..arg'Last);
        begin
          if opt = "" then
            test_only:= True;
          else
            background_image_name:= To_Unbounded_String(opt);
            Put_Line(Standard_Error, "Background image is " & To_String(background_image_name));
            Process(arg, True, False);
          end if;
        end;
      else
        Process(arg, False, test_only);
      end if;
    end;
  end loop;
end To_BMP;
