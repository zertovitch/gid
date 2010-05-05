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
    Put_Line(Standard_Error, "to_bmp [-<background_image_name>] <image_1> [<image_2>...]");
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
    type Primary_color_range is range 0..255;
    type Opacity_range is range 0..255;
    --
    procedure Put_Pixel (
      x, y             : Natural;
      red, green, blue : Primary_color_range;
      alpha            : Opacity_range
    )
    is
    pragma Inline(Put_Pixel);
      idx: Natural;
    begin
      idx:= 3 * (x + image_width * y);
      buffer(idx)  := Unsigned_8(blue);
      buffer(idx+1):= Unsigned_8(green);
      buffer(idx+2):= Unsigned_8(red);
    end;
    -- Here, the exciting thing: the instanciation of
    -- GID.Load_image_contents. We load here the image
    -- into a 24-bit bitmap (because we provide a Put_Pixel
    -- that does that with the pixels), but we could do plenty
    -- of other things instead, like display the image on a GUI.
    procedure BMP24_Load is
      new GID.Load_image_contents(
        Primary_color_range,
        Opacity_range,
        Put_Pixel
      );
    next_frame_dummy: Ada.Calendar.Day_Duration;
  begin
    Dispose(buffer);
    buffer:= new Byte_Buffer(0..image_width * GID.Pixel_height(image) * 3 - 1);
    BMP24_Load(image, next_frame_dummy);
  end Load_raw_image;

  procedure Process(name: String; as_background: Boolean) is
    f: Ada.Streams.Stream_IO.File_Type;
    i: GID.Image_descriptor;
  begin
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
      if as_background then
        return;
      end if;
    exception
      when GID.unknown_image_format =>
        Put_Line(Standard_Error, "  Image format is unknown!");
		return;
    end;
      --
      -- Now, save the image as BMP 24-bit
      --
      declare
        -- BMP output code from GL.IO...
        type BITMAPFILEHEADER is record
          bfType     : Unsigned_16;
          bfSize     : Unsigned_32;
          bfReserved1: Unsigned_16;
          bfReserved2: Unsigned_16;
          bfOffBits  : Unsigned_32;
        end record;
        pragma Pack(BITMAPFILEHEADER);
        for BITMAPFILEHEADER'Size use 8 * 14;

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
        pragma Pack(BITMAPINFOHEADER);
        for BITMAPINFOHEADER'Size use 8 * 40;

        FileInfo  : BITMAPINFOHEADER;
        FileHeader: BITMAPFILEHEADER;
      begin
        FileHeader.bfType := 16#4D42#; -- 'BM'
        FileHeader.bfOffBits :=
          BITMAPINFOHEADER'Size / 8 +
          BITMAPFILEHEADER'Size / 8;
        FileHeader.bfReserved1:= 0;
        FileHeader.bfReserved2:= 0;

        FileInfo.biSize       := BITMAPINFOHEADER'Size / 8;
        FileInfo.biWidth      := Unsigned_32(GID.Pixel_width(i));
        FileInfo.biHeight     := Unsigned_32(GID.Pixel_height(i));
        FileInfo.biPlanes     := 1;
        FileInfo.biBitCount   := 24;
        FileInfo.biCompression:= 0;
        FileInfo.biSizeImage  :=
          Unsigned_32(FileInfo.biWidth * FileInfo.biHeight) *
          Unsigned_32(FileInfo.biBitCount / 8);
        FileInfo.biXPelsPerMeter:= 0;
        FileInfo.biYPelsPerMeter:= 0;
        FileInfo.biClrUsed      := 0;
        FileInfo.biClrImportant := 0;

        FileHeader.bfSize := FileHeader.bfOffBits + FileInfo.biSizeImage;

        Create(f, Out_File, name & ".dib");
        declare
          generic
            type Number is mod <>;
            s: Stream_Access;
          procedure Write_Intel_x86_number(n: in Number);

          procedure Write_Intel_x86_number(n: in Number) is
            m: Number:= n;
            bytes: constant Integer:= Number'Size/8;
          begin
            for i in 1..bytes loop
              Unsigned_8'Write(s, Unsigned_8(m and 255));
              m:= m / 256;
            end loop;
          end Write_Intel_x86_number;
          procedure Write_Intel is new Write_Intel_x86_number( Unsigned_16, Stream(f) );
          procedure Write_Intel is new Write_Intel_x86_number( Unsigned_32, Stream(f) );
        begin
          -- ** Endian-safe: ** --
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
          --
          Byte_Buffer'Write(Stream(f), img_buf.all);
          Close(f);
        end;
      end;
  end Process;

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
        background_image_name:= To_Unbounded_String(arg(arg'First+1..arg'Last));
        Put_Line(Standard_Error, "Background image is " & To_String(background_image_name));
        Process(arg, True);
      else
        Process(arg, False);
      end if;
    end;
  end loop;
end To_BMP;
