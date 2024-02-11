--
--  Convert any image or animation file to BMP file(s).
--
--  Middle-size test/demo for the GID (Generic Image Decoder) package.
--
--  Supports:
--
--  - Transparency: the program blends transparent or partially opaque
--    areas with a background image, `gid.gif`, or a fixed,
--    predefined colour.
--  - Display orientation (JPEG EXIF informations from digital
--    cameras): the result is rotated accordingly.
--
--  For a smaller and simpler example, look for `mini.adb` .
--

with GID;

with Ada.Calendar,
     Ada.Characters.Handling,
     Ada.Command_Line,
     Ada.Streams.Stream_IO,
     Ada.Strings.Fixed,
     Ada.Strings.Unbounded,
     Ada.Text_IO,
     Ada.Unchecked_Deallocation;

with Interfaces;

procedure To_BMP is

  default_bkg_name : constant String := "gid.gif";

  use Ada.Characters.Handling, Ada.Streams.Stream_IO, Ada.Strings.Unbounded, Ada.Text_IO;

  procedure Blurb is
  begin
    Put_Line (Standard_Error, "To_BMP * Converts any image file to a BMP file");
    Put_Line (Standard_Error, "Simple test for the GID (Generic Image Decoder) package");
    Put_Line (Standard_Error, "Package version " & GID.version & " dated " & GID.reference);
    Put_Line (Standard_Error, "URL: " & GID.web);
    New_Line (Standard_Error);
    Put_Line (Standard_Error, "Syntax:");
    Put_Line (Standard_Error, "to_bmp [-] [-<background_image_name>] <image_1> [<image_2>...]");
    New_Line (Standard_Error);
    Put_Line (Standard_Error, "Options:");
    Put_Line (Standard_Error, "  '-': don't output image (testing only)");
    Put_Line (Standard_Error, "  '-<background_image_name>':");
    Put_Line (Standard_Error, "      use specifed background to mix with transparent images");
    Put_Line (Standard_Error, "      (otherwise, trying with '" & default_bkg_name & "' or single color)");
    New_Line (Standard_Error);
    Put_Line (Standard_Error, "Output: "".dib"" is added the full input name(s)");
    Put_Line (Standard_Error, "  Reason of the "".dib"" extension: it is an unknown synonym of "".bmp"";");
    Put_Line (Standard_Error, "  just do ""del *.dib"" for cleanup");
    New_Line (Standard_Error);
  end Blurb;

  --  Image used as background for displaying images having transparency
  background_image_name : Unbounded_String := Null_Unbounded_String;

  use Interfaces;

  type Byte_Array is array (Integer range <>) of Unsigned_8;
  type p_Byte_Array is access Byte_Array;
  procedure Dispose is new Ada.Unchecked_Deallocation (Byte_Array, p_Byte_Array);

  forgive_errors : constant Boolean := False;
  in_error : Boolean;

  img_buf, bkg_buf : p_Byte_Array := null;
  bkg : GID.Image_Descriptor;

  force_allocate : constant := -1;
  mem_buffer_last : Integer := force_allocate;

  generic
    correct_orientation : GID.Orientation;
  --  Load image into a 24-bit truecolor BGR raw bitmap (for a BMP output)
  procedure Load_raw_image
    (image      : in out GID.Image_Descriptor;
     buffer     : in out p_Byte_Array;
     next_frame :    out Ada.Calendar.Day_Duration);
  --
  procedure Load_raw_image
    (image      : in out GID.Image_Descriptor;
     buffer     : in out p_Byte_Array;
     next_frame :    out Ada.Calendar.Day_Duration)
  is
    subtype Primary_color_range is Unsigned_8;
    subtype U16 is Unsigned_16;
    image_width  : constant Positive := GID.Pixel_Width (image);
    image_height : constant Positive := GID.Pixel_Height (image);
    padded_line_size_x : constant Positive :=
      4 * Integer (Float'Ceiling (Float (image_width) * 3.0 / 4.0));
    padded_line_size_y : constant Positive :=
      4 * Integer (Float'Ceiling (Float (image_height) * 3.0 / 4.0));
    --  (in bytes)
    idx : Integer;
    mem_x, mem_y : Natural;
    bkg_padded_line_size : Positive;
    bkg_width, bkg_height : Natural;
    --
    procedure Set_X_Y (x, y : Natural) is
    pragma Inline (Set_X_Y);
      use GID;
      rev_x : constant Natural := image_width - (x + 1);
      rev_y : constant Natural := image_height - (y + 1);
    begin
      case correct_orientation is
        when Unchanged =>
          idx := 3 * x + padded_line_size_x * y;
        when Rotation_90 =>
          idx := 3 * rev_y + padded_line_size_y * x;
        when Rotation_180 =>
          idx := 3 * rev_x + padded_line_size_x * rev_y;
        when Rotation_270 =>
          idx := 3 * y + padded_line_size_y * rev_x;
      end case;
      mem_x := x;
      mem_y := y;
    end Set_X_Y;
    --
    --  No background version of Put_Pixel
    --
    procedure Put_Pixel_without_bkg (
      red, green, blue : Primary_color_range;
      alpha            : Primary_color_range
    )
    is
    pragma Inline (Put_Pixel_without_bkg);
    pragma Unreferenced (alpha);
      use GID;
    begin
      buffer (idx .. idx + 2) := (blue, green, red);
      --  GID requires us to look to next pixel for next time:
      case correct_orientation is
        when Unchanged =>
          idx := idx + 3;
        when Rotation_90 =>
          idx := idx + padded_line_size_y;
        when Rotation_180 =>
          idx := idx - 3;
        when Rotation_270 =>
          idx := idx - padded_line_size_y;
      end case;
    end Put_Pixel_without_bkg;
    --
    --  Unicolor background version of Put_Pixel
    --
    procedure Put_Pixel_with_unicolor_bkg (
      red, green, blue : Primary_color_range;
      alpha            : Primary_color_range
    )
    is
    pragma Inline (Put_Pixel_with_unicolor_bkg);
      u_red  : constant := 200;
      u_green : constant := 133;
      u_blue : constant := 32;
    begin
      if alpha = 255 then
        buffer (idx .. idx + 2) := (blue, green, red);
      else  --  Blend with background color
        buffer (idx)  := Primary_color_range ((U16 (alpha) * U16 (blue)  + U16 (255 - alpha) * u_blue) / 255);
        buffer (idx + 1) := Primary_color_range ((U16 (alpha) * U16 (green) + U16 (255 - alpha) * u_green) / 255);
        buffer (idx + 2) := Primary_color_range ((U16 (alpha) * U16 (red)   + U16 (255 - alpha) * u_red) / 255);
      end if;
      idx := idx + 3;
      --  ^ GID requires us to look to next pixel on the right for next time.
    end Put_Pixel_with_unicolor_bkg;
    --
    --  Background image version of Put_Pixel
    --
    procedure Put_Pixel_with_image_bkg (
      red, green, blue : Primary_color_range;
      alpha            : Primary_color_range
    )
    is
    pragma Inline (Put_Pixel_with_image_bkg);
      b_red,
      b_green,
      b_blue : Primary_color_range;
      bkg_idx : Natural;
    begin
      if alpha = 255 then
        buffer (idx .. idx + 2) := (blue, green, red);
      else  --  Blend with background image
        bkg_idx := 3 * (mem_x mod bkg_width) + bkg_padded_line_size * (mem_y mod bkg_height);
        b_blue := bkg_buf (bkg_idx);
        b_green := bkg_buf (bkg_idx + 1);
        b_red  := bkg_buf (bkg_idx + 2);
        buffer (idx)  := Primary_color_range ((U16 (alpha) * U16 (blue)  + U16 (255 - alpha) * U16 (b_blue)) / 255);
        buffer (idx + 1) := Primary_color_range ((U16 (alpha) * U16 (green) + U16 (255 - alpha) * U16 (b_green)) / 255);
        buffer (idx + 2) := Primary_color_range ((U16 (alpha) * U16 (red)   + U16 (255 - alpha) * U16 (b_red)) / 255);
      end if;
      idx := idx + 3;
      --  ^ GID requires us to look to next pixel on the right for next time.
      mem_x := mem_x + 1;
    end Put_Pixel_with_image_bkg;

    stars : Natural := 0;
    procedure Feedback (percents : Natural) is
      so_far : constant Natural := percents / 5;
    begin
      for i in stars + 1 .. so_far loop
        Put (Standard_Error, '*');
      end loop;
      stars := so_far;
    end Feedback;

    --  Here, the exciting thing: the instanciation of
    --  GID.Load_image_contents. In our case, we load the image
    --  into a 24-bit bitmap (because we provide a Put_Pixel
    --  that does that with the pixels), but we could do plenty
    --  of other things instead, like display the image live on a GUI.

    --  More exciting: for tuning performance, we have 3 different
    --  instances of GID.Load_image_contents (each of them with the full
    --  decoders for all formats, own specialized generic instances, inlines,
    --  etc.) depending on the transparency features.

    procedure BMP24_Load_without_bkg is
      new GID.Load_Image_Contents
        (Primary_color_range,
         Set_X_Y,
         Put_Pixel_without_bkg,
         Feedback,
         GID.fast);

    procedure BMP24_Load_with_unicolor_bkg is
      new GID.Load_Image_Contents
        (Primary_color_range,
         Set_X_Y,
         Put_Pixel_with_unicolor_bkg,
         Feedback,
         GID.fast);

    procedure BMP24_Load_with_image_bkg is
      new GID.Load_Image_Contents
        (Primary_color_range,
         Set_X_Y,
         Put_Pixel_with_image_bkg,
         Feedback,
         GID.fast);

    buffer_last : Natural;

  begin
    in_error := False;
    case correct_orientation is
      when GID.Unchanged | GID.Rotation_180 =>
        buffer_last := padded_line_size_x * image_height - 1;
      when GID.Rotation_90 | GID.Rotation_270 =>
        buffer_last := padded_line_size_y * image_width - 1;
    end case;
    if buffer_last > mem_buffer_last then
      Dispose (buffer);
      buffer := new Byte_Array (0 .. buffer_last);
      mem_buffer_last := buffer_last;
    end if;
    if GID.Expect_Transparency (image) then
      if background_image_name = Null_Unbounded_String then
        BMP24_Load_with_unicolor_bkg (image, next_frame);
      else
        bkg_width  := GID.Pixel_Width (bkg);
        bkg_height := GID.Pixel_Height (bkg);
        bkg_padded_line_size :=
          4 * Integer (Float'Ceiling (Float (bkg_width) * 3.0 / 4.0));
        BMP24_Load_with_image_bkg (image, next_frame);
      end if;
    else
      BMP24_Load_without_bkg (image, next_frame);
    end if;
    --  -- For testing: white rectangle with a red half-frame.
    --  buffer.all:= (others => 255);
    --  for x in 0..GID.Pixel_width(image)-1 loop
    --    Put_Pixel_with_unicolor_bkg(x,0,255,0,0,255);
    --  end loop;
    --  for y in 0..GID.Pixel_height(image)-1 loop
    --    Put_Pixel_with_unicolor_bkg(0,y,255,0,0,255);
    --  end loop;
  exception
    when others =>
      if forgive_errors then
        in_error := True;
        next_frame := 0.0;
      else
        raise;
      end if;
  end Load_raw_image;

  procedure Load_raw_image_0 is new Load_raw_image (GID.Unchanged);
  procedure Load_raw_image_90 is new Load_raw_image (GID.Rotation_90);
  procedure Load_raw_image_180 is new Load_raw_image (GID.Rotation_180);
  procedure Load_raw_image_270 is new Load_raw_image (GID.Rotation_270);

  procedure Dump_BMP_24 (name : String; i : GID.Image_Descriptor) is
    f : Ada.Streams.Stream_IO.File_Type;
    type BITMAPFILEHEADER is record
      bfType     : Unsigned_16;
      bfSize     : Unsigned_32;
      bfReserved1 : Unsigned_16 := 0;
      bfReserved2 : Unsigned_16 := 0;
      bfOffBits  : Unsigned_32;
    end record;
    --  ^ No packing needed
    BITMAPFILEHEADER_Bytes : constant := 14;

    type BITMAPINFOHEADER is record
      biSize         : Unsigned_32;
      biWidth        : Unsigned_32;
      biHeight       : Unsigned_32;
      biPlanes       : Unsigned_16 := 1;
      biBitCount     : Unsigned_16;
      biCompression  : Unsigned_32 := 0;
      biSizeImage    : Unsigned_32;
      biXPelsPerMeter : Unsigned_32 := 0;
      biYPelsPerMeter : Unsigned_32 := 0;
      biClrUsed      : Unsigned_32 := 0;
      biClrImportant : Unsigned_32 := 0;
    end record;
    --  ^ No packing needed
    BITMAPINFOHEADER_Bytes : constant := 40;

    FileInfo  : BITMAPINFOHEADER;
    FileHeader : BITMAPFILEHEADER;
    --
    generic
      type Number is mod <>;
    procedure Write_Intel_x86_number (n : in Number);

    procedure Write_Intel_x86_number (n : in Number) is
      m : Number := n;
      bytes : constant Integer := Number'Size / 8;
    begin
      for i in 1 .. bytes loop
        Unsigned_8'Write (Stream (f), Unsigned_8 (m and 255));
        m := m / 256;
      end loop;
    end Write_Intel_x86_number;
    procedure Write_Intel is new Write_Intel_x86_number (Unsigned_16);
    procedure Write_Intel is new Write_Intel_x86_number (Unsigned_32);
  begin
    FileHeader.bfType := 16#4D42#; -- 'BM'
    FileHeader.bfOffBits := BITMAPINFOHEADER_Bytes + BITMAPFILEHEADER_Bytes;
    FileInfo.biSize      := BITMAPINFOHEADER_Bytes;
    case GID.Display_Orientation (i) is
      when GID.Unchanged | GID.Rotation_180 =>
        FileInfo.biWidth  := Unsigned_32 (GID.Pixel_Width (i));
        FileInfo.biHeight := Unsigned_32 (GID.Pixel_Height (i));
      when GID.Rotation_90 | GID.Rotation_270 =>
        FileInfo.biWidth  := Unsigned_32 (GID.Pixel_Height (i));
        FileInfo.biHeight := Unsigned_32 (GID.Pixel_Width (i));
    end case;
    FileInfo.biBitCount  := 24;
    FileInfo.biSizeImage := Unsigned_32 (img_buf.all'Length);

    FileHeader.bfSize := FileHeader.bfOffBits + FileInfo.biSizeImage;

    Create (f, Out_File, name & ".dib");
    --  BMP Header, endian-safe:
    Write_Intel (FileHeader.bfType);
    Write_Intel (FileHeader.bfSize);
    Write_Intel (FileHeader.bfReserved1);
    Write_Intel (FileHeader.bfReserved2);
    Write_Intel (FileHeader.bfOffBits);
    --
    Write_Intel (FileInfo.biSize);
    Write_Intel (FileInfo.biWidth);
    Write_Intel (FileInfo.biHeight);
    Write_Intel (FileInfo.biPlanes);
    Write_Intel (FileInfo.biBitCount);
    Write_Intel (FileInfo.biCompression);
    Write_Intel (FileInfo.biSizeImage);
    Write_Intel (FileInfo.biXPelsPerMeter);
    Write_Intel (FileInfo.biYPelsPerMeter);
    Write_Intel (FileInfo.biClrUsed);
    Write_Intel (FileInfo.biClrImportant);
    --  BMP raw BGR image:
    declare
      --  Workaround for the severe xxx'Read xxx'Write performance
      --  problems in the GNAT and ObjectAda compilers (as in 2009)
      --  This is possible if and only if Byte = Stream_Element and
      --  arrays types are both packed the same way.
      --
      subtype Size_test_a is Byte_Array (1 .. 19);
      subtype Size_test_b is Ada.Streams.Stream_Element_Array (1 .. 19);
      workaround_possible : constant Boolean :=
        Size_test_a'Size = Size_test_b'Size and then
        Size_test_a'Alignment = Size_test_b'Alignment;
      --
    begin
      if workaround_possible then
        declare
          use Ada.Streams;
          SE_Buffer   : Stream_Element_Array (0 .. Stream_Element_Offset (img_buf'Length - 1));
          for SE_Buffer'Address use img_buf.all'Address;
          pragma Import (Ada, SE_Buffer);
        begin
          Ada.Streams.Write (Stream (f).all, SE_Buffer (0 .. Stream_Element_Offset (img_buf'Length - 1)));
        end;
      else
        Byte_Array'Write (Stream (f), img_buf.all); -- the workaround is about this line...
      end if;
    end;
    Close (f);
  end Dump_BMP_24;

  procedure Process (name : String; as_background, test_only : Boolean) is
    f : Ada.Streams.Stream_IO.File_Type;
    i : GID.Image_Descriptor;
    up_name : constant String := To_Upper (name);
    --
    next_frame, current_frame : Ada.Calendar.Day_Duration := 0.0;
    use Ada.Calendar, Ada.Strings, Ada.Strings.Fixed;
    use type GID.Image_Format_Type;
    T0, T1 : Time;
  begin
    --
    --  Load the image in its original format
    --
    Open (f, In_File, name);
    Put_Line (Standard_Error, "Processing " & name & "...");
    --
    GID.Load_Image_Header
      (i,
       Stream (f).all,
       try_tga =>
         name'Length >= 4 and then
         up_name (up_name'Last - 3 .. up_name'Last) = ".TGA");
    --
    Put_Line
      (Standard_Error,
       "  Image format: " & GID.Format (i)'Image);
    Put_Line
      (Standard_Error,
       "  Image detailed format: " & GID.Detailed_format (i));
    Put_Line
      (Standard_Error,
       "  Image sub-format ID (if any):" & GID.Subformat (i)'Image);
    Put_Line
      (Standard_Error,
       "  Dimensions in pixels:" &
       GID.Pixel_Width (i)'Image & " x" &
       GID.Pixel_Height (i)'Image);
    Put_Line
      (Standard_Error,
       "  Display orientation: " & GID.Display_Orientation (i)'Image);
    Put
      (Standard_Error,
       "  Color depth:" & GID.Bits_per_Pixel (i)'Image & " bits");

    if GID.Bits_per_Pixel (i) <= 24 then
      Put_Line
        (Standard_Error,
         ',' & Integer'Image (2**GID.Bits_per_Pixel (i)) & " colors");
    else
      New_Line (Standard_Error);
    end if;

    Put_Line
      (Standard_Error,
       "  Palette: " & GID.Has_Palette (i)'Image);
    Put_Line
      (Standard_Error,
       "  Greyscale: " & GID.Greyscale (i)'Image);
    Put_Line
      (Standard_Error,
       "  RLE encoding (if any): " & GID.Is_RLE_Encoded (i)'Image);
    Put_Line
      (Standard_Error,
       "  Expect transparency: " & GID.Expect_Transparency (i)'Image);

    if GID.Format (i) = GID.GIF then
      null;  --  Interlaced-or-not choice is per frame in the GIF format.
    else
      Put_Line
        (Standard_Error,
         "  Interlaced / Progressive: " & GID.Is_Progressive (i)'Image);
    end if;

    Put_Line (Standard_Error, "1........10........20");
    Put_Line (Standard_Error, "         |         | ");
    --

    T0 := Clock;

    if as_background then
      case GID.Display_Orientation (i) is
        when GID.Unchanged =>
          Load_raw_image_0 (i, bkg_buf, next_frame);
        when GID.Rotation_90 =>
          Load_raw_image_90 (i, bkg_buf, next_frame);
        when GID.Rotation_180 =>
          Load_raw_image_180 (i, bkg_buf, next_frame);
        when GID.Rotation_270 =>
          Load_raw_image_270 (i, bkg_buf, next_frame);
      end case;
      bkg := i;
      New_Line (Standard_Error);
    else
      mem_buffer_last := force_allocate;

      Animation_Loop :
      loop
        case GID.Display_Orientation (i) is
          when GID.Unchanged =>
            Load_raw_image_0 (i, img_buf, next_frame);
          when GID.Rotation_90 =>
            Load_raw_image_90 (i, img_buf, next_frame);
          when GID.Rotation_180 =>
            Load_raw_image_180 (i, img_buf, next_frame);
          when GID.Rotation_270 =>
            Load_raw_image_270 (i, img_buf, next_frame);
        end case;
        if not test_only then
          Dump_BMP_24 (name & '_' & Trim (current_frame'Image, Left), i);
        end if;
        New_Line (Standard_Error);
        if in_error then
          Put_Line (Standard_Error, "Error!");
        end if;
        exit Animation_Loop when next_frame = 0.0;
        current_frame := next_frame;
      end loop Animation_Loop;
    end if;

    T1 := Clock;

    Close (f);
    Put_Line
      (Standard_Error,
       "Time elapsed:" & Duration'Image (T1 - T0) & " seconds.");

  exception
    when GID.unknown_image_format =>
      Put_Line (Standard_Error, "  Image format is unknown!");
      if Is_Open (f) then
        Close (f);
      end if;
  end Process;

  test_only : Boolean := False;

  use Ada.Command_Line;

begin
  if Argument_Count = 0 then
    Blurb;
    return;
  end if;
  Put_Line (Standard_Error, "To_BMP, using GID version " & GID.version & " dated " & GID.reference);
  begin
    Process (default_bkg_name, True, False);
    --  if success:
    background_image_name := To_Unbounded_String (default_bkg_name);
  exception
    when Ada.Text_IO.Name_Error =>
      null; -- nothing bad, just couldn't find default background
  end;
  for i in 1 .. Argument_Count loop
    declare
      arg : constant String := Argument (i);
    begin
      if arg /= "" and then arg (arg'First) = '-' then
        declare
          opt : constant String := arg (arg'First + 1 .. arg'Last);
        begin
          if opt = "" then
            test_only := True;
          else
            Put_Line (Standard_Error, "Background image is " & opt);
            Process (opt, True, False);
            --  define this only after processing, otherwise
            --  a transparent background will try to use
            --  an undefined background
            background_image_name := To_Unbounded_String (opt);
          end if;
        end;
      else
        Process (arg, False, test_only);
      end if;
    end;
  end loop;
end To_BMP;
