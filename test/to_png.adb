--
--  Convert any image or animation file to PNG file(s).
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

with Dumb_PNG;

with Ada.Calendar,
     Ada.Characters.Handling,
     Ada.Command_Line,
     Ada.Streams.Stream_IO,
     Ada.Strings.Fixed,
     Ada.Strings.Unbounded,
     Ada.Text_IO;

with Interfaces;

procedure To_PNG is

  default_bkg_name : constant String := "gid.gif";

  use Ada.Characters.Handling, Ada.Streams.Stream_IO, Ada.Strings.Unbounded, Ada.Text_IO;

  procedure Blurb is
  begin
    Put_Line (Current_Error, "To_PNG * Converts any image file to a PNG file");
    Put_Line (Current_Error, "Simple test for the GID (Generic Image Decoder) package");
    Put_Line (Current_Error, "Package version " & GID.version & " dated " & GID.reference);
    Put_Line (Current_Error, "URL: " & GID.web);
    New_Line (Current_Error);
    Put_Line (Current_Error, "Syntax:");
    Put_Line (Current_Error, "to_png [-] [-<background_image_name>] <image_1> [<image_2>...]");
    New_Line (Current_Error);
    Put_Line (Current_Error, "Options:");
    Put_Line (Current_Error, "  '-': don't output image (testing only)");
    Put_Line (Current_Error, "  '-<background_image_name>':");
    Put_Line (Current_Error, "      use specifed background to mix with transparent images");
    Put_Line (Current_Error, "      (otherwise, trying with '" & default_bkg_name & "' or single color)");
    New_Line (Current_Error);
    Put_Line (Current_Error, "Output: "".gid.png"" is added the full input name(s)");
    New_Line (Current_Error);
  end Blurb;

  --  Image used as background for displaying images having transparency
  background_image_name : Unbounded_String := Null_Unbounded_String;

  use Interfaces;

  subtype Byte_Array is Dumb_PNG.Byte_Array;
  subtype p_Byte_Array is Dumb_PNG.p_Byte_Array;
  procedure Dispose (X : in out p_Byte_Array) renames Dumb_PNG.Dispose;

  forgive_errors : constant Boolean := False;
  in_error : Boolean;

  img_buf, bkg_buf : p_Byte_Array := null;
  bkg : GID.Image_Descriptor;

  force_allocate : constant := -1;
  mem_buffer_last : Integer := force_allocate;

  generic
    correct_orientation : GID.Orientation;
  --  Load image into a 24-bit truecolor RGB raw bitmap (for a PNG output)
  procedure Load_Raw_Image
    (image      : in out GID.Image_Descriptor;
     buffer     : in out p_Byte_Array;
     next_frame :    out Ada.Calendar.Day_Duration);
  --
  procedure Load_Raw_Image
    (image      : in out GID.Image_Descriptor;
     buffer     : in out p_Byte_Array;
     next_frame :    out Ada.Calendar.Day_Duration)
  is
    subtype Primary_color_range is Unsigned_8;
    subtype U16 is Unsigned_16;
    image_width  : constant Positive := GID.Pixel_Width (image);
    image_height : constant Positive := GID.Pixel_Height (image);
    padded_line_size_x : constant Positive := 1 + 3 * image_width;
    padded_line_size_y : constant Positive := 1 + 3 * image_height;
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
          idx := 1 + 3 * x     + padded_line_size_x * rev_y;
        when Rotation_90 =>
          idx := 1 + 3 * rev_y + padded_line_size_y * rev_x;
        when Rotation_180 =>
          idx := 1 + 3 * rev_x + padded_line_size_x * y;
        when Rotation_270 =>
          idx := 1 + 3 * y     + padded_line_size_y * x;
      end case;
      mem_x := x;
      mem_y := y;
    end Set_X_Y;
    --
    --  No background version of Put_Pixel
    --
    procedure Put_Pixel_without_Bkg
      (red, green, blue : Primary_color_range;
       alpha            : Primary_color_range)
    is
    pragma Inline (Put_Pixel_without_Bkg);
    pragma Unreferenced (alpha);
      use GID;
    begin
      buffer (idx .. idx + 2) := (red, green, blue);
      --  GID requires us to look to next pixel for next time:
      case correct_orientation is
        when Unchanged =>
          idx := idx + 3;
        when Rotation_90 =>
          idx := idx - padded_line_size_y;
        when Rotation_180 =>
          idx := idx - 3;
        when Rotation_270 =>
          idx := idx + padded_line_size_y;
      end case;
    end Put_Pixel_without_Bkg;
    --
    --  Unicolor background version of Put_Pixel
    --
    procedure Put_Pixel_with_Unicolor_Bkg
      (red, green, blue : Primary_color_range;
       alpha            : Primary_color_range)
    is
    pragma Inline (Put_Pixel_with_Unicolor_Bkg);
      u_red   : constant := 200;
      u_green : constant := 133;
      u_blue  : constant := 32;
    begin
      if alpha = 255 then
        buffer (idx .. idx + 2) := (red, green, blue);
      else  --  Blend with background color
        buffer (idx)     := Primary_color_range ((U16 (alpha) * U16 (red)   + U16 (255 - alpha) * u_red)   / 255);
        buffer (idx + 1) := Primary_color_range ((U16 (alpha) * U16 (green) + U16 (255 - alpha) * u_green) / 255);
        buffer (idx + 2) := Primary_color_range ((U16 (alpha) * U16 (blue)  + U16 (255 - alpha) * u_blue)  / 255);
      end if;
      idx := idx + 3;
      --  ^ GID requires us to look to next pixel on the right for next time.
    end Put_Pixel_with_Unicolor_Bkg;
    --
    --  Background image version of Put_Pixel
    --
    procedure Put_Pixel_with_Image_Bkg
      (red, green, blue : Primary_color_range;
       alpha            : Primary_color_range)
    is
    pragma Inline (Put_Pixel_with_Image_Bkg);
      b_red,
      b_green,
      b_blue : Primary_color_range;
      bkg_idx : Natural;
    begin
      if alpha = 255 then
        buffer (idx .. idx + 2) := (red, green, blue);
      else  --  Blend with background image
        bkg_idx :=
          1 + 3 * (mem_x mod bkg_width) +
          bkg_padded_line_size * (bkg_height - 1 - (mem_y mod bkg_height));
        b_red   := bkg_buf (bkg_idx);
        b_green := bkg_buf (bkg_idx + 1);
        b_blue  := bkg_buf (bkg_idx + 2);
        buffer (idx)     := Primary_color_range ((U16 (alpha) * U16 (red)   + U16 (255 - alpha) * U16 (b_red))   / 255);
        buffer (idx + 1) := Primary_color_range ((U16 (alpha) * U16 (green) + U16 (255 - alpha) * U16 (b_green)) / 255);
        buffer (idx + 2) := Primary_color_range ((U16 (alpha) * U16 (blue)  + U16 (255 - alpha) * U16 (b_blue))  / 255);
      end if;
      idx := idx + 3;
      --  ^ GID requires us to look to next pixel on the right for next time.
      mem_x := mem_x + 1;
    end Put_Pixel_with_Image_Bkg;

    stars : Natural := 0;
    procedure Feedback (percents : Natural) is
      so_far : constant Natural := percents / 5;
    begin
      for i in stars + 1 .. so_far loop
        Put (Current_Error, '*');
      end loop;
      stars := so_far;
    end Feedback;

    --  Here, the exciting thing: the instantiation of
    --  GID.Load_Image_Contents. In our case, we load the image
    --  into a 24-bit bitmap (because we provide a Put_Pixel
    --  that does that with the pixels), but we could do plenty
    --  of other things instead, like display the image live on a GUI.

    --  More exciting: for tuning performance, we have 3 different
    --  instances of GID.Load_Image_Contents (each of them with the full
    --  decoders for all formats, own specialized generic instances, inlines,
    --  etc.) depending on the transparency features.

    procedure PNG_Load_without_Bkg is
      new GID.Load_Image_Contents
        (Primary_color_range,
         Set_X_Y,
         Put_Pixel_without_Bkg,
         Feedback,
         GID.fast);

    procedure PNG_Load_with_Unicolor_Bkg is
      new GID.Load_Image_Contents
        (Primary_color_range,
         Set_X_Y,
         Put_Pixel_with_Unicolor_Bkg,
         Feedback,
         GID.fast);

    procedure PNG_Load_with_Image_Bkg is
      new GID.Load_Image_Contents
        (Primary_color_range,
         Set_X_Y,
         Put_Pixel_with_Image_Bkg,
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
    case correct_orientation is
      --  Set the PNG filter byte to 0 at the start of each destination line.
      when GID.Unchanged | GID.Rotation_180 =>
        for y in 0 .. image_height - 1 loop
          buffer (y * padded_line_size_x) := 0;
        end loop;
      when GID.Rotation_90 | GID.Rotation_270 =>
        for y in 0 .. image_width - 1 loop
          buffer (y * padded_line_size_y) := 0;
        end loop;
    end case;
    if GID.Expect_Transparency (image) then
      if background_image_name = Null_Unbounded_String then
        PNG_Load_with_Unicolor_Bkg (image, next_frame);
      else
        bkg_width  := GID.Pixel_Width (bkg);
        bkg_height := GID.Pixel_Height (bkg);
        bkg_padded_line_size := 1 + 3 * bkg_width;
        PNG_Load_with_Image_Bkg (image, next_frame);
      end if;
    else
      PNG_Load_without_Bkg (image, next_frame);
    end if;
    --  -- For testing: white rectangle with a red half-frame.
    --  buffer.all:= (others => 255);
    --  for x in 0 .. GID.Pixel_width (image) - 1 loop
    --    Put_Pixel_with_Unicolor_Bkg (x, 0, 255, 0, 0, 255);
    --  end loop;
    --  for y in 0 .. GID.Pixel_height (image) - 1 loop
    --    Put_Pixel_with_Unicolor_Bkg (0, y, 255, 0, 0, 255);
    --  end loop;
  exception
    when others =>
      if forgive_errors then
        in_error := True;
        next_frame := 0.0;
      else
        raise;
      end if;
  end Load_Raw_Image;

  procedure Load_raw_image_0 is new Load_Raw_Image (GID.Unchanged);
  procedure Load_raw_image_90 is new Load_Raw_Image (GID.Rotation_90);
  procedure Load_raw_image_180 is new Load_Raw_Image (GID.Rotation_180);
  procedure Load_raw_image_270 is new Load_Raw_Image (GID.Rotation_270);

  procedure Dump_PNG (file_name : String; i : GID.Image_Descriptor) is
    f : Ada.Streams.Stream_IO.File_Type;
    dest_width  : Integer;
    dest_height : Integer;
  begin
    case GID.Display_Orientation (i) is
      when GID.Unchanged | GID.Rotation_180 =>
        dest_width  := GID.Pixel_Width (i);
        dest_height := GID.Pixel_Height (i);
      when GID.Rotation_90 | GID.Rotation_270 =>
        dest_width  := GID.Pixel_Height (i);
        dest_height := GID.Pixel_Width (i);
    end case;

    Create (f, Out_File, file_name & ".gid.png");
    Dumb_PNG.Write (img_buf.all, Dumb_PNG.padded, dest_width, dest_height, Stream (f).all);
    Close (f);
  end Dump_PNG;

  procedure Process (file_name : String; as_background, test_only : Boolean) is
    f : Ada.Streams.Stream_IO.File_Type;
    i : GID.Image_Descriptor;
    up_name : constant String := To_Upper (file_name);
    --
    next_frame, current_frame : Ada.Calendar.Day_Duration := 0.0;
    use Ada.Calendar, Ada.Strings, Ada.Strings.Fixed;
    use type GID.Image_Format_Type;
    T0, T1 : Time;
  begin
    --
    --  Load the image in its original format
    --
    Open (f, In_File, file_name);
    Put_Line (Current_Error, "Processing " & file_name & "...");
    --
    GID.Load_Image_Header
      (i,
       Stream (f).all,
       try_tga =>
         file_name'Length >= 4 and then
         up_name (up_name'Last - 3 .. up_name'Last) = ".TGA");
    --
    Put_Line
      (Current_Error,
       "  Image format: " & GID.Format (i)'Image);
    Put_Line
      (Current_Error,
       "  Image detailed format: " & GID.Detailed_Format (i));
    Put_Line
      (Current_Error,
       "  Image sub-format ID (if any):" & GID.Subformat (i)'Image);
    Put_Line
      (Current_Error,
       "  Dimensions in pixels:" &
       GID.Pixel_Width (i)'Image & " x" &
       GID.Pixel_Height (i)'Image);
    Put_Line
      (Current_Error,
       "  Display orientation: " & GID.Display_Orientation (i)'Image);
    Put
      (Current_Error,
       "  Color depth:" & GID.Bits_per_Pixel (i)'Image & " bits");

    if GID.Bits_per_Pixel (i) <= 24 then
      Put_Line
        (Current_Error,
         ',' & Integer'Image (2**GID.Bits_per_Pixel (i)) & " colors");
    else
      New_Line (Current_Error);
    end if;

    Put_Line
      (Current_Error,
       "  Palette: " & GID.Has_Palette (i)'Image);
    Put_Line
      (Current_Error,
       "  Greyscale: " & GID.Greyscale (i)'Image);
    Put_Line
      (Current_Error,
       "  RLE encoding (if any): " & GID.Is_RLE_Encoded (i)'Image);
    Put_Line
      (Current_Error,
       "  Expect transparency: " & GID.Expect_Transparency (i)'Image);

    if GID.Format (i) = GID.GIF then
      null;  --  Interlaced-or-not choice is per frame in the GIF format.
    else
      Put_Line
        (Current_Error,
         "  Interlaced / Progressive: " & GID.Is_Progressive (i)'Image);
    end if;

    Put_Line (Current_Error, "1........10........20");
    Put_Line (Current_Error, "         |         | ");
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
      New_Line (Current_Error);
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
          Dump_PNG (file_name & '_' & Trim (current_frame'Image, Left), i);
        end if;
        New_Line (Current_Error);
        if in_error then
          Put_Line (Current_Error, "Error!");
        end if;
        exit Animation_Loop when next_frame = 0.0;
        current_frame := next_frame;
      end loop Animation_Loop;
    end if;

    T1 := Clock;

    Close (f);
    Put_Line
      (Current_Error,
       "Time elapsed:" & Duration'Image (T1 - T0) & " seconds.");

  exception
    when GID.unknown_image_format =>
      Put_Line (Current_Error, "  Image format is unknown!");
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
  Put_Line
    (Current_Error, "To_PNG, using GID version " & GID.version & " dated " & GID.reference);
  begin
    Process (default_bkg_name, True, False);
    --  if success:
    background_image_name := To_Unbounded_String (default_bkg_name);
  exception
    when Ada.Text_IO.Name_Error =>
      null;  --  nothing bad, we just couldn't find the default background
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
            Put_Line (Current_Error, "Background image is " & opt);
            Process (opt, True, False);
            --  Define this only after processing, otherwise
            --  a transparent background will try to use
            --  an undefined background.
            background_image_name := To_Unbounded_String (opt);
          end if;
        end;
      else
        Process (arg, False, test_only);
      end if;
    end;
  end loop;
end To_PNG;
