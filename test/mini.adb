--
--  Convert any image or animation file to PNG file(s).
--
--  Small-size demo for the GID (Generic Image Decoder) package.
--  For a larger example, look for to_bmp.adb .
--

with GID;

with Dumb_PNG;

with Ada.Calendar,
     Ada.Characters.Handling,
     Ada.Command_Line,
     Ada.Streams.Stream_IO,
     Ada.Text_IO;

with Interfaces;

procedure Mini is

  use Ada.Characters.Handling, Ada.Streams.Stream_IO, Ada.Text_IO;

  procedure Blurb is
  begin
    Put_Line (Current_Error, "Mini * Converts any image file to a PNG file");
    Put_Line (Current_Error, "Simple test for the GID (Generic Image Decoder) package");
    Put_Line (Current_Error, "Package version " & GID.version & " dated " & GID.reference);
    Put_Line (Current_Error, "URL: " & GID.web);
    New_Line (Current_Error);
    Put_Line (Current_Error, "Syntax:");
    Put_Line (Current_Error, "mini <image_1> [<image_2>...]");
    New_Line (Current_Error);
  end Blurb;

  use Interfaces;

  subtype Byte_Array is Dumb_PNG.Byte_Array;
  subtype p_Byte_Array is Dumb_PNG.p_Byte_Array;
  procedure Dispose (X : in out p_Byte_Array) renames Dumb_PNG.Dispose;

  mem_buffer_last : Integer;

  --  Load image into a 24-bit truecolor RGB raw bitmap (for a PNG output)
  procedure Load_Raw_Image
    (image : in out GID.Image_Descriptor;
     buffer : in out p_Byte_Array;
     next_frame : out Ada.Calendar.Day_Duration)
  is
    subtype Primary_Color_Range is Unsigned_8;
    image_width  : constant Positive := GID.Pixel_Width (image);
    image_height : constant Positive := GID.Pixel_Height (image);
    idx : Natural;
    --
    procedure Set_X_Y (x, y : Natural) is
    begin
      idx := 3 * (x + image_width * (image_height - 1 - y));
    end Set_X_Y;
    --
    procedure Put_Pixel
      (red, green, blue : Primary_Color_Range;
       alpha            : Primary_Color_Range)
    is
    pragma Warnings (off, alpha);  --  Alpha is just ignored
    begin
      buffer (idx .. idx + 2) := (red, green, blue);
      idx := idx + 3;
      --  ^ GID requires us to look to next pixel on the right for next time.
    end Put_Pixel;

    stars : Natural := 0;
    procedure Feedback (percents : Natural) is
      so_far : constant Natural := percents / 5;
    begin
      for i in stars + 1 .. so_far loop
        Put (Current_Error, '*');
      end loop;
      stars := so_far;
    end Feedback;

    procedure Load_Image is
      new GID.Load_Image_Contents
        (Primary_Color_Range, Set_X_Y,
         Put_Pixel, Feedback, GID.fast);

    buffer_last : Natural;

  begin
    buffer_last := 3 * image_width * image_height - 1;
    if buffer_last /= mem_buffer_last then
      Dispose (buffer);
      buffer := new Byte_Array (0 .. buffer_last);
      mem_buffer_last := buffer_last;
    end if;
    Load_Image (image, next_frame);
  end Load_Raw_Image;

  img_buf : p_Byte_Array := null;
  force_allocate : constant := -1;

  procedure Dump_PNG (file_name : String; i : GID.Image_Descriptor) is
    f : Ada.Streams.Stream_IO.File_Type;
  begin
    Create (f, Out_File, file_name & ".png");
    Dumb_PNG.Write (img_buf.all, GID.Pixel_Width (i), GID.Pixel_Height (i), Stream (f).all);
    Close (f);
  end Dump_PNG;

  procedure Process (file_name : String) is
    f : Ada.Streams.Stream_IO.File_Type;
    i : GID.Image_Descriptor;
    up_name : constant String := To_Upper (file_name);
    --
    use Ada.Calendar;
    next_frame, current_frame : Day_Duration := 0.0;
    t0, t1 : Time;
  begin
    --
    --  Load the image in its original format.
    --  JPEG's EXIF orientation correction is not done
    --  here (see `to_bmp` or `to_png` for that).
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
    Put_Line (Current_Error, ".........v.........v");
    --
    mem_buffer_last := force_allocate;
    Animation_Loop :
    loop
      t0 := Clock;
      Load_Raw_Image (i, img_buf, next_frame);
      t1 := Clock;
      Put
        (Current_Error,
         "  Decoded in" & Duration'Image (t1 - t0) & " seconds");
      Dump_PNG (file_name & current_frame'Image, i);
      New_Line (Current_Error);
      exit Animation_Loop when next_frame = 0.0;
      current_frame := next_frame;
    end loop Animation_Loop;
    --
    Close (f);
  end Process;

  use Ada.Command_Line;

begin
  if Argument_Count = 0 then
    Blurb;
    return;
  end if;
  for i in 1 .. Argument_Count loop
    Process (Argument (i));
  end loop;
end Mini;
