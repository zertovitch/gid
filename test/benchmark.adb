with GID,
     Comp_Img_Fct;

with Ada.Calendar,
     Ada.Characters.Handling,
     Ada.Streams.Stream_IO,
     Ada.Text_IO,
     Ada.Unchecked_Deallocation;

with Interfaces.C;

procedure Benchmark is

  use Ada.Characters.Handling, Ada.Streams.Stream_IO, Ada.Text_IO, Interfaces;

  procedure Blurb is
  begin
    Put_Line ("Benchmark for the GID (Generic Image Decoder) package");
    Put_Line ("Package version " & GID.version & " dated " & GID.reference);
    Put_Line ("URL: " & GID.web);
    New_Line;
    Put_Line ("GID is compared against results of ImageMagick ( https://imagemagick.org/ ).");
    Put_Line ("The executable magick needs to be visible on the path.");
    New_Line;
    Put_Line ("Syntax:");
    Put_Line ("benchmark");
    New_Line;
    Put_Line ("In order to save your SDD / Hard Disk and to minimize file transaction times,");
    Put_Line ("it is recommended to copy this program's executable and the images (./img)");
    Put_Line ("to a RAM Disk and run the program there.");
    New_Line;
  end Blurb;

  --  The following code is copy-pasted from mini.adb.

  type Byte_Array is array (Integer range <>) of Unsigned_8;
  type p_Byte_Array is access Byte_Array;
  procedure Dispose is new Ada.Unchecked_Deallocation (Byte_Array, p_Byte_Array);

  img_buf : p_Byte_Array := null;

  force_allocate : constant := -1;
  mem_buffer_last : Integer;

  --  Load image into a 24-bit truecolor RGB raw bitmap (for a PPM output)
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
      so_far : constant Natural := percents / 10;
    begin
      for i in stars + 1 .. so_far loop
        Put ('*');
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

  procedure Dump_PPM (name : String; i : GID.Image_Descriptor) is
    f : Ada.Streams.Stream_IO.File_Type;
  begin
    Create (f, Out_File, name);
    --  PPM Header:
    String'Write
      (Stream (f),
       "P6 " & GID.Pixel_Width (i)'Image & GID.Pixel_Height (i)'Image &
       " 255" & ASCII.LF);
    --  PPM raw BGR image:
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
  end Dump_PPM;

  procedure Process (name : String) is
    f : Ada.Streams.Stream_IO.File_Type;
    i : GID.Image_Descriptor;
    up_name : constant String := To_Upper (name);
    --
    next_frame : Ada.Calendar.Day_Duration := 0.0;
    --
    function GNAT_Sys (Arg : Interfaces.C.char_array) return Interfaces.C.int;
    pragma Import (C, GNAT_Sys, "system");

    procedure Sys (Command : String; Result : out Integer) is
      --  https://rosettacode.org/wiki/Execute_a_system_command#Ada
    begin
      Result := Integer (GNAT_Sys (Interfaces.C.To_C (Command)));
    end Sys;

    use Ada.Calendar;
    res : Integer;
    T0, T1, T2 : Time;
    dur_gid, dur_magick : Duration;

    gid_name    : constant String := name & "_gid.ppm";
    magick_name : constant String := name & "_magick.ppm";

    dist : Long_Float;

  begin
    --
    --  Load the image in its original format
    --
    Open (f, In_File, name);
    Put ("Processing " & name & "... ");
    T0 := Clock;
    --
    GID.Load_Image_Header
      (i,
       Stream (f).all,
       try_tga =>
         name'Length >= 4 and then
         up_name (up_name'Last - 3 .. up_name'Last) = ".TGA");
    --
    mem_buffer_last := force_allocate;
    Load_Raw_Image (i, img_buf, next_frame);
    Dump_PPM (gid_name, i);
    New_Line;
    --
    Close (f);
    T1 := Clock;
    --
    --  Call the other tool.
    --
    Sys ("magick " & name & ' ' & magick_name, res);
    if res /= 0 then
      Put_Line ("Error calling magick!");
      raise Program_Error;
    else
      T2 := Clock;
      dur_gid    := T1 - T0;
      dur_magick := T2 - T1;
      Put
        ("Durations: GID:" & dur_gid'Image &
         ", Magick:" & dur_magick'Image & "; difference score:");
      dist := Comp_Img_Fct (gid_name, magick_name, False);
      Put_Line (dist'Image);
      New_Line;
    end if;
  end Process;

  iterations : constant := 1;
  --  !!  To do: compute duration averages over many iterations.

begin
  Blurb;
  delay 5.0;
  for iter in 1 .. iterations loop
    Put_Line ("================================== Iteration" & iter'Image);
    Process ("img/jpeg_baseline_biarritz.jpg");
    Process ("img/jpeg_progressive_lyon.jpg");
    Process ("img/jpeg_progressive_walensee.jpg");
  end loop;
end Benchmark;
