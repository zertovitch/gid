--  All_RGB. See https://allrgb.com/
--
--  "The objective of allRGB is simple: To create images with one
--   pixel for every RGB color (16,777,216); not one color missing,
--   and not one color twice."
--
--  Example derived from mini.adb and recurve.adb.

with GID;

with Ada.Calendar;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Numerics.Discrete_Random;
with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Interfaces;

procedure All_RGB is

  procedure Blurb is
  begin
    Put_Line (Standard_Error, "All_RGB * Converts an image file to a PPM file with exactly");
    Put_Line (Standard_Error, "          1 pixel per RGB colour (8-bit channel)");
    Put_Line (Standard_Error, "Simple test for the GID (Generic Image Decoder) package");
    Put_Line (Standard_Error, "Package version " & GID.version & " dated " & GID.reference);
    Put_Line (Standard_Error, "URL: " & GID.web);
    New_Line (Standard_Error);
    Put_Line (Standard_Error, "Syntax:");
    Put_Line (Standard_Error, "all_rgb <image_1> [<image_2>...]");
    New_Line (Standard_Error);
  end Blurb;

  use Interfaces;

  type RGB is record
    r, g, b : Unsigned_8;
  end record;

  function Dist (p, q : RGB) return Natural is
  begin
    return
      abs (Integer (p.r) - Integer (q.r)) +
      abs (Integer (p.g) - Integer (q.g)) +
      abs (Integer (p.b) - Integer (q.b));
  end Dist;

  procedure Swap (p, q : in out RGB) is
    tmp : constant RGB := p;
  begin
    p := q;
    q := tmp;
  end Swap;

  type Bitmap is array (Integer range <>, Integer range <>) of RGB;
  type p_Bitmap is access Bitmap;
  procedure Dispose is new Ada.Unchecked_Deallocation (Bitmap, p_Bitmap);

  --  Load image into a 24-bit truecolor RGB raw bitmap (for a PPM output)
  procedure Load_raw_image (
    image      : in out GID.Image_descriptor;
    bmp        : in out Bitmap;
    next_frame :    out Ada.Calendar.Day_Duration
  )
  is
    subtype Primary_color_range is Unsigned_8;
    pos_x, pos_y, max_y : Natural;
    --
    procedure Set_X_Y (x, y : Natural) is
    begin
      pos_x := x;
      pos_y := y;
    end Set_X_Y;
    --
    procedure Put_Pixel (
      red, green, blue : Primary_color_range;
      alpha            : Primary_color_range
    )
    is
    pragma Unreferenced (alpha);
    begin
      bmp (pos_x, max_y - pos_y) := (red, green, blue);
      pos_x := pos_x + 1;
      --  ^ GID requires us to look to next pixel on the right for next time.
    end Put_Pixel;

    stars : Natural := 0;
    procedure Feedback (percents : Natural) is
      so_far : constant Natural := percents / 5;
    begin
      for i in stars + 1 .. so_far loop
        Put (Standard_Error, '*');
      end loop;
      stars := so_far;
    end Feedback;

    procedure Load_image is
      new GID.Load_image_contents (
        Primary_color_range, Set_X_Y,
        Put_Pixel, Feedback, GID.fast
      );

  begin
    max_y := GID.Pixel_height (image) - 1;
    Load_image (image, next_frame);
  end Load_raw_image;

  subtype All_RGB_Range is Integer range 0 .. 4095;

  procedure Transform (src : in Bitmap; dst : out Bitmap) is
    x1, y1, x2, y2 : Integer;
    s1, s2, d1, d2 : RGB;
    package Side_Random is new Ada.Numerics.Discrete_Random (All_RGB_Range);
    use Side_Random;
    gen : Generator;
    dist_no_swap, dist_swap : Natural;
  begin
    for r in Unsigned_8'(0) .. 255 loop
      for g in Unsigned_8'(0) .. 255 loop
        for b in Unsigned_8'(0) .. 255 loop
          x1 := Integer (r) + Integer (g and 15) * 256;
          y1 := Integer (b) + Integer (Shift_Right (g, 4)) * 256;
          dst (x1, y1) := (r, g, b);
        end loop;
      end loop;
    end loop;
    for i in 1 .. 1_000_000_000 loop
      x1 := Random (gen);
      x2 := Random (gen);
      y1 := Random (gen);
      y2 := Random (gen);
      s1 := src (x1 * src'Last (1) / dst'Last (1), y1 * src'Last (2) / dst'Last (2));
      s2 := src (x2 * src'Last (1) / dst'Last (1), y2 * src'Last (2) / dst'Last (2));
      d1 := dst (x1, y1);
      d2 := dst (x2, y2);
      dist_no_swap := Dist (s1, d1) + Dist (s2, d2);
      dist_swap    := Dist (s1, d2) + Dist (s2, d1);
      if dist_swap < dist_no_swap then
        Swap (dst (x1, y1), dst (x2, y2));
      end if;
    end loop;
  end Transform;

  procedure Dump_PPM (name : String; bmp : Bitmap) is
    f : Ada.Streams.Stream_IO.File_Type;
  begin
    Create (f, Out_File, name & ".ppm");
    --  PPM Header:
    String'Write (
      Stream (f),
      "P6 " &
      Integer'Image (bmp'Length (1)) &
      Integer'Image (bmp'Length (2)) & " 255" & ASCII.LF
    );
    for y in bmp'Range (2) loop
      for x in bmp'Range (1) loop
        Unsigned_8'Write (Stream (f), bmp (x, y).r);
        Unsigned_8'Write (Stream (f), bmp (x, y).g);
        Unsigned_8'Write (Stream (f), bmp (x, y).b);
      end loop;
    end loop;
    Close (f);
  end Dump_PPM;

  procedure Process (name : String) is
    f : Ada.Streams.Stream_IO.File_Type;
    i : GID.Image_descriptor;
    up_name : constant String := To_Upper (name);
    --
    use Ada.Calendar;
    next_frame : Day_Duration := 0.0;
    src, dst : p_Bitmap := null;
    T0, T1 : Time;
  begin
    --
    --  Load the image in its original format
    --
    Open (f, In_File, name);
    Put_Line (Standard_Error, "Processing " & name & "...");
    --
    GID.Load_image_header (
      i,
      Stream (f).all,
      try_tga =>
        name'Length >= 4 and then
        up_name (up_name'Last - 3 .. up_name'Last) = ".TGA"
    );
    Put_Line (Standard_Error, ".........v.........v");
    T0 := Clock;
    --
    src := new Bitmap (0 .. GID.Pixel_width (i) - 1, 0 .. GID.Pixel_height (i) - 1);
    Load_raw_image (i, src.all, next_frame);
    dst := new Bitmap (All_RGB_Range, All_RGB_Range);
    Transform (src.all, dst.all);
    Dump_PPM (name, dst.all);
    Dispose (src);
    Dispose (dst);
    T1 := Clock;
    Put (Standard_Error, " Time elapsed:" & Duration'Image (T1 - T0));
    New_Line (Standard_Error);
    Close (f);
  end Process;

begin
  if Argument_Count = 0 then
    Blurb;
    return;
  end if;
  for i in 1 .. Argument_Count loop
    Process (Argument (i));
  end loop;
end All_RGB;
