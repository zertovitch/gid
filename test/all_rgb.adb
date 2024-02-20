--  All_RGB. See https://allrgb.com/
--
--    "The objective of allRGB is simple: To create images with one
--     pixel for every RGB color (16,777,216); not one color missing,
--     and not one color twice."
--
--  This program is derived from mini.adb and recurve.adb.

with GID;

with Color_Distances;

with Ada.Calendar,
     Ada.Characters.Handling,
     Ada.Command_Line,
     Ada.Numerics.Discrete_Random,
     Ada.Streams.Stream_IO,
     Ada.Strings.Unbounded,
     Ada.Text_IO,
     Ada.Unchecked_Deallocation;

with Interfaces;

procedure All_RGB is

  use Ada.Streams.Stream_IO, Ada.Text_IO;

  procedure Blurb is
  begin
    Put_Line (Standard_Error, "All_RGB * Creates an ""all RGB"" image (in .ppm format) similar to a given image");
    Put_Line (Standard_Error, "            ""all RGB"" = 1 pixel per possible RGB colour (8-bit colour channels)");
    New_Line (Standard_Error);
    Put_Line (Standard_Error, "Simple test for the GID (Generic Image Decoder) package");
    Put_Line (Standard_Error, "Package version " & GID.version & " dated " & GID.reference);
    Put_Line (Standard_Error, "URL: " & GID.web);
    New_Line (Standard_Error);
    Put_Line (Standard_Error, "Syntax:");
    Put_Line (Standard_Error, "all_rgb [option] <image_1> [[option] <image_2>...]");
    New_Line (Standard_Error);
    Put_Line (Standard_Error, "Options:");
    Put_Line (Standard_Error, "  -lp: set Lp distance (l1, l2, l3, linf); default: -l2");
    Put_Line (Standard_Error, "  -ix: set number of iterations (x = 1 million iterations); default: -x100");
    Put_Line (Standard_Error, "  -s<img>: set start image as ""<img>"" instead of a trivial, then randomized, image");
    New_Line (Standard_Error);
  end Blurb;

  use Interfaces;

  package Color_Distances_8_Bit is
    new Color_Distances (Unsigned_8, Long_Float);

  use Color_Distances_8_Bit;

  procedure Swap (p, q : in out RGB) is
  pragma Inline (Swap);
    tmp : constant RGB := p;
  begin
    p := q;
    q := tmp;
  end Swap;

  type Bitmap is array (Integer range <>, Integer range <>) of RGB;
  type p_Bitmap is access Bitmap;
  procedure Dispose is new Ada.Unchecked_Deallocation (Bitmap, p_Bitmap);

  --  Load image into a 24-bit truecolor RGB raw bitmap (for a PPM output)
  procedure Load_Raw_Image
    (image      : in out GID.Image_Descriptor;
     bmp        : in out Bitmap;
     next_frame :    out Ada.Calendar.Day_Duration)
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
      so_far : constant Natural := percents / 10;
    begin
      for i in stars + 1 .. so_far loop
        Put (Standard_Error, '*');
      end loop;
      stars := so_far;
    end Feedback;

    procedure Load_image is
      new GID.Load_Image_Contents
        (Primary_color_range, Set_X_Y,
         Put_Pixel, Feedback, GID.fast);

  begin
    max_y := GID.Pixel_Height (image) - 1;
    Load_image (image, next_frame);
  end Load_raw_image;

  subtype All_RGB_Range is Integer range 0 .. 4095;

  generic
    transform_dist_choice : Dist_Type;
  procedure Transform (src : in Bitmap; dst : out Bitmap; do_clear : Boolean; tr_iterations : Integer);

  procedure Transform (src : in Bitmap; dst : out Bitmap; do_clear : Boolean; tr_iterations : Integer) is
    x1, y1, x2, y2 : Integer;
    s1, s2 : RGB;
    package Side_Random is new Ada.Numerics.Discrete_Random (All_RGB_Range);
    use Side_Random;
    gen : Generator;
    dist_no_swap, dist_swap : Natural;
    function M_Funct_Dist_Lx is new Distance (transform_dist_choice);
    mix_phase : Integer := 3 * 4096 ** 2;
    do_swap : Boolean;
    total_iter : Integer;
    tick : Integer;
    --
  begin
    if do_clear then
      --  Deterministic bitmap with all possible 8-bit-per-channel colours.
      for r in Unsigned_8'(0) .. 255 loop
        for g in Unsigned_8'(0) .. 255 loop
          for b in Unsigned_8'(0) .. 255 loop
            x1 := Integer (r) + Integer (g and 15) * 256;
            y1 := Integer (b) + Integer (Shift_Right (g, 4)) * 256;
            dst (x1, y1) := (r, g, b);
          end loop;
        end loop;
      end loop;
    else
      mix_phase := 0;
    end if;
    --
    Reset (gen);
    total_iter := mix_phase + tr_iterations;
    tick := total_iter / 10;
    for i in 1 .. total_iter loop
      x1 := Random (gen);
      y1 := Random (gen);
      x2 := Random (gen);
      y2 := Random (gen);
      if i <= mix_phase then
        --  In the initial phase (mix phase) we always swap pixels, in
        --  order to have an uniform looking, randomized background.
        do_swap := True;
      else
        --  We improve the colour distance to source image
        --  for a pair of randomly chosen pixels.
        s1 := src (x1 * src'Last (1) / dst'Last (1), y1 * src'Last (2) / dst'Last (2));
        s2 := src (x2 * src'Last (1) / dst'Last (1), y2 * src'Last (2) / dst'Last (2));
        dist_no_swap := M_Funct_Dist_Lx (s1, dst (x1, y1)) + M_Funct_Dist_Lx (s2, dst (x2, y2));
        dist_swap    := M_Funct_Dist_Lx (s1, dst (x2, y2)) + M_Funct_Dist_Lx (s2, dst (x1, y1));
        do_swap := dist_swap < dist_no_swap;
        --  Note that destination pixels' colours are *pairwise* improved
        --  in the sense of being closer to the source image's pixels' colours.
        --  However, pixel at (x1, y1), or at (x2, y2), might
        --  have *individually*, after the swap, colours that are
        --  more different from the source's than before the swap.
        --  A consequence is that we do not have the risk having pixels
        --  that are prematurely stuck in a local optimum.
      end if;
      if do_swap  then
        Swap (dst (x1, y1), dst (x2, y2));
      end if;
      if i rem tick = 0 then
        Put (Standard_Error, '*');
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

  procedure Process (name : String; Lx : Dist_Type; iterations : Integer; startup_name : String) is
    use Ada.Calendar, Ada.Characters.Handling;
    f : Ada.Streams.Stream_IO.File_Type;
    i : GID.Image_Descriptor;
    up_name : constant String := To_Upper (name);
    try_tga : constant Boolean :=
      name'Length >= 4 and then
      up_name (up_name'Last - 3 .. up_name'Last) = ".TGA";
    clears      : constant Boolean := startup_name = "";
    use_startup : constant Boolean := not clears;
    up_startup_name : constant String := To_Upper (startup_name);
    try_tga_startup : constant Boolean :=
      startup_name'Length >= 4 and then
      up_startup_name (up_startup_name'Last - 3 .. up_startup_name'Last) = ".TGA";
    --
    next_frame : Day_Duration := 0.0;
    T0, T1 : Time;
    procedure Transform_L1   is new Transform (L1);
    procedure Transform_L2   is new Transform (L2);
    procedure Transform_L3   is new Transform (L3);
    procedure Transform_Linf is new Transform (Linf);
    src, dst : p_Bitmap := null;
    iter_m_img : constant String := Integer'Image (iterations / 1e6);
  begin
    --
    --  Load the image in its original format
    --
    Open (f, In_File, name);
    Put_Line (Standard_Error, "Processing " & name & "...");
    --
    GID.Load_Image_Header (i, Stream (f).all, try_tga);
    if use_startup then
      Put (Standard_Error, ".........v");
    end if;
    Put_Line (Standard_Error, ".........v.........v");
    T0 := Clock;
    --
    src := new Bitmap (0 .. GID.Pixel_Width (i) - 1, 0 .. GID.Pixel_Height (i) - 1);
    Load_raw_image (i, src.all, next_frame);
    Close (f);
    dst := new Bitmap (All_RGB_Range, All_RGB_Range);
    if use_startup then
      Open (f, In_File, startup_name);
      GID.Load_Image_Header (i, Stream (f).all, try_tga_startup);
      Load_raw_image (i, dst.all, next_frame);
      Close (f);
    end if;
    case Lx is
      when L1   => Transform_L1   (src.all, dst.all, clears, iterations);
      when L2   => Transform_L2   (src.all, dst.all, clears, iterations);
      when L3   => Transform_L3   (src.all, dst.all, clears, iterations);
      when Linf => Transform_Linf (src.all, dst.all, clears, iterations);
    end case;
    Dump_PPM
      (name (name'First .. name'Last - 4) & '_' &
       Dist_Type'Image (Lx) & '_' &
       iter_m_img (iter_m_img'First + 1 .. iter_m_img'Last) & 'M',
       dst.all);
    Dispose (src);
    Dispose (dst);
    T1 := Clock;
    New_Line (Standard_Error);
    Put_Line
      (Standard_Error,
       "Time elapsed:" & Duration'Image (T1 - T0) & " seconds.");
  end Process;

  Lx : Dist_Type := L2;
  iter : Integer := 100e6;
  use Ada.Command_Line, Ada.Strings.Unbounded;
  startup : Unbounded_String;

begin
  if Argument_Count = 0 then
    Blurb;
    return;
  end if;
  for i in 1 .. Argument_Count loop
    declare
      arg : constant String := Argument (i);
    begin
      if arg'Length >= 3 and then arg (arg'First) = '-' then
        case arg (arg'First + 1) is
          when 'l' => Lx := Dist_Type'Value (arg (arg'First + 1 .. arg'Last));
          when 'i' => iter := 1e6 * Integer'Value (arg (arg'First + 2 .. arg'Last));
          when 's' =>
            startup := To_Unbounded_String (arg (arg'First + 2 .. arg'Last));
          when others =>
            Blurb;
            return;
        end case;
      else
        Process (Argument (i), Lx, iter, To_String (startup));
      end if;
    end;
  end loop;
end All_RGB;
