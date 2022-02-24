--  All_RGB. See https://allrgb.com/
--
--    "The objective of allRGB is simple: To create images with one
--     pixel for every RGB color (16,777,216); not one color missing,
--     and not one color twice."
--
--  Example derived from mini.adb and recurve.adb.

with GID;

with Ada.Calendar,
     Ada.Characters.Handling,
     Ada.Command_Line,
     Ada.Numerics.Discrete_Random,
     Ada.Streams.Stream_IO,
     Ada.Text_IO,
     Ada.Unchecked_Deallocation;

with Interfaces;

procedure All_RGB is

  use Ada.Streams.Stream_IO, Ada.Text_IO;

  procedure Blurb is
  begin
    Put_Line (Standard_Error, "All_RGB * Converts an image file to a PPM image file with exactly");
    Put_Line (Standard_Error, "            1 pixel per possible RGB colour (8-bit colour channels)");
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
    New_Line (Standard_Error);
  end Blurb;

  use Interfaces;

  type RGB is record
    r, g, b : Unsigned_8;
  end record;

  type Dist_Type is (L1, L2, L3, Linf);

  generic
    dist_choice : Dist_Type;
  function Monotone_Function_of_Distance (p, q : RGB) return Natural;
  pragma Inline (Monotone_Function_of_Distance);

  function Monotone_Function_of_Distance (p, q : RGB) return Natural is
  begin
    --  The goal of the use of generics is to optimize
    --  out the following case statement when the function
    --  is inlined in the main iteration loop:
    case dist_choice is
      when L1 =>
        return
          abs (Integer (p.r) - Integer (q.r)) +
          abs (Integer (p.g) - Integer (q.g)) +
          abs (Integer (p.b) - Integer (q.b));
      when L2 =>
        return
          (Integer (p.r) - Integer (q.r)) ** 2 +
          (Integer (p.g) - Integer (q.g)) ** 2 +
          (Integer (p.b) - Integer (q.b)) ** 2;
      when L3 =>
        return
          (abs (Integer (p.r) - Integer (q.r))) ** 3 +
          (abs (Integer (p.g) - Integer (q.g))) ** 3 +
          (abs (Integer (p.b) - Integer (q.b))) ** 3;
      when Linf =>
        return
          Integer'Max
            (Integer'Max
              ((Integer (p.r) - Integer (q.r)),
               (Integer (p.g) - Integer (q.g))),
             (Integer (p.b) - Integer (q.b)));
    end case;
  end Monotone_Function_of_Distance;

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
      so_far : constant Natural := percents / 10;
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

  generic
    transform_dist_choice : Dist_Type;
  procedure Transform (src : in Bitmap; dst : out Bitmap; tr_iterations : Integer);

  procedure Transform (src : in Bitmap; dst : out Bitmap; tr_iterations : Integer) is
    x1, y1, x2, y2 : Integer;
    s1, s2 : RGB;
    package Side_Random is new Ada.Numerics.Discrete_Random (All_RGB_Range);
    use Side_Random;
    gen : Generator;
    dist_no_swap, dist_swap : Natural;
    function M_Funct_Dist_Lx is
      new Monotone_Function_of_Distance (transform_dist_choice);
    mix_phase : constant := 3 * 4096 ** 2;
    do_swap : Boolean;
    total_iter : constant Integer := mix_phase + tr_iterations;
    tick : constant Integer := total_iter / 10;
    --
  begin
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
    --
    Reset (gen);
    for i in 1 .. total_iter loop
      x1 := Random (gen);
      y1 := Random (gen);
      x2 := Random (gen);
      y2 := Random (gen);
      if i <= mix_phase then
        --  In the initial phase we always swap pixels, in order
        --  to have an uniform background.
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
        --  A consequence is that we do not risk having pixels that
        --  are prematurely stuck in a local optimum.
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

  procedure Process (name : String; Lx : Dist_Type; iterations : Integer) is
    use Ada.Calendar, Ada.Characters.Handling;
    f : Ada.Streams.Stream_IO.File_Type;
    i : GID.Image_descriptor;
    up_name : constant String := To_Upper (name);
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
    case Lx is
      when L1   => Transform_L1   (src.all, dst.all, iterations);
      when L2   => Transform_L2   (src.all, dst.all, iterations);
      when L3   => Transform_L3   (src.all, dst.all, iterations);
      when Linf => Transform_Linf (src.all, dst.all, iterations);
    end case;
    Dump_PPM
      (name (name'First .. name'Last - 4) & '_' &
       Dist_Type'Image (Lx) & '_' &
       iter_m_img (iter_m_img'First + 1 .. iter_m_img'Last) & 'M',
       dst.all);
    Dispose (src);
    Dispose (dst);
    T1 := Clock;
    Put (Standard_Error, " Time elapsed:" & Duration'Image (T1 - T0));
    New_Line (Standard_Error);
    Close (f);
  end Process;

  Lx : Dist_Type := L2;
  iter : Integer := 100e6;
  use Ada.Command_Line;

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
          when others =>
            Blurb;
            return;
        end case;
      else
        Process (Argument (i), Lx, iter);
      end if;
    end;
  end loop;
end All_RGB;
