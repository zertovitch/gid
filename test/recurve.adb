--  Recurve  -  recover curves from a chart (in JPEG, PNG, or other image format)
--
--  By David Malinge and Gautier de Montmollin

with GID;

with Ada.Calendar;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Interfaces;

procedure Recurve is

  --  Parameters

  thres_grid        : constant:= 0.925;      --  avg intensity below thres_grid => grid line
  thres_curve       : constant:= 0.75;       --  intensity below thres_curve => curve
  thres_simil       : constant:= 0.01 ** 2;  --  similarity within curve
  thres_simil_start : constant:= 0.10 ** 2;  --  similarity when scanning for curves
  radius: constant:= 40;

  sep: constant Character:= ';';

  procedure Blurb is
  begin
    Put_Line(Standard_Error, "Recurve * Recover from a chart in any image format");
    New_Line(Standard_Error);
    Put_Line(Standard_Error, "GID version " & GID.version & " dated " & GID.reference);
    Put_Line(Standard_Error, "URL: " & GID.web);
    New_Line(Standard_Error);
    Put_Line(Standard_Error, "Syntax:");
    Put_Line(Standard_Error, " recurve <image>");
    New_Line(Standard_Error);
    Put_Line(Standard_Error, "Output:");
    Put_Line(Standard_Error, " <image>.csv");
    New_Line(Standard_Error);
  end Blurb;

  use Interfaces;

  subtype Primary_color_range is Unsigned_8;

  subtype Real is Long_Float;

  type RGB is record
    r,g,b: Real;
  end record;

  function Grey(c: RGB) return Real is
  begin
    return (c.r + c.g + c.b) / 3.0;
  end;

  function Dist2(c1,c2: RGB) return Real is
  begin
    return
      (c1.r - c2.r) ** 2 +
      (c1.g - c2.g) ** 2 +
      (c1.b - c2.b) ** 2;
  end;

  function Img(c: RGB) return String is
  begin
    return "  R:" & Integer'Image(Integer(c.r * 255.0)) &
           "  G:" & Integer'Image(Integer(c.g * 255.0)) &
           "  B:" & Integer'Image(Integer(c.b * 255.0));
  end Img;

  --  Bidimensional array. Slower than unidimensional, but fits our purpose.
  type Bitmap is array(Integer range <>, Integer range <>) of RGB;
  type p_Bitmap is access Bitmap;
  procedure Dispose is new Ada.Unchecked_Deallocation(Bitmap, p_Bitmap);

  -- Load image
  procedure Load_raw_image(
    image : in out GID.Image_descriptor;
    bmp   : in out p_Bitmap;
    next_frame: out Ada.Calendar.Day_Duration
  )
  is
    image_width : constant Positive:= GID.Pixel_width(image);
    image_height: constant Positive:= GID.Pixel_height(image);
    pos_x, pos_y: Natural;
    --
    procedure Set_X_Y (x, y: Natural) is
    begin
      pos_x:= x;
      pos_y:= y;
    end Set_X_Y;
    --
    procedure Put_Pixel (
      red, green, blue : Primary_color_range;
      alpha            : Primary_color_range
    )
    is
    pragma Warnings(off, alpha); -- alpha is just ignored
    begin
      bmp(pos_x, bmp'Last(2) - pos_y):=
        (Real(red)   / 255.0,
         Real(green) / 255.0,
         Real(blue)  / 255.0
        );
      pos_x:= pos_x + 1;
      -- ^ GID requires us to look to next pixel on the right for next time.
    end Put_Pixel;

    stars: Natural:= 0;
    procedure Feedback(percents: Natural) is
      so_far: constant Natural:= percents / 5;
    begin
      for i in stars+1..so_far loop
        Put( Standard_Error, '*');
      end loop;
      stars:= so_far;
    end Feedback;

    procedure Load_image is
      new GID.Load_image_contents(
        Primary_color_range, Set_X_Y,
        Put_Pixel, Feedback, GID.fast
      );

  begin
    Dispose(bmp);
    bmp:= new Bitmap(0..image_width-1, 0..image_height-1);
    Load_image(image, next_frame);
  end Load_raw_image;

  bmp: p_Bitmap:= null;

  procedure Detect_curves(name: String) is
    grid_hor: array(bmp'Range(1)) of Boolean:= (others => False);
    grid_ver: array(bmp'Range(1)) of Boolean:= (others => False);
    v: Real;
    done: array(bmp'Range(1), bmp'Range(1)) of Boolean:= (others => (others => False));
    --  color_scanned: array(0..255, 0..255, 0..255) of Boolean:= ... mmmh too big

    type Curve_ys is array(bmp'Range(1)) of Integer;
    type Curve_descr is record
      ys: Curve_ys:= (others => -1);
      min_x: Integer:= Integer'Last;
      max_x: Integer:= Integer'First;
      color: RGB;
    end record;

    Curve_Stack: array(1..bmp'Length(2)) of Curve_descr;
    curve_top: Natural:= 0;

    procedure Scan_curve(x0, y0, xd: Integer) is
      curv: Curve_descr renames Curve_Stack(curve_top);
      c: RGB renames curv.color;
      x: Integer:= x0;
      y: Integer:= y0;
      found: Boolean;
      xt, yt: Integer;  --  test points
      --
      procedure Mark_point is
      begin
        done(x,y):= True;
        curv.ys(x):= bmp'Last(2) - y;
        curv.min_x:= Integer'Min(curv.min_x, x);
        curv.max_x:= Integer'Max(curv.max_x, x);
      end;
      --
    begin
      Mark_point;
      Scan: loop
        found:= False;
        Half_Disc: for r in 1..radius loop
          for xs in 1..r loop
            for ys in -r..r loop
              if xs**2 + ys**2 <= r **2 then
                xt:= x + xs * xd;  --  Choose direction
                yt:= y + ys;
                if xt in bmp'Range(1) and then yt in bmp'Range(2) and then
                  (not done(xt, yt) or grid_ver(xt) or grid_hor(yt)) and then
                  Dist2(bmp(xt,yt), c) < thres_simil
                then
                  x:= xt;
                  y:= yt;
                  Mark_point;
                  found:= True;
                  exit Half_Disc;
                end if;
              end if;
            end loop;
          end loop;
        end loop Half_Disc;
        exit when not found;
      end loop Scan;
    end;

    x0: Integer;
    color0: RGB;
    f: File_Type;
    min_min_x: Integer:= Integer'Last;
    max_max_x: Integer:= Integer'First;
  begin
    New_Line;
    --
    --  Detect vertical gridlines - and some noise...
    --
    for x in bmp'Range(1) loop
      v:= 0.0;
      for y in bmp'Range(2) loop
        v:= v + Grey(bmp(x,y));
      end loop;
      v:= v / Real(bmp'Length(2));
      if v < thres_grid then
        grid_ver(x):= True;
        Put_Line("Vertical: " & Integer'Image(x));
      end if;
    end loop;
    --
    --  Detect horizontal gridlines - and some noise...
    --
    for y in bmp'Range(2) loop
      v:= 0.0;
      for x in bmp'Range(1) loop
        v:= v + Grey(bmp(x,y));
      end loop;
      v:= v / Real(bmp'Length(1));
      if v < thres_grid then
        grid_hor(y):= True;
        Put_Line("Horizontal: " & Integer'Image(y));
      end if;
    end loop;
    --
    --  Main scan for curves, start in the middle
    --
    x0:= bmp'Last(1) / 2;
    for y in bmp'Range(2) loop
      color0:= bmp(x0,y);
      if (not grid_hor(y)) and then Grey(color0) < thres_curve and then not done(x0,y) then
        if y > 0 and then done(x0,y-1) and then Dist2(bmp(x0,y-1), color0) < thres_simil_start then
          done(x0,y):= True;  --  Actually the same, fat curve as one pixel above
        else
          Put_Line("curve: " & Integer'Image(x0) & Integer'Image(y));
          curve_top:= curve_top + 1;
          Curve_Stack(curve_top).color:= color0;
          Scan_curve(x0, y, -1);
          Scan_curve(x0, y, +1);
        end if;
      end if;
    end loop;
    --
    --  Output curves
    --
    for i in 1..curve_top loop
      min_min_x:= Integer'Min(min_min_x, Curve_Stack(i).min_x);
      max_max_x:= Integer'Max(max_max_x, Curve_Stack(i).max_x);
    end loop;
    --
    Create(f, Out_File, name & ".csv");
    Put_Line(f, "Recurve output");
    Put(f, "Color");
    for i in 1..curve_top loop
      Put(f, sep & Img(Curve_Stack(i).color));
    end loop;
    New_Line(f);
    Put(f, 'x');
    for i in 1..curve_top loop
      Put(f, sep & 'y' & Integer'Image(i));
    end loop;
    New_Line(f);
    for x in min_min_x .. max_max_x loop
      Put(f, Integer'Image(x));
      for i in 1..curve_top loop
        Put(f, sep);
        if x in Curve_Stack(i).min_x .. Curve_Stack(i).max_x and then Curve_Stack(i).ys(x) >= 0 then
          Put(f, Integer'Image(Curve_Stack(i).ys(x)));
        end if;
      end loop;
      New_Line(f);
    end loop;
    Close(f);
  end Detect_curves;

  procedure Process(name: String) is
    use Ada.Streams.Stream_IO;
    f: Ada.Streams.Stream_IO.File_Type;
    i: GID.Image_descriptor;
    up_name: constant String:= To_Upper(name);
    --
    next_frame: Ada.Calendar.Day_Duration:= 0.0;
  begin
    --
    -- Load the image in its original format
    --
    Open(f, In_File, name);
    Put_Line(Standard_Error, "Processing " & name & "...");
    --
    GID.Load_image_header(
      i,
      Stream(f).all,
      try_tga =>
        name'Length >= 4 and then
        up_name(up_name'Last-3..up_name'Last) = ".TGA"
    );
    Put_Line(Standard_Error, ".........v.........v");
    --
    Load_raw_image(i, bmp, next_frame);
    Detect_curves(name);
    New_Line(Standard_Error);
    Close(f);
  end Process;

begin
  if Argument_Count=0 then
    Blurb;
    return;
  end if;
  for i in 1..Argument_Count loop
    Process(Argument(i));
  end loop;
end Recurve;
