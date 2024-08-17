with GID;

with Color_Distances;

with Ada.Calendar,
     Ada.Streams.Stream_IO,
     Ada.Text_IO,
     Ada.Unchecked_Deallocation;

with Interfaces;

function Comp_Img_Fct
  (image_name_1, image_name_2 : String;
   verbose                    : Boolean) return Long_Float
is
  score : Long_Float;

  use Ada.Streams.Stream_IO, Ada.Text_IO;
  use Interfaces;

  package Color_Distances_8_Bit is
    new Color_Distances (Unsigned_8, Long_Float);

  use Color_Distances_8_Bit;

  function Normalized_L1_Distance is new Normalized_Distance (L1);

  type Bitmap is array (Integer range <>, Integer range <>) of RGB;
  type p_Bitmap is access Bitmap;
  procedure Dispose is new Ada.Unchecked_Deallocation (Bitmap, p_Bitmap);

  --  Load image into a 24-bit truecolor RGB bitmap
  procedure Load_Raw_Image
    (image      : in out GID.Image_Descriptor;
     bmp        : in out Bitmap;
     next_frame :    out Ada.Calendar.Day_Duration)
  is
    subtype Primary_Color_Range is Unsigned_8;
    pos_x, pos_y, max_y : Natural;
    --
    procedure Set_X_Y (x, y : Natural) is
    begin
      pos_x := x;
      pos_y := y;
    end Set_X_Y;
    --
    procedure Put_Pixel
      (red, green, blue : Primary_Color_Range;
       alpha            : Primary_Color_Range)
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
      if verbose then
        for i in stars + 1 .. so_far loop
          Put ('*');
        end loop;
        stars := so_far;
      end if;
    end Feedback;

    procedure Load_image is
      new GID.Load_Image_Contents
        (Primary_Color_Range, Set_X_Y,
         Put_Pixel, Feedback, GID.fast);

  begin
    max_y := GID.Pixel_Height (image) - 1;
    Load_image (image, next_frame);
  end Load_Raw_Image;

  type Image_Container is record
    file : Ada.Streams.Stream_IO.File_Type;
    desc : GID.Image_Descriptor;
    bitm : p_Bitmap := null;
  end record;

  img : array (1 .. 2) of Image_Container;

  next_frame : Ada.Calendar.Day_Duration := 0.0;

  procedure Free_Bitmaps is
  begin
    for i in img'Range loop
      Dispose (img (i).bitm);
    end loop;
  end Free_Bitmaps;

begin
  Open (img (1).file, In_File, image_name_1);
  Open (img (2).file, In_File, image_name_2);
  for i in img'Range loop
    if verbose then
      Put_Line ("  Loading " & Name (img (i).file) & "...");
    end if;
    --
    GID.Load_Image_Header (img (i).desc, Stream (img (i).file).all, True);
    if verbose then
      Put_Line ("    .........v");
      Put ("    ");
    end if;
    --
    img (i).bitm :=
      new Bitmap
        (0 .. GID.Pixel_Width  (img (i).desc) - 1,
         0 .. GID.Pixel_Height (img (i).desc) - 1);
    Load_Raw_Image (img (i).desc, img (i).bitm.all, next_frame);
    Close (img (i).file);
    if verbose then
      New_Line;
    end if;
  end loop;
  if img (1).bitm'Length (1) /= img (2).bitm'Length (1) then
    Free_Bitmaps;
    raise Constraint_Error
      with
        "Image widths don't fit: " &
        GID.Pixel_Width (img (1).desc)'Image & " vs." &
        GID.Pixel_Width (img (2).desc)'Image;
  end if;
  if img (1).bitm'Length (2) /= img (2).bitm'Length (2) then
    Free_Bitmaps;
    raise Constraint_Error
      with "Image heights don't fit: " &
        GID.Pixel_Height (img (1).desc)'Image & " vs." &
        GID.Pixel_Height (img (2).desc)'Image;
  end if;
  score := 0.0;
  for x in img (1).bitm'Range (1) loop
    for y in img (1).bitm'Range (2) loop
      score := score + Normalized_L1_Distance (img (1).bitm (x, y), img (2).bitm (x, y));
    end loop;
  end loop;
  score :=
    score /
      (Long_Float (GID.Pixel_Width  (img (1).desc)) *
       Long_Float (GID.Pixel_Height (img (2).desc)));
  Free_Bitmaps;
  return score;
end Comp_Img_Fct;
