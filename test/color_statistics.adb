with GID;

with Ada.Calendar,
     Ada.Streams.Stream_IO,
     Ada.Text_IO,
     Ada.Unchecked_Deallocation;

with Interfaces;

package body Color_Statistics is

  type Int_64 is range -(2**63) .. 2**63 - 1;

  procedure Compute
    (image_name : in     String;
     stat       :    out Segmented_Statistic;
     width      :    out Positive;
     height     :    out Positive;
     verbose    : in     Boolean)
  is

    use Ada.Streams.Stream_IO, Ada.Text_IO;
    use Interfaces;

    subtype Primary_Color_Range is Unsigned_8;

    type RGB is record
      r, g, b : Primary_Color_Range;
    end record;

    type RGB_Sum is record
      r, g, b : Int_64;
    end record;

    type Bitmap is array (Integer range <>, Integer range <>) of RGB;
    type p_Bitmap is access Bitmap;
    procedure Dispose is new Ada.Unchecked_Deallocation (Bitmap, p_Bitmap);

    --  Load image into a 24-bit truecolor RGB bitmap
    procedure Load_Raw_Image
      (image      : in out GID.Image_Descriptor;
       bmp        : in out Bitmap;
       next_frame :    out Ada.Calendar.Day_Duration)
    is
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

    img : Image_Container;

    next_frame : Ada.Calendar.Day_Duration := 0.0;

    procedure Free_Bitmap is
    begin
      Dispose (img.bitm);
    end Free_Bitmap;

    xp1, xp2, yp1, yp2 : Natural;

    sum : array (stat'Range (1), stat'Range (2)) of RGB_Sum :=
      (others => (others => (others => 0)));

    pixels_per_sample : array (stat'Range (1), stat'Range (2)) of Int_64;

    width_loc  : Positive;
    height_loc : Positive;

    function x_sample_to_bitmap (xs : Integer) return Integer is
    (Integer'Max (0, Integer'Min (width_loc - 1, (width_loc * (xs - stat'First (1))) / stat'Length (1))));

    function y_sample_to_bitmap (ys : Integer) return Integer is
    (Integer'Max (0, Integer'Min (height_loc - 1, (height_loc * (ys - stat'First (2))) / stat'Length (2))));

    factor : Long_Float;

  begin
    Open (img.file, In_File, image_name);
    if verbose then
      Put_Line ("Loading " & Name (img.file) & "...");
    end if;
    --
    GID.Load_Image_Header (img.desc, Stream (img.file).all, True);
    if verbose then
      Put_Line ("    .........v");
      Put ("    ");
    end if;
    --
    img.bitm :=
      new Bitmap
        (0 .. GID.Pixel_Width  (img.desc) - 1,
         0 .. GID.Pixel_Height (img.desc) - 1);
    Load_Raw_Image (img.desc, img.bitm.all, next_frame);
    Close (img.file);
    if verbose then
      New_Line;
    end if;

    width_loc  := img.bitm'Length (1);
    height_loc := img.bitm'Length (2);

    for xs in stat'Range (1) loop
      for ys in stat'Range (2) loop
        xp1 :=                   x_sample_to_bitmap (xs);
        xp2 := Integer'Max (xp1, x_sample_to_bitmap (xs + 1) - 1);
        yp1 :=                   y_sample_to_bitmap (ys);
        yp2 := Integer'Max (yp1, y_sample_to_bitmap (ys + 1) - 1);
        for x in xp1 .. xp2 loop
          for y in yp1 .. yp2 loop
            sum (xs, ys).r := sum (xs, ys).r + Int_64 (img.bitm (x, y).r);
            sum (xs, ys).g := sum (xs, ys).g + Int_64 (img.bitm (x, y).g);
            sum (xs, ys).b := sum (xs, ys).b + Int_64 (img.bitm (x, y).b);
          end loop;
        end loop;
        pixels_per_sample (xs, ys) := Int_64 (xp2 - xp1 + 1) * Int_64 (yp2 - yp1 + 1);
        --  Put_Line
        --    (xs'Image & ys'Image &
        --     ": " &
        --     xp1'Image & " .." & xp2'Image &
        --     ",  " &
        --     yp1'Image & " .." & yp2'Image &
        --     ": rect=" & pixels_per_sample (xs, ys)'Image);
      end loop;
    end loop;

    for xs in stat'Range (1) loop
      for ys in stat'Range (2) loop
        factor := 1.0 / Long_Float (pixels_per_sample (xs, ys) * 255);
        stat (xs, ys).average.r := Long_Float (sum (xs, ys).r) * factor;
        stat (xs, ys).average.g := Long_Float (sum (xs, ys).g) * factor;
        stat (xs, ys).average.b := Long_Float (sum (xs, ys).b) * factor;
      end loop;
    end loop;

    Free_Bitmap;

    width  := width_loc;
    height := height_loc;

  end Compute;

end Color_Statistics;
