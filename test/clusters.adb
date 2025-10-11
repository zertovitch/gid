--  Clusters
--
--  This program distribute pixels into different clusters and
--  optimize them using the k-means method in the color space.
--
--  This program is derived from all_rgb.adb.

with GID;

with Color_Distances;
with Dumb_PNG;

with Ada.Calendar,
     Ada.Characters.Handling,
     Ada.Command_Line,
     Ada.Streams.Stream_IO,
     Ada.Text_IO,
     Ada.Unchecked_Deallocation;

with Interfaces;

procedure Clusters is

  use Ada.Streams.Stream_IO, Ada.Text_IO;

  procedure Blurb is
  begin
    Put_Line (Standard_Error, "Clusters");
    New_Line (Standard_Error);
    Put_Line (Standard_Error, "Demo for the GID (Generic Image Decoder) package");
    Put_Line (Standard_Error, "Package version " & GID.version & " dated " & GID.reference);
    Put_Line (Standard_Error, "URL: " & GID.web);
    New_Line (Standard_Error);
    Put_Line (Standard_Error, "Syntax:");
    Put_Line (Standard_Error, "clusters [option] <image_1> [[option] <image_2>...]");
    New_Line (Standard_Error);
    Put_Line (Standard_Error, "Options:");
    Put_Line (Standard_Error, "  -lP: set Lp distance (l1, l2, l3, linf); default: -l2");
    Put_Line (Standard_Error, "  -iN: set number of iterations; default: 6");
    Put_Line (Standard_Error, "  -c:  partition the initial clusters in the color space (default)");
    Put_Line (Standard_Error, "  -m:  partition the initial clusters in the image space");
    New_Line (Standard_Error);
    Put_Line (Standard_Error, "Options for initial clustering in color space");
    Put_Line (Standard_Error, "  -rN: set number of red segments (default: 2)");
    Put_Line (Standard_Error, "  -gN: set number of green segments (default: 2)");
    Put_Line (Standard_Error, "  -bN: set number of blue segments (default: 2)");
    New_Line (Standard_Error);
    Put_Line (Standard_Error, "Options for initial clustering in image space");
    Put_Line (Standard_Error, "  -xN: set number of horizontal segments (default: 10)");
    Put_Line (Standard_Error, "  -yN: set number of vertical segments (default: 10)");
    New_Line (Standard_Error);
  end Blurb;

  use Interfaces;

  type Real is digits 15;

  package Color_Distances_8_Bit is
    new Color_Distances (Unsigned_8, Real);

  use Color_Distances_8_Bit;

  type Bitmap is array (Integer range <>, Integer range <>) of RGB;
  type p_Bitmap is access Bitmap;
  procedure Dispose is new Ada.Unchecked_Deallocation (Bitmap, p_Bitmap);

  --  Load image into a 24-bit truecolor RGB raw bitmap.
  procedure Load_Raw_Image
    (image      : in out GID.Image_Descriptor;
     bmp        : in out Bitmap;
     next_frame :    out Ada.Calendar.Day_Duration)
  is
    subtype Primary_color_range is Unsigned_8;
    pos_x, pos_y, max_y : Natural;

    procedure Set_X_Y (x, y : Natural) is
    begin
      pos_x := x;
      pos_y := y;
    end Set_X_Y;

    procedure Put_Pixel
      (red, green, blue : Primary_color_range;
       alpha            : Primary_color_range)
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
  end Load_Raw_Image;

  type Initial_Clustering_Type is (color_space, image_space);

  procedure Dump_PNG (name : String; bmp : Bitmap) is
    f : Ada.Streams.Stream_IO.File_Type;
    use Dumb_PNG;
    rgb_flat_map : p_Byte_Array;
    idx : Integer := 1;
  begin
    Create (f, Out_File, name & ".png");
    rgb_flat_map := new Byte_Array (1 .. 3 * bmp'Length (1) * bmp'Length (2));
    for y in bmp'Range (2) loop
      for x in bmp'Range (1) loop
        rgb_flat_map (idx) := bmp (x, y).r; idx := idx + 1;
        rgb_flat_map (idx) := bmp (x, y).g; idx := idx + 1;
        rgb_flat_map (idx) := bmp (x, y).b; idx := idx + 1;
      end loop;
    end loop;
    Dumb_PNG.Write (rgb_flat_map.all, packed, bmp'Length (1), bmp'Length (2), Stream (f).all);
    Close (f);
  end Dump_PNG;

  generic
    similarity_dist_choice    : Dist_Type;
  procedure Transform
    (src                       : in     Bitmap;
     dst                       :    out Bitmap;
     initial_clustering_choice : in     Initial_Clustering_Type;
     segm_rx, segm_gy, segm_bz : in     Positive;  --  Segments in color or in image space.
     tr_iterations             : in     Integer;
     prefix                    : in     String);

  procedure Transform
    (src                       : in     Bitmap;
     dst                       :    out Bitmap;
     initial_clustering_choice : in     Initial_Clustering_Type;
     segm_rx, segm_gy, segm_bz : in     Positive;
     tr_iterations             : in     Integer;
     prefix                    : in     String)
  is

    function M_Funct_Dist_Lx is new Distance_No_Root_2 (similarity_dist_choice);

    subtype Cluster_X is Integer range 1 .. segm_rx;
    subtype Cluster_Y is Integer range 1 .. segm_gy;
    subtype Cluster_Z is Integer range 1 .. segm_bz;

    centroid : array (Cluster_X, Cluster_Y, Cluster_Z) of RGB_Real;

    used_cluster : array (Cluster_X, Cluster_Y, Cluster_Z) of Boolean :=
      (others => (others => (others => False)));

    type Cluster_Id is record
      x : Cluster_X;
      y : Cluster_Y;
      z : Cluster_Z;
    end record;

    --  Associate each pixel with a cluster.
    type Attrib_Type is array (src'Range (1), src'Range (2)) of Cluster_Id;
    type Attrib_Access is access Attrib_Type;
    procedure Dispose is new Ada.Unchecked_Deallocation (Attrib_Type, Attrib_Access);

    attrib : Attrib_Access := new Attrib_Type;

    pix : RGB_Real;
    id : Cluster_Id;

    procedure Initial_Clustering is
    begin
      if initial_clustering_choice = image_space then
        if segm_bz /= 1 then
          raise Constraint_Error;
        end if;
      end if;

      for x in src'Range (1) loop
        for y in src'Range (2) loop

          case initial_clustering_choice is

            when color_space =>

              id :=
                (x => 1 + (Integer (src (x, y).r) * Cluster_X'Last) / 256,
                 y => 1 + (Integer (src (x, y).g) * Cluster_Y'Last) / 256,
                 z => 1 + (Integer (src (x, y).b) * Cluster_Z'Last) / 256);

            when image_space =>

              id :=
                (x => 1 + (x * Cluster_X'Last) / src'Length (1),
                 y => 1 + (y * Cluster_Y'Last) / src'Length (2),
                 z => 1);

        end case;

        attrib (x, y) := id;
        used_cluster (id.x, id.y, id.z) := True;

        end loop;
      end loop;

    end Initial_Clustering;

    procedure Allocate_K_Means (stable : out Boolean) is
      best_cluster : Cluster_Id;
      best_dist, dist : Real;
      defectors : Natural := 0;
    begin

      for x in src'Range (1) loop
        for y in src'Range (2) loop
          best_dist := Real'Last;

          for rx in Cluster_X loop
            for gy in Cluster_Y loop
              for bz in Cluster_Z loop

                if used_cluster (rx, gy, bz) then
                  dist := M_Funct_Dist_Lx (src (x, y), centroid (rx, gy, bz));
                  if dist < best_dist then
                    best_dist := dist;
                    best_cluster := (rx, gy, bz);
                  end if;
                end if;

              end loop;
            end loop;
          end loop;

          if attrib (x, y) /= best_cluster then
            attrib (x, y) := best_cluster;
            defectors := defectors + 1;
          end if;

        end loop;
      end loop;

      stable := defectors = 0;

      Put_Line (Standard_Error,
        "Defectors:" & defectors'Image &
        " pixels have joined another cluster.");

    end Allocate_K_Means;

    procedure Calculate_Centroids is
      sum_r, sum_g, sum_b, counter : Natural;
      denom : Real;
    begin

      for rx in Cluster_X loop
        for gy in Cluster_Y loop
          for bz in Cluster_Z loop

            counter := 0;
            sum_r := 0;
            sum_g := 0;
            sum_b := 0;

            for x in src'Range (1) loop
              for y in src'Range (2) loop
                if attrib (x, y) = (rx, gy, bz) then
                  sum_r := sum_r + Natural (src (x, y).r);
                  sum_g := sum_g + Natural (src (x, y).g);
                  sum_b := sum_b + Natural (src (x, y).b);
                  counter := counter + 1;
                end if;
              end loop;
            end loop;

            if counter = 0 then
              used_cluster (id.x, id.y, id.z) := False;
            else
              denom := 1.0 / Real (counter);
              centroid (rx, gy, bz) :=
                (Real (sum_r) * denom,
                 Real (sum_g) * denom,
                 Real (sum_b) * denom);
            end if;

          end loop;
        end loop;
      end loop;

    end Calculate_Centroids;

    procedure Show_Stats (title : String) is
      total : Natural := 0;
    begin
      for rx in Cluster_X loop
        for gy in Cluster_Y loop
          for bz in Cluster_Z loop
            if used_cluster (rx, gy, bz) then
              total := total + 1;
            end if;
          end loop;
        end loop;
      end loop;
      Put_Line (Standard_Error,
        "Clusters in use at stage: " & title & ':' & total'Image &
        " of" & Integer'Image (segm_rx * segm_gy * segm_bz));
    end Show_Stats;

    procedure Dump_PNG_Step (iter : Natural) is
      iter_img : constant String := Integer'Image (iter);
      sx_img : constant String := Integer'Image (segm_rx);
      sy_img : constant String := Integer'Image (segm_gy);
      sz_img : constant String := Integer'Image (segm_bz);
    begin
      Dump_PNG
        (prefix & '_' &
         Dist_Type'Image (similarity_dist_choice) & '_' &
         sx_img (sx_img'First + 1 .. sx_img'Last) & '_' &
         sy_img (sy_img'First + 1 .. sy_img'Last) & '_' &
         sz_img (sz_img'First + 1 .. sz_img'Last) & '_' &
         iter_img (iter_img'First + 1 .. iter_img'Last),
         dst);
    end Dump_PNG_Step;

    procedure Calculate_Centroids_and_Dump (iter : Natural) is
    begin
      Calculate_Centroids;
      Show_Stats ("iteration" & iter'Image);

      for x in src'Range (1) loop
        for y in src'Range (2) loop
          id := attrib (x, y);
          pix := centroid (id.x, id.y, id.z);
          dst (x, y) :=
            (Unsigned_8 (pix.r),
             Unsigned_8 (pix.g),
             Unsigned_8 (pix.b));
        end loop;
      end loop;

      Dump_PNG_Step (iter);
    end Calculate_Centroids_and_Dump;

    stable : Boolean;

  begin
    Initial_Clustering;
    Show_Stats ("Initial clustering");

    Calculate_Centroids_and_Dump (0);
    for i in 1 .. tr_iterations loop
      Allocate_K_Means (stable);
      exit when stable;
      Calculate_Centroids_and_Dump (i);
    end loop;

    Dispose (attrib);
  end Transform;

  procedure Process
    (name                  : String;
     Lx                    : Dist_Type;
     initial_clustering    : Initial_Clustering_Type;
     seg_rx, seg_gy, seg_b : Positive;
     iterations            : Integer)
  is

    use Ada.Calendar, Ada.Characters.Handling;

    f : Ada.Streams.Stream_IO.File_Type;
    i : GID.Image_Descriptor;
    up_name : constant String := To_Upper (name);
    try_tga : constant Boolean :=
      name'Length >= 4 and then
      up_name (up_name'Last - 3 .. up_name'Last) = ".TGA";

    name_prefix : constant String := name (name'First .. name'Last - 4);

    next_frame : Day_Duration := 0.0;
    T0, T1 : Time;

    procedure Transform_L1   is new Transform (L1);
    procedure Transform_L2   is new Transform (L2);
    procedure Transform_L3   is new Transform (L3);
    procedure Transform_Linf is new Transform (Linf);

    src, dst : p_Bitmap := null;

    function Allocate_Bitmap return p_Bitmap is
    (new Bitmap
        (0 .. GID.Pixel_Width  (i) - 1,
         0 .. GID.Pixel_Height (i) - 1));

  begin
    --
    --  Load the image in its original format
    --
    Open (f, In_File, name);
    Put_Line (Standard_Error, "Processing " & name & "...");
    --
    GID.Load_Image_Header (i, Stream (f).all, try_tga);
    Put_Line (Standard_Error, ".........v");
    T0 := Clock;

    src := Allocate_Bitmap;
    dst := Allocate_Bitmap;

    Load_Raw_Image (i, src.all, next_frame);
    Close (f);
    New_Line (Standard_Error);

    case Lx is
      when L1   => Transform_L1   (src.all, dst.all, initial_clustering, seg_rx, seg_gy, seg_b, iterations, name_prefix);
      when L2   => Transform_L2   (src.all, dst.all, initial_clustering, seg_rx, seg_gy, seg_b, iterations, name_prefix);
      when L3   => Transform_L3   (src.all, dst.all, initial_clustering, seg_rx, seg_gy, seg_b, iterations, name_prefix);
      when Linf => Transform_Linf (src.all, dst.all, initial_clustering, seg_rx, seg_gy, seg_b, iterations, name_prefix);
    end case;

    Dispose (src);
    Dispose (dst);
    T1 := Clock;
    New_Line (Standard_Error);
    Put_Line
      (Standard_Error,
       "Time elapsed:" & Duration'Image (T1 - T0) & " seconds.");
  end Process;

  Lx : Dist_Type := L2;
  iter : Integer := 6;
  use Ada.Command_Line;
  default_partition_color_space : constant := 2;
  default_partition_image_space : constant := 10;
  rx, gy, bz : Integer := default_partition_color_space;
  partition : Initial_Clustering_Type := color_space;

begin
  if Argument_Count = 0 then
    Blurb;
    return;
  end if;

  for i in 1 .. Argument_Count loop
    declare
      arg : constant String := Argument (i);
      function Num_Arg return Integer is
      (Integer'Value (arg (arg'First + 2 .. arg'Last)));
    begin

      if arg'Length >= 2 and then arg (arg'First) = '-' then

        case arg (arg'First + 1) is
          when 'c'       => partition := color_space;
          when 'm'       =>
            partition := image_space;
            rx := default_partition_image_space;
            gy := default_partition_image_space;
            bz := 1;

          when 'i' | 'l' | 'r' | 'g' | 'b' | 'x' | 'y' =>

            if arg'Length >= 3 then

              case arg (arg'First + 1) is
                when 'l'       => Lx := Dist_Type'Value (arg (arg'First + 1 .. arg'Last));
                when 'i'       => iter := Num_Arg;
                when 'r' | 'x' => rx := Num_Arg;
                when 'g' | 'y' => gy := Num_Arg;
                when 'b'       => bz := Num_Arg;
                when others    => null;
              end case;

            else
              Put_Line
                (Standard_Error,
                 "Option " & arg (arg'First .. arg'First + 1) & ": missing argument");
              return;
            end if;

         when others =>
           Blurb;
           return;

       end case;

      else
        Process (Argument (i), Lx, partition, rx, gy, bz, iter);
      end if;

    end;
  end loop;
end Clusters;
