--
--  Minimal steganography tool.
--
--  This demo is derived from mini.adb.
--

with GID;

with Ada.Calendar,
     Ada.Characters.Handling,
     Ada.Command_Line,
     Ada.Float_Text_IO,
     Ada.Numerics.Elementary_Functions,
     Ada.Streams.Stream_IO,
     Ada.Text_IO,
     Ada.Unchecked_Deallocation;

with Interfaces;

procedure Steg is

  use Ada.Characters.Handling, Ada.Streams.Stream_IO, Ada.Text_IO;

  procedure Blurb is
  begin
    Put_Line (Current_Error, "Steg * Minimal steganography tool");
    New_Line (Current_Error);
    Put_Line (Current_Error, "Syntax:");
    Put_Line (Current_Error, "  steg [e|d] <image_file_name> <data_file_name>");
    New_Line (Current_Error);
    Put_Line (Current_Error, "  (e)ncoding: converts any image file to a PPM image file, with a data");
    Put_Line (Current_Error, "              file hidden in it. The PPM image can then be converted");
    Put_Line (Current_Error, "              to a lossless-compressed format like PNG or QOI.");
    New_Line (Current_Error);
    Put_Line (Current_Error, "  (d)ecoding: extracts a data file hidden in an image.");
    New_Line (Current_Error);
    Put_Line (Current_Error, "Encryption must be independently applied to the data file.");
    New_Line (Current_Error);
    Put_Line (Current_Error, "GID version " & GID.version & " dated " & GID.reference);
    Put_Line (Current_Error, "URL: " & GID.web);
    New_Line (Current_Error);
    Put ("Press return");
    Skip_Line;
  end Blurb;

  use Interfaces;

  type Byte_Array is array (Integer range <>) of Unsigned_8;
  type p_Byte_Array is access Byte_Array;
  procedure Dispose is new Ada.Unchecked_Deallocation (Byte_Array, p_Byte_Array);

  img_buf : p_Byte_Array := null;

  --  Load image into a 24-bit truecolor RGB raw bitmap (for a PPM output)
  procedure Load_Raw_Image
    (image      : in out GID.Image_Descriptor;
     buffer     : in out p_Byte_Array;
     next_frame :    out Ada.Calendar.Day_Duration)
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
    pragma Warnings (off, alpha);  --  alpha is ignored
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

    procedure Load_image is
      new GID.Load_Image_Contents
        (Primary_Color_Range, Set_X_Y,
         Put_Pixel, Feedback, GID.fast);

  begin
    Dispose (buffer);
    buffer := new Byte_Array (0 .. 3 * image_width * image_height - 1);
    Load_image (image, next_frame);
  end Load_Raw_Image;

  procedure Dump_PPM (name : String; i : GID.Image_Descriptor) is
    f : Ada.Streams.Stream_IO.File_Type;
    ppm_name : constant String := name & ".ppm";
  begin
    Create (f, Out_File, ppm_name);
    Put_Line (Current_Error, "Creating PPM image, name = " & ppm_name & " ...");
    --  PPM Header:
    String'Write
      (Stream (f),
       "P6 " &
       Integer'Image (GID.Pixel_Width (i)) &
       Integer'Image (GID.Pixel_Height (i)) & " 255" & ASCII.LF);
    --  PPM raw BGR image:
    Byte_Array'Write (Stream (f), img_buf.all);
    --  ^ slow on some Ada systems, see to_bmp to have a faster version
    Close (f);
  end Dump_PPM;

  procedure Show_Sizes (data_size, available_size : Unsigned_64) is
    factor : constant Float := Float (data_size) / Float (available_size);
    use Ada.Float_Text_IO;
  begin
    Put (Current_Error, "Data size:" & data_size'Image & ", using ");
    Put (Current_Error, 100.0 * factor, 0, 3, 0);
    Put_Line (Current_Error,  "% of image data");
  end Show_Sizes;

  type Operation is (encoding, decoding);

  Data_too_large : exception;

  procedure Process (image_name, data_name : String; op : Operation) is
    f_im, f_dt : Ada.Streams.Stream_IO.File_Type;
    available_size : Unsigned_64;
    --
    procedure Encode (info_width, info_height : Natural) is
      idx : Natural := img_buf'Last;
      --  ^ Start with buffer's end (the image's bottom), with the hope it
      --    is "noisier": often there is a blue sky, or some smooth
      --    background like that, on the image's top...
      --
      procedure Encode_Byte (b : Unsigned_8) is
      begin
        --  One pixel contains one data byte.
        --  Blue:
        img_buf (idx) := (img_buf (idx) and 2#1111_1100#) or (b and 2#0000_0011#);
        idx := idx - 1;
        --  Green:
        img_buf (idx) := (img_buf (idx) and 2#1111_1000#) or Shift_Right (b and 2#0001_1100#, 2);
        idx := idx - 1;
        --  Red:
        img_buf (idx) := (img_buf (idx) and 2#1111_1000#) or Shift_Right (b, 5);
        idx := idx - 1;
      end Encode_Byte;
      --
      b : Unsigned_8;
      data_size : Unsigned_64;
      needed_size : Unsigned_64;
      factor : Float;
      use Ada.Numerics.Elementary_Functions;
      function Percents_Image (x : Float) return String is (Integer (100.0 * x)'Image & '%');
      function Suggested_Scaling_Percents return String is (Percents_Image (Sqrt (factor)));
    begin
      Open (f_dt, In_File, data_name);
      data_size := Unsigned_64 (Size (f_dt));
      needed_size := data_size + 8;
      factor := Float (needed_size) / Float (available_size);
      if needed_size > available_size then
        factor := factor + 0.01;  --  Adjust for avoiding exception message with exactly "100%".
        raise Data_too_large with
          "Needs around" & Percents_Image (factor) & " raw size scaling, i.e. around" &
          Suggested_Scaling_Percents & " image scaling in each of both dimensions, that is as" &
          Integer (Sqrt (factor) * Float (info_width))'Image & " x" &
          Integer (Sqrt (factor) * Float (info_height))'Image & " bitmap";
      end if;
      Show_Sizes (data_size, available_size);
      if factor < 0.98 then
        Put_Line
          (Current_Error,
           "You could still encode the data on a reduced image, scaled down to" &
           Suggested_Scaling_Percents & " in each of both dimensions");
      end if;
      for i in 1 .. 8 loop
        Encode_Byte (Unsigned_8 (data_size and 16#FF#));
        data_size := Shift_Right (data_size, 8);
      end loop;
      while not End_Of_File (f_dt) loop
        Unsigned_8'Read (Stream (f_dt), b);
        Encode_Byte (b);
      end loop;
      Close (f_dt);
    end Encode;
    --
    procedure Decode is
      idx : Natural := img_buf'Last;
      --
      procedure Decode_Byte (b : out Unsigned_8) is
      begin
        --  One pixel contains one data byte.
        --  Blue:
        b := img_buf (idx) and 2#0000_0011#;
        idx := idx - 1;
        --  Green:
        b := b + Shift_Left (img_buf (idx) and 2#0000_0111#, 2);
        idx := idx - 1;
        --  Red:
        b := b + Shift_Left (img_buf (idx) and 2#0000_0111#, 5);
        idx := idx - 1;
      end Decode_Byte;
      --
      b : Unsigned_8;
      data_size : Unsigned_64 := 0;
    begin
      for i in 0 .. 7 loop
        Decode_Byte (b);
        data_size := data_size + Shift_Left (Unsigned_64 (b), i * 8);
      end loop;
      if data_size * 3 > Unsigned_64 (idx + 1) then
        raise Data_too_large with
          "Data size (as stored in the image) exceeds the image's capacity." &
          ASCII.LF &
          "It seems that either the steganography has been" &
          " altered, or that there is no steganography at all.";
      end if;
      Show_Sizes (data_size, available_size);
      Create (f_dt, Out_File, data_name);
      for i in 1 .. data_size loop
        Decode_Byte (b);
        Unsigned_8'Write (Stream (f_dt), b);
      end loop;
      Close (f_dt);
    end Decode;
    --
    img : GID.Image_Descriptor;
    up_name : constant String := To_Upper (image_name);
    --
    next_frame : Ada.Calendar.Day_Duration;
  begin
    --
    --  Load the image in its original format
    --
    Open (f_im, In_File, image_name);
    Put_Line (Current_Error, "Processing " & image_name & "...");
    --
    GID.Load_Image_Header
      (img,
       Stream (f_im).all,
       try_tga =>
         image_name'Length >= 4 and then
         up_name (up_name'Last - 3 .. up_name'Last) = ".TGA");
    Put_Line (Current_Error, ".........v.........v");
    --
    Load_Raw_Image (img, img_buf, next_frame);
    New_Line (Current_Error);
    Close (f_im);
    available_size := img_buf'Length / 3;  --  1 byte per pixel;
    case op is
      when encoding =>
        Put_Line (Current_Error, "Encoding data...");
        Encode (GID.Pixel_Width (img), GID.Pixel_Height (img));
        Dump_PPM (image_name, img);  --  Output encoded image
      when decoding =>
        Put_Line (Current_Error, "Decoding data...");
        Decode;
    end case;
  end Process;

  op : Operation;

  use Ada.Command_Line;

begin
  if Argument_Count /= 3 then
    Blurb;
    return;
  end if;
  if To_Lower (Argument (1)) = "e" then
    op := encoding;
  elsif To_Lower (Argument (1)) = "d" then
    op := decoding;
  else
    Blurb;
    return;
  end if;
  Process (Argument (2), Argument (3), op);
end Steg;
