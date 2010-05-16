with GID.Color_tables;

with Ada.Text_IO, Ada.Exceptions;

package body GID.Decoding_GIF is

  ----------
  -- Load --
  ----------

  procedure Load (image: in Image_descriptor) is

    local: Image_descriptor;
    -- With GIF, each frame is a local image with an eventual
    -- palette, different dimensions, etc. ...

    use Ada.Exceptions;

    type GIFDescriptor is record
      ImageLeft,
      ImageTop,
      ImageWidth,
      ImageHeight : U16;
      Depth       : U8;
    end record;

    -- For loading from the GIF file
    Descriptor     : GIFDescriptor;
    Temp           : U8;

    -- Coordinates
    X, tlX, brX : Natural;
    Y, tlY, brY : Natural;

    -- GIF data is stored in blocks of a certain size
    BlockSize      : Natural;

    -- The string table
    Prefix       : array ( 0..4096 ) of Natural;
    Suffix       : array ( 0..4096 ) of Natural;
    OutCode      : array ( 0..1024 ) of Natural;
    FirstFree,
      FreeCode     : Natural;

    -- All the code information
    InitCodeSize,
      CodeSize     : Natural;
    Code,
      OldCode,
      MaxCode      : Natural;

    -- Special codes
    ClearCode,
      EOICode      : Natural;

    -- Used while reading the codes
    BitsIn       : U8;

    -- Interlaced images
    Interlaced     : Boolean;
    Interlace_pass : Natural range 1..4:= 1;
    Span           : Natural:= 7;

    Separator :  Character ;
    -- block separator - "," for image, "!" for exten.

    subtype Color_type is U8;

    -- Colour information
    New_num_of_colours : Natural;
    Pixel_mask : Color_type;
    -- This is in case of local colour map
    BitsPerPixel  : Natural;

    -- Local procedure to read the next code from the file
    procedure ReadCode is
      Bit_Mask: Natural:= 1;
    begin
      Code := 0;
      -- Read the code, bit by bit
      for  Counter  in reverse  0..CodeSize - 1  loop
        -- Next bit
        BitsIn:= BitsIn + 1;

        -- Maybe, a new byte needs to be loaded with a further 8 bits
        if BitsIn = 9 then
          U8'Read( image.stream, Temp );
          BitsIn := 1;
        end if;

        -- Add the current bit to the code
        if  (Temp  and  1) > 0 then
          Code:= Code + Bit_Mask;
        end if;
        Bit_Mask := Bit_Mask * 2;
        Temp     := Temp / 2;
      end loop;
    end ReadCode;

    procedure Pixel_with_palette(b: U8) is
      pragma Inline(Pixel_with_palette);
    begin
      case Primary_color_range'Modulus is
        when 256 =>
          Put_Pixel(
            Primary_color_range(local.palette(Integer(b)).Red),
            Primary_color_range(local.palette(Integer(b)).Green),
            Primary_color_range(local.palette(Integer(b)).Blue),
            255
          );
        when 65_536 =>
          Put_Pixel(
            256 * Primary_color_range(local.palette(Integer(b)).Red),
            256 * Primary_color_range(local.palette(Integer(b)).Green),
            256 * Primary_color_range(local.palette(Integer(b)).Blue),
            65_535
          );
        when others =>
          raise invalid_primary_color_range;
      end case;
    end Pixel_with_palette;

    -- Local procedure to draw a pixel
    procedure NextPixel( code: Natural) is
      c : constant Color_Type:= Color_Type(code) and Pixel_mask;
    begin
      -- Actually draw the pixel on screen buffer
      if X < image.width then
        if Interlaced then
          for i in reverse 0..Span loop
            if Y+i < image.height then
              Set_X_Y(X, Y+i);
              Pixel_with_palette(c);
            end if;
          end loop;
        elsif Y < image.height then
          Pixel_with_palette(c);
        end if;
      end if;

      -- Move on to next pixel
      X:= X + 1;

      -- Or next row, if necessary
      if X = brX then
        X:= tlX;
        if Interlaced then
          case Interlace_pass is
            when 1 =>
              Y:= Y + 8;
              if Y >= brY then Y:= 4; Interlace_pass:= 2; Span:= 3; end if;
            when 2 =>
              Y:= Y + 8;
              if Y >= brY then Y:= 2; Interlace_pass:= 3; Span:= 1; end if;
            when 3 =>
              Y:= Y + 4;
              if Y >= brY then Y:= 1; Interlace_pass:= 4; Span:= 0; end if;
            when 4 =>
              Y:= Y + 2;
          end case;
        else
          Y:= Y + 1;
          Feedback((Y*100)/image.height);
          if Y < image.height then
            Set_X_Y(X, Y);
          end if;
        end if;
      end if;
    end NextPixel;

    -- Local function to output a string. Returns the first character.
    function OutString (CurCode_1: Natural) return Natural is
      CurCode  : Natural:= CurCode_1;
      OutCount : Natural;

    begin
      -- If it's a single character, output that
      if CurCode < New_num_of_colours then
        NextPixel( CurCode );
      else
        OutCount := 0;

        -- Store the string, which ends up in reverse order
        loop
          OutCode (OutCount) := Suffix (CurCode);
          OutCount:= OutCount + 1;
          CurCode := Prefix (CurCode);
          exit when  CurCode < New_num_of_colours;
        end loop;

        -- Add the last character
        OutCode (OutCount) := CurCode;
        OutCount:= OutCount + 1;

        -- Output all the string, in the correct order
        loop
          OutCount:= OutCount - 1;
          NextPixel( OutCode(OutCount) );
          exit when  OutCount = 0;
        end loop;

      end if;
      -- Return 1st character
      return CurCode;
    end OutString;

  begin
    New_num_of_colours:= 2**image.bits_per_pixel;
    Pixel_mask:= Color_Type(New_num_of_colours - 1);

    -- Skip all extensions until finding an image descriptor

    loop
      Character'Read( image.stream, Separator );
      if full_trace then
        Ada.Text_IO.Put("GIF sep [" & separator & ']');
      end if;
      case Separator is
        when ',' => exit; -- image descriptor will begin !
        when '!' =>
          U8'Read( image.stream, temp ); -- skip extension identifier byte
          loop
            U8'Read( image.stream, temp ); -- load sub-block length byte
            exit when temp = 0;   -- null sub-block -> the end!
            for i in reverse 1..temp loop
              U8'Read( image.stream, temp ); -- load sub-block byte
            end loop;
          end loop;
        when others =>
          Raise_exception(
            error_in_image_data'Identity,
            "Unknown GIF separator: " & separator
          );
      end case;
    end loop;

    -- Load the image descriptor
    GIFDescriptor'Read( image.stream, Descriptor );
    -- !! endianess !!

    -- Get image corner coordinates
    tlX := Natural(Descriptor.ImageLeft);
    tlY := Natural(Descriptor.ImageTop);
    brX := tlX + Natural(Descriptor.ImageWidth);
    brY := tlY + Natural(Descriptor.ImageHeight);

    Interlaced:= (Descriptor.Depth and 64) = 64;

    -- Local colour table
    if (Descriptor.Depth and 128) = 128 then
      -- Get amount of colours in image
      BitsPerPixel := 1 + Natural(Descriptor.Depth and 7);
      New_num_of_colours:= 2 ** BitsPerPixel;
      Pixel_mask:= Color_Type(New_num_of_colours - 1);
      -- !!! pixel_mask for # of colours
      Color_tables.Load_palette(local);
    else
      -- Just copy main palette
      local.palette:= new Color_table'(image.palette.all);
    end if;

    -- Get initial code size
    U8'Read( image.stream, temp );
    CodeSize := Natural(temp);

    -- GIF data is stored in blocks, it is useful to know the size
    U8'Read( image.stream, temp );
    BlockSize:= Natural(temp);
    -- !! buffering !!

    -- Special codes used in the GIF spec
    ClearCode        := 2 ** CodeSize;     -- Code to reset
    EOICode          := ClearCode + 1;     -- End of file

    -- Initialize the string table
    FirstFree        := ClearCode + 2;     -- Strings start here
    FreeCode         := FirstFree;         -- Strings can be added here

    -- Initial size of the code and its maximum value
    CodeSize      := CodeSize + 1;
    InitCodeSize  := CodeSize;
    MaxCode       := 2 ** CodeSize;

    BitsIn := 8;

    -- Start at top left of image
    X := Natural(Descriptor.ImageLeft);
    Y := Natural(Descriptor.ImageTop);
    Set_X_Y(X, Y);

    loop
      -- Read next code
      ReadCode;

      -- If it's an End-Of-Information code, stop processing
      exit when Code = EOICode;

      -- If it's a clear code...
      if  Code = ClearCode then
        -- Clear the string table
        FreeCode := FirstFree;

        -- Set the code size to initial values
        CodeSize := InitCodeSize;
        MaxCode  := 2 ** CodeSize;

        -- The next code may be read
        ReadCode;
        OldCode := Code;

        -- Set pixel
        NextPixel( Code );

        -- Other codes
      else

        -- If the code is already in the string table, it's string is displayed,
        --   and the old string followed by the new string's first character is
        --   added to the string table.
        if  Code < FreeCode then
          Suffix (FreeCode) := OutString (Code);
        else
          -- If it is not already in the string table, the old string followed by
          --  the old string's first character is added to the string table and
          --  displayed.
          Suffix (FreeCode) := OutString (OldCode);
          NextPixel( Suffix (FreeCode) );
        end if;

        -- Finish adding to string table
        Prefix (FreeCode) := OldCode;
        FreeCode:= FreeCode + 1;

        -- If the code size needs to be adjusted, do so
        if  (FreeCode >= MaxCode)  and then (CodeSize < 12) then
          CodeSize:= CodeSize + 1;
          MaxCode := MaxCode  * 2;
        end if;

        -- The current code is now old
        OldCode := Code;
      end if;

      exit when  Code = EOICode;
    end loop;
  end Load;

end GID.Decoding_GIF;
