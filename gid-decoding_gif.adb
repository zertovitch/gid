with GID.Buffering, GID.Color_tables;

with Ada.Exceptions, Ada.IO_Exceptions, Ada.Streams, Ada.Text_IO;

package body GID.Decoding_GIF is

  generic
    type Number is mod <>;
  procedure Read_Intel_x86_number(
    n    :    out Number;
    from : in     Stream_Access
  );
    pragma Inline(Read_Intel_x86_number);

  procedure Read_Intel_x86_number(
    n    :    out Number;
    from : in     Stream_Access
  )
  is
    b: U8;
    m: Number:= 1;
  begin
    n:= 0;
    for i in 1..Number'Size/8 loop
      U8'Read(from, b);
      n:= n + m * Number(b);
      m:= m * 256;
    end loop;
  end Read_Intel_x86_number;

  procedure Read_Intel is new Read_Intel_x86_number( U16 );

  ----------
  -- Load --
  ----------

  procedure Load (
    image     : in out Image_descriptor;
    next_frame:    out Ada.Calendar.Day_Duration
  )
  is
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

    -- GIF data is stored in blocks and sub-blocks.
    -- We initialize BlockRead and BlockSize to force
    -- reading and buffering the next sub-block
    BlockSize      : Natural:= 0;
    BlockRead      : Natural:= 0;

    -- The string table
    Prefix       : array ( 0..4096 ) of Natural;
    Suffix       : array ( 0..4096 ) of Natural;
    OutCode      : array ( 0..1024 ) of Natural;
    FirstFree,
      FreeCode     : Natural;

    -- All the code information
    subtype Code_size_range is Natural range 2..12;
    InitCodeSize,
      CodeSize     : Code_size_range;
    Code,
      OldCode,
      MaxCode      : Natural;
    -- Special codes (GIF only, not LZW)
    ClearCode,
      EOICode      : Natural;

    -- Used while reading the codes
    BitsIn       : U8;

    subtype Color_type is U8;

    Interlaced     : Boolean;
    Local_palette  : Boolean;
    Transparency   : Boolean:= False;
    Transp_color   : Color_type:= 0;
    Interlace_pass : Natural range 1..4:= 1;
    Span           : Natural:= 7;

    Separator :  Character ;
    -- block separator - "," for image, "!" for exten.

    -- Colour information
    New_num_of_colours : Natural;
    Pixel_mask : Color_type;
    -- This is in case of local colour map
    BitsPerPixel  : Natural;

    block_buffer : Byte_array( 1..256 );

    function Read_Byte return U8 is
      b: U8;
      use Ada.Streams;
      Last_Read: Stream_Element_Offset;
    begin
      if BlockRead >= BlockSize then
        U8'Read( image.stream, b);
        BlockSize:= Natural(b);
        BlockRead:= 0;
        -- Pre-read the block
        --
        -- We could do simply:
        --
        -- >>> Byte_Array'Read(image.stream, block_buffer(1..BlockSize));
        --
        -- but main Ada implementations (GNAT, ObjectAda; early 2010)
        -- are very slow on this. So we use Ada.Streams.
        -- Speedup for overall GIF decompression is 2x !
        --
        if GID.Buffering.is_mapping_possible then
          declare
            SE_Buffer_mapped: Stream_Element_Array (1..Stream_Element_Offset(BlockSize));
            -- direct mapping: buffer = SE_Buffer_mapped
            for SE_Buffer_mapped'Address use block_buffer'Address;
            pragma Import (Ada, SE_Buffer_mapped);
          begin
            Read(image.stream.all, SE_Buffer_mapped, Last_Read);
          end;
        else
          declare
            SE_Buffer_mapped: Stream_Element_Array (1..Stream_Element_Offset(BlockSize));
          begin
            Read(image.stream.all, SE_Buffer_mapped, Last_Read);
            for i in 1..Integer(Last_Read) loop
              block_buffer(i):= U8(SE_Buffer_mapped(Stream_Element_Offset(i-block_buffer'First)+SE_buffer_mapped'First));
            end loop;
          end;
        end if;
        if Integer(Last_Read) < BlockSize then
          raise Ada.IO_Exceptions.End_Error;
        end if;
      end if;
      BlockRead:= BlockRead + 1;
      return block_buffer(BlockRead);
    end Read_Byte;

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
          Temp:= Read_Byte;
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
      --  if Integer(b) >= local.palette'Length then
      --    --  Put_Pixel(0,0,0, 255);
      --    --  return;
      --    Raise_exception(
      --      error_in_image_data'Identity,
      --      "Bad palette index: " & b'img & local.palette'Length'img & pixel_mask'img
      --    );
      --  end if;
      if Transparency and b = Transp_color then
        Put_Pixel(0,0,0, 0);
        return;
      end if;
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
      c : constant Color_Type:= Color_type(U32(code) and U32(Pixel_mask));
    begin
      -- Actually draw the pixel on screen buffer
      if X < image.width then
        if Interlaced and mode = nice then
          for i in reverse 0..Span loop
            if Y+i < image.height then
              Set_X_Y(X, image.height - (Y+i) - 1);
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
          Feedback((Interlace_pass*100)/4);
          if mode = fast and then Y < image.height then
            Set_X_Y(X, image.height - Y - 1);
          end if;
        else
          Y:= Y + 1;
          if Y < image.height then
            Set_X_Y(X, image.height - Y - 1);
          end if;
          Feedback((Y*100)/image.height);
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

    procedure Skip_sub_blocks is
    begin
       sub_blocks_sequence:
       loop
        U8'Read( image.stream, temp ); -- load sub-block length byte
        exit sub_blocks_sequence when temp = 0;
        -- null sub-block = end of sub-block sequence
        for i in 1..temp loop
          U8'Read( image.stream, temp ); -- load sub-block byte
        end loop;
      end loop sub_blocks_sequence;
    end Skip_sub_blocks;

    label: U8;
    delay_frame: U16;
    c: Character;

  begin
    next_frame:= 0.0;
    -- Scan various GIF blocks, until finding an image
    loop
      Character'Read( image.stream, Separator );
      if full_trace then
        Ada.Text_IO.Put(
          "GIF separator [" & separator &
          "][" & integer'image(character'pos(separator)) & ']'
        );
      end if;
      case Separator is
        when ',' => -- 16#2C#
          exit;
          -- Image descriptor will begin
          -- See: 20. Image Descriptor
        when ';' => -- 16#3B#
          if full_trace then
            Ada.Text_IO.Put(" - End of GIF");
          end if;          image.next_frame:= 0.0;
          next_frame:= image.next_frame;
          return; -- End of GIF image
        when '!' => -- 16#21# Extensions
          if full_trace then
            Ada.Text_IO.Put(" - Extension");
          end if;
          U8'Read( image.stream, label );
          case label is
            when 16#F9# => -- See: 23. Graphic Control Extension
              if full_trace then
                Ada.Text_IO.Put_Line(" - Graphic Control Extension");
              end if;
              U8'Read( image.stream, temp );
              if temp /= 4 then
                Raise_exception(
                  error_in_image_data'Identity,
                  "GIF: error in Graphic Control Extension"
                );
              end if;
              U8'Read( image.stream, temp );
              --  Reserved                      3 Bits
              --  Disposal Method               3 Bits
              --  User Input Flag               1 Bit
              --  Transparent Color Flag        1 Bit
              Transparency:= (temp and 1) = 1;
              Read_Intel(delay_frame, image.stream);
              image.next_frame:=
                image.next_frame + Ada.Calendar.Day_Duration(delay_frame) / 100.0;
              next_frame:= image.next_frame;
              U8'Read( image.stream, Transp_color );
              -- zero sub-block:
              U8'Read( image.stream, temp );
            when 16#FE# => -- See: 24. Comment Extension
              if full_trace then
                Ada.Text_IO.Put_Line(" - Comment Extension");
                sub_blocks_sequence:
                loop
                  U8'Read( image.stream, temp ); -- load sub-block length byte
                  exit sub_blocks_sequence when temp = 0;
                  -- null sub-block = end of sub-block sequence
                  for i in 1..temp loop
                    Character'Read( image.stream, c );
                    Ada.Text_IO.Put(c);
                  end loop;
                end loop sub_blocks_sequence;
                Ada.Text_IO.New_Line;
              else
                Skip_sub_blocks;
              end if;
            when 16#01# => -- See: 25. Plain Text Extension
              if full_trace then
                Ada.Text_IO.Put_Line(" - Plain Text Extension");
              end if;
              Skip_sub_blocks;
            when 16#FF# => -- See: 26. Application Extension
              if full_trace then
                Ada.Text_IO.Put_Line(" - Application Extension");
              end if;
              Skip_sub_blocks;
            when others =>
              if full_trace then
                Ada.Text_IO.Put_Line(" - Unused:" & U8'Image(label));
              end if;
              Skip_sub_blocks;
          end case;
        when others =>
          Raise_exception(
            error_in_image_data'Identity,
            "Unknown GIF separator: " & separator
          );
      end case;
    end loop;

    -- Load the image descriptor
    Read_Intel(Descriptor.ImageLeft,   image.stream);
    Read_Intel(Descriptor.ImageTop,    image.stream);
    Read_Intel(Descriptor.ImageWidth,  image.stream);
    Read_Intel(Descriptor.ImageHeight, image.stream);
    U8'Read(image.stream, Descriptor.Depth);

    -- Get image corner coordinates
    tlX := Natural(Descriptor.ImageLeft);
    tlY := Natural(Descriptor.ImageTop);
    brX := tlX + Natural(Descriptor.ImageWidth);
    brY := tlY + Natural(Descriptor.ImageHeight);

    --  Local Color Table Flag        1 Bit
    --  Interlace Flag                1 Bit
    --  Sort Flag                     1 Bit
    --  Reserved                      2 Bits
    --  Size of Local Color Table     3 Bits
    --
    Interlaced:= (Descriptor.Depth and 64) = 64;
    Local_palette:= (Descriptor.Depth and 128) = 128;
    local.format:= GIF;
    local.stream:= image.stream;
    if Local_palette then
      -- Get amount of colours in image
      BitsPerPixel := 1 + Natural(Descriptor.Depth and 7);
      New_num_of_colours:= 2 ** BitsPerPixel;
      -- 21. Local Color Table
      local.palette:= new Color_Table(0..New_num_of_colours-1);
      Color_tables.Load_palette(local);
    elsif image.palette = null then
      Raise_exception(
        error_in_image_data'Identity,
        "GIF: neither local, nor global palette"
      );
    else
      -- Use global palette
      New_num_of_colours:= 2 ** image.subformat_id;
      -- usually <= 2** image.bits_per_pixel
      -- Just copy main palette
      local.palette:= new Color_table'(image.palette.all);
    end if;
    Pixel_mask:= Color_Type(New_num_of_colours - 1);

    if full_trace then
      Ada.Text_IO.Put_Line(
        " - Image, interlaced: " & Boolean'Image(Interlaced) &
        "; local palette: " & Boolean'Image(Local_palette) &
        "; transparency: " & Boolean'Image(Transparency) &
        "; transparency index:" & U8'Image(Transp_color)
      );
    end if;

    -- Get initial code size
    U8'Read( image.stream, temp );
    if Natural(temp) not in Code_size_range then
      Raise_exception(
        error_in_image_data'Identity,
        "GIF: wrong LZW code size (must be in 2..12), is" &
        U8'Image(temp)
      );
    end if;
    CodeSize := Natural(temp);

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
    Set_X_Y(X, image.height - Y - 1);

    LZW_decompression:
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
        if Code < FreeCode then
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
        if FreeCode >= MaxCode and then CodeSize < 12 then
          CodeSize:= CodeSize + 1;
          MaxCode := MaxCode  * 2;
        end if;
        -- The current code is now old
        OldCode := Code;
      end if;
      exit when Code = EOICode;
    end loop LZW_decompression;
    U8'Read( image.stream, temp ); -- zero-size sub-block
  end Load;

end GID.Decoding_GIF;
