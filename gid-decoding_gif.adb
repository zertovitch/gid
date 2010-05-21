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
    Descriptor : GIFDescriptor;

    -- Coordinates
    X, tlX, brX : Natural;
    Y, tlY, brY : Natural;

    -- Code information
    subtype Code_size_range is Natural range 2..12;
    CodeSize : Code_size_range;
    Code : Natural;

    subtype Color_type is U8;
    Transp_color   : Color_type:= 0;

    -- GIF data is stored in blocks and sub-blocks.
    -- We initialize block_read and block_size to force
    -- reading and buffering the next sub-block
    block_size   : Natural:= 0;
    block_read   : Natural:= 0;
    block_buffer : Byte_array( 1..256 );

    function Read_Byte return U8 is
    pragma Inline(Read_Byte);
      b: U8;
      use Ada.Streams;
      Last_Read: Stream_Element_Offset;
    begin
      if block_read >= block_size then
        U8'Read( image.stream, b);
        block_size:= Natural(b);
        block_read:= 0;
        -- Pre-read the block
        --
        -- We could do simply:
        --
        -- >>> Byte_Array'Read(image.stream, block_buffer(1..block_size));
        --
        -- but main Ada implementations (GNAT, ObjectAda; early 2010)
        -- are very slow on this. So we use Ada.Streams.
        -- Speedup for overall GIF decompression is 2x !
        --
        if GID.Buffering.is_mapping_possible then
          declare
            SE_Buffer_mapped: Stream_Element_Array (1..Stream_Element_Offset(block_size));
            -- direct mapping: buffer = SE_Buffer_mapped
            for SE_Buffer_mapped'Address use block_buffer'Address;
            pragma Import (Ada, SE_Buffer_mapped);
          begin
            Read(image.stream.all, SE_Buffer_mapped, Last_Read);
          end;
        else
          declare
            SE_Buffer_mapped: Stream_Element_Array (1..Stream_Element_Offset(block_size));
          begin
            Read(image.stream.all, SE_Buffer_mapped, Last_Read);
            for i in 1..Integer(Last_Read) loop
              block_buffer(i):= U8(SE_Buffer_mapped(Stream_Element_Offset(i-block_buffer'First)+SE_Buffer_mapped'First));
            end loop;
          end;
        end if;
        if Integer(Last_Read) < block_size then
          raise Ada.IO_Exceptions.End_Error;
        end if;
      end if;
      block_read:= block_read + 1;
      return block_buffer(block_read);
    end Read_Byte;

    -- Used while reading the codes
    bits_in : U8:= 8;
    bits_buf: U8;

    -- Local procedure to read the next code from the file
    procedure ReadCode is
      bit_mask: Natural:= 1;
    begin
      Code := 0;
      -- Read the code, bit by bit
      for Counter  in reverse  0..CodeSize - 1  loop
        -- Next bit
        bits_in:= bits_in + 1;
        -- Maybe, a new byte needs to be loaded with a further 8 bits
        if bits_in = 9 then
          bits_buf:= Read_Byte;
          bits_in := 1;
        end if;
        -- Add the current bit to the code
        if (bits_buf  and  1) > 0 then
          Code:= Code + bit_mask;
        end if;
        bit_mask := bit_mask * 2;
        bits_buf := bits_buf / 2;
      end loop;
    end ReadCode;

    generic
      -- Parameter(s) that are constant through
      -- the whole image. Macro-expanded generics and
      -- some optimization will trim corresponding "if's"
      interlaced     : Boolean;
      transparency   : Boolean;
      num_of_colours : Natural;
      pixel_mask     : U32;
      --
    procedure GIF_Decode;

    procedure GIF_Decode is

      procedure Pixel_with_palette(b: U8) is
      pragma Inline(Pixel_with_palette);
      begin
        if transparency and then b = Transp_color then
          Put_Pixel(0,0,0, 0);
          return;
        end if;
        case Primary_color_range'Modulus is
          when 256 =>
            Put_Pixel(
              Primary_color_range(local.palette(Integer(b)).red),
              Primary_color_range(local.palette(Integer(b)).green),
              Primary_color_range(local.palette(Integer(b)).blue),
              255
            );
          when 65_536 =>
            Put_Pixel(
              256 * Primary_color_range(local.palette(Integer(b)).red),
              256 * Primary_color_range(local.palette(Integer(b)).green),
              256 * Primary_color_range(local.palette(Integer(b)).blue),
              65_535
            );
          when others =>
            raise invalid_primary_color_range;
        end case;
      end Pixel_with_palette;

      -- Interlacing
      Interlace_pass : Natural range 1..4:= 1;
      Span           : Natural:= 7;

      -- Local procedure to draw a pixel
      procedure NextPixel(code: Natural) is
      pragma Inline(NextPixel);
        c : constant Color_Type:= Color_type(U32(code) and Pixel_mask);
      begin
        -- Actually draw the pixel on screen buffer
        if X < image.width then
          if interlaced and mode = nice then
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
          if interlaced then
            case Interlace_pass is
              when 1 =>
                Y:= Y + 8;
                if Y >= brY then
                  Y:= 4;
                  Interlace_pass:= 2;
                  Span:= 3;
                  Feedback((Interlace_pass*100)/4);
                end if;
              when 2 =>
                Y:= Y + 8;
                if Y >= brY then
                  Y:= 2;
                  Interlace_pass:= 3;
                  Span:= 1;
                  Feedback((Interlace_pass*100)/4);
                end if;
              when 3 =>
                Y:= Y + 4;
                if Y >= brY then
                  Y:= 1;
                  Interlace_pass:= 4;
                  Span:= 0;
                  Feedback((Interlace_pass*100)/4);
                end if;
              when 4 =>
                Y:= Y + 2;
            end case;
            if mode = fast and then Y < image.height then
              Set_X_Y(X, image.height - Y - 1);
            end if;
          else
            Y:= Y + 1;
            if Y < image.height then
              Set_X_Y(X, image.height - Y - 1);
            end if;
            if Y mod 32 = 0 then
              Feedback((Y*100)/image.height);
            end if;
          end if;
        end if;
      end NextPixel;

      -- The string table
      Prefix       : array ( 0..4096 ) of Natural;
      Suffix       : array ( 0..4096 ) of Natural;
      OutCode      : array ( 0..1024 ) of Natural;

      -- Local function to output a string. Returns the first character.
      function OutString (CurCode_1: Natural) return Natural is
      pragma Inline(OutString);
      begin
        -- If it's a single character, output that
        if CurCode_1 < num_of_colours then
          NextPixel( CurCode_1 );
          return CurCode_1;
        else
          declare
            CurCode  : Natural:= CurCode_1;
            OutCount : Natural:= 0;
          begin
            -- Store the string, which ends up in reverse order
            loop
              OutCode (OutCount) := Suffix (CurCode);
              OutCount:= OutCount + 1;
              CurCode := Prefix (CurCode);
              exit when CurCode < num_of_colours;
            end loop;
            -- Add the last character
            OutCode (OutCount) := CurCode;
            -- Output all the string, in the correct order
            loop
              NextPixel( OutCode(OutCount) );
              exit when OutCount = 0;
              OutCount:= OutCount - 1;
            end loop;
            -- Return 1st character
            return CurCode;
          end;
        end if;
      end OutString;

      -- Special codes (GIF only, not LZW)
      ClearCode: constant Natural:= 2 ** CodeSize; -- Reset code
      EOICode  : constant Natural:= ClearCode + 1; -- End of file
      -- String table
      FirstFree: constant Natural:= ClearCode + 2;    -- Strings start here
      FreeCode : Natural:= FirstFree; -- Strings can be added here

      InitCodeSize : constant Code_size_range:= CodeSize + 1;
      MaxCode      : Natural:= 2 ** InitCodeSize;
      OldCode      : Natural;

    begin -- GIF_Decode
      CodeSize:= InitCodeSize;
      LZW_decompression:
      loop
        -- Read next code
        ReadCode;
        -- If it's an End-Of-Information code, stop processing
        exit when Code = EOICode;
        -- If it's a clear code...
        if Code = ClearCode then
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
    end GIF_Decode;

    -- Here we have several specialized instances of GIF_Decode,
    -- with compile-time known parameters...
    --
    procedure GIF_Decode_interlaced_transparent_8 is
      new GIF_Decode(True, True, 256, 255);
    procedure GIF_Decode_straight_transparent_8 is
      new GIF_Decode(False, True, 256, 255);
    procedure GIF_Decode_interlaced_opaque_8 is
      new GIF_Decode(True, False, 256, 255);
    procedure GIF_Decode_straight_opaque_8 is
      new GIF_Decode(False, False, 256, 255);
    --
    procedure Skip_sub_blocks is
      temp: U8;
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

    temp, label: U8;
    delay_frame: U16;
    c: Character;
    frame_interlaced: Boolean;
    frame_transparency: Boolean:= False;
    local_palette  : Boolean;
    --
    separator :  Character ;
    -- Colour information
    new_num_of_colours : Natural;
    pixel_mask : U32;
    BitsPerPixel  : Natural;

  begin -- Load
    next_frame:= 0.0;
    -- Scan various GIF blocks, until finding an image
    loop
      Character'Read( image.stream, separator );
      if full_trace then
        Ada.Text_IO.Put(
          "GIF separator [" & separator &
          "][" & Integer'Image(Character'pos(separator)) & ']'
        );
      end if;
      case separator is
        when ',' => -- 16#2C#
          exit;
          -- Image descriptor will begin
          -- See: 20. Image Descriptor
        when ';' => -- 16#3B#
          if full_trace then
            Ada.Text_IO.Put(" - End of GIF");
          end if;
          image.next_frame:= 0.0;
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
                Raise_Exception(
                  error_in_image_data'Identity,
                  "GIF: error in Graphic Control Extension"
                );
              end if;
              U8'Read( image.stream, temp );
              --  Reserved                      3 Bits
              --  Disposal Method               3 Bits
              --  User Input Flag               1 Bit
              --  Transparent Color Flag        1 Bit
              frame_transparency:= (temp and 1) = 1;
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
          Raise_Exception(
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
    frame_interlaced:= (Descriptor.Depth and 64) = 64;
    local_palette:= (Descriptor.Depth and 128) = 128;
    local.format:= GIF;
    local.stream:= image.stream;
    if local_palette then
      -- Get amount of colours in image
      BitsPerPixel := 1 + Natural(Descriptor.Depth and 7);
      New_num_of_colours:= 2 ** BitsPerPixel;
      -- 21. Local Color Table
      local.palette:= new Color_table(0..New_num_of_colours-1);
      Color_tables.Load_palette(local);
    elsif image.palette = null then
      Raise_Exception(
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
    Pixel_mask:= U32(New_num_of_colours - 1);

    if full_trace then
      Ada.Text_IO.Put_Line(
        " - Image, interlaced: " & Boolean'Image(frame_interlaced) &
        "; local palette: " & Boolean'Image(local_palette) &
        "; transparency: " & Boolean'Image(frame_transparency) &
        "; transparency index:" & U8'Image(Transp_color)
      );
    end if;

    -- Get initial code size
    U8'Read( image.stream, temp );
    if Natural(temp) not in Code_size_range then
      Raise_Exception(
        error_in_image_data'Identity,
        "GIF: wrong LZW code size (must be in 2..12), is" &
        U8'Image(temp)
      );
    end if;
    CodeSize := Natural(temp);

    -- Start at top left of image
    X := Natural(Descriptor.ImageLeft);
    Y := Natural(Descriptor.ImageTop);
    Set_X_Y(X, image.height - Y - 1);
    --
    if new_num_of_colours < 256 then
      -- "Rare" formats -> no need of best speed
      declare
        -- We create an instance with dynamic parameters
        procedure GIF_Decode_general is
          new GIF_Decode(frame_interlaced, frame_transparency, new_num_of_colours, pixel_mask);
      begin
        GIF_Decode_general;
      end;
    else
      -- 8 bit, usual format: we try to make things
      -- faster with specialized instances...
      if frame_interlaced then
        if frame_transparency then
          GIF_Decode_interlaced_transparent_8;
        else
          GIF_Decode_interlaced_opaque_8;
        end if;
      else -- straight, non-interlaced
        if frame_transparency then
          GIF_Decode_straight_transparent_8;
        else
          GIF_Decode_straight_opaque_8;
        end if;
      end if;
    end if;
    Feedback(100);
    --
    U8'Read( image.stream, temp ); -- zero-size sub-block
  end Load;

end GID.Decoding_GIF;
