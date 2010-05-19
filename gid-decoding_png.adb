with GID.Decoding_PNG.Huffman;

with Ada.Text_IO, Ada.Exceptions, Interfaces;

package body GID.Decoding_PNG is

  generic
    type Number is mod <>;
  procedure Big_endian_number(
    n    :    out Number;
    from : in     Stream_Access
  );
    pragma Inline(Big_endian_number);

  procedure Big_endian_number(
    n    :    out Number;
    from : in     Stream_Access
  )
  is
    b: U8;
  begin
    n:= 0;
    for i in 1..Number'Size/8 loop
      U8'Read(from, b);
      n:= n * 256 + Number(b);
    end loop;
  end Big_endian_number;

  procedure Big_endian is new Big_endian_number( U32 );

  use Ada.Exceptions;

  ----------
  -- Read --
  ----------

  procedure Read (image: image_descriptor; ch: out Chunk_head) is
    str4: String(1..4);
  begin
    Big_endian(ch.length, image.stream);
    String'Read(image.stream, str4);
    begin
      ch.kind:= PNG_Chunk_tag'Value(str4);
      if some_trace then
        Ada.Text_IO.Put_Line('[' & str4 & ']');
      end if;
    exception
      when Constraint_Error =>
        Raise_exception(
          error_in_image_data'Identity,
          "PNG chunk: " &
          Integer'Image(Character'Pos(str4(1))) &
          Integer'Image(Character'Pos(str4(2))) &
          Integer'Image(Character'Pos(str4(3))) &
          Integer'Image(Character'Pos(str4(4)))
        );
    end;
  end Read;

  package CRC32 is

    use Interfaces;

    procedure Init( CRC: out Unsigned_32 );

    function  Final( CRC: Unsigned_32 ) return Unsigned_32;

    procedure Update( CRC: in out Unsigned_32; InBuf: Byte_array );
    pragma Inline( Update );

  end CRC32;

  package body CRC32 is

    CRC32_Table : array( Unsigned_32'(0)..255 ) of Unsigned_32;

    procedure Prepare_table is
      -- CRC-32 algorithm, ISO-3309
      Seed: constant:= 16#EDB88320#;
      l: Unsigned_32;
    begin
      for i in CRC32_Table'Range loop
        l:= i;
        for bit in 0..7 loop
          if (l and 1) = 0 then
            l:= Shift_Right(l,1);
          else
            l:= Shift_Right(l,1) xor Seed;
          end if;
        end loop;
        CRC32_Table(i):= l;
      end loop;
    end Prepare_table;

    procedure Update( CRC: in out Unsigned_32; InBuf: Byte_array ) is
      local_CRC: Unsigned_32;
    begin
      local_CRC:= CRC ;
      for i in InBuf'Range loop
        local_CRC :=
          CRC32_Table( 16#FF# and ( local_CRC xor Unsigned_32( InBuf(i) ) ) )
          xor
          Shift_Right( local_CRC , 8 );
      end loop;
      CRC:= local_CRC;
    end Update;

    table_empty: Boolean:= True;

    procedure Init( CRC: out Unsigned_32 ) is
    begin
      if table_empty then
        Prepare_table;
        table_empty:= False;
      end if;
      CRC:= 16#FFFF_FFFF#;
    end Init;

    function Final( CRC: Unsigned_32 ) return Unsigned_32 is
    begin
      return not CRC;
    end Final;

  end CRC32;

  ----------
  -- Load --
  ----------

  procedure Load (image: in Image_descriptor) is

    subtype Mem_row_bytes_array is Byte_array(0..image.width*4);

    mem_row_bytes: array(0..1) of Mem_row_bytes_array;
    -- We need to memorize two image rows, for un-filtering
    curr_row: Natural:= 1;
    -- either current is 1 and old is 0, or the reverse

    subtype Width_range is Integer range -1..image.width-1;
    subtype Height_range is Integer range 0..image.height-1;

    x: Width_range:= -1;
    y: Height_range:= 0;

    bytes_pp: Integer;

    ------------------
    -- 9: Filtering --
    ------------------

    type Filter_method_0 is (None, Sub, Up, Average, Paeth);

    current_filter: Filter_method_0;

    procedure Unfilter_bytes(
      f: in  Byte_array;  -- filtered
      u: out Byte_array   -- unfiltered
    )
    is
    pragma Inline(Unfilter_bytes);
      -- c b
      -- a f
      a,b,c,p,pa,pb,pc,pr: Integer;
      j: Integer:= 0;
    begin
      case current_filter is
        when None    =>
          -- Recon(x) = Filt(x)
          u:= f;
        when Sub     =>
          -- Recon(x) = Filt(x) + Recon(a)
          if x > 0 then
            for i in f'Range loop
              u(u'First+j):= f(i) + mem_row_bytes(curr_row)((x-1)*bytes_pp+j);
              j:= j + 1;
            end loop;
          else
            u:= f;
          end if;
        when Up      =>
          -- Recon(x) = Filt(x) + Recon(b)
          if y > 0 then
            for i in f'Range loop
              u(u'First+j):= f(i) + mem_row_bytes(1-curr_row)(x*bytes_pp+j);
              j:= j + 1;
            end loop;
          else
            u:= f;
          end if;
        when Average =>
          -- Recon(x) = Filt(x) + floor((Recon(a) + Recon(b)) / 2)
          for i in f'Range loop
            if x > 0 then
              a:= Integer(mem_row_bytes(curr_row)((x-1)*bytes_pp+j));
            else
              a:= 0;
            end if;
            if y > 0 then
              b:= Integer(mem_row_bytes(1-curr_row)(x*bytes_pp+j));
            else
              b:= 0;
            end if;
            u(u'First+j):= f(i) + U8((a+b)/2);
            j:= j + 1;
          end loop;
        when Paeth   =>
          -- Recon(x) = Filt(x) + PaethPredictor(Recon(a), Recon(b), Recon(c))
          for i in f'Range loop
            if x > 0 then
              a:= Integer(mem_row_bytes(curr_row)((x-1)*bytes_pp+j));
            else
              a:= 0;
            end if;
            if y > 0 then
              b:= Integer(mem_row_bytes(1-curr_row)(x*bytes_pp+j));
            else
              b:= 0;
            end if;
            if x > 0 and y > 0 then
              c:= Integer(mem_row_bytes(1-curr_row)((x-1)*bytes_pp+j));
            else
              c:= 0;
            end if;
            p := a + b - c;
            pa:= abs(p - a);
            pb:= abs(p - b);
            pc:= abs(p - c);
            if pa <= pb and pa <= pc then
              pr:= a;
            elsif pb <= pc then
              pr:= b;
            else
              pr:= c;
            end if;
            u(u'First+j):= f(i) + U8(pr);
            j:= j + 1;
          end loop;
      end case;
      j:= 0;
      for i in u'Range loop
        mem_row_bytes(curr_row)(x*bytes_pp+j):= u(i);
        j:= j + 1;
      end loop;
    end Unfilter_bytes;

    filter_stat: array(Filter_method_0) of Natural:= (others => 0);

    -- Output bytes from decompression
    --
    procedure Output_uncompressed(
      data  : in     Byte_array;
      reject:    out Natural
      -- amount of bytes to be resent next time,
      -- in order to have a full multi-byte pixel
    )
    is
      i, color_idx: Integer;

      procedure Out_Pixel(br, bg, bb, ba: U8) is
      pragma Inline(Out_Pixel);
      begin
        case Primary_color_range'Modulus is
          when 256 =>
            Put_Pixel(
              Primary_color_range(br),
              Primary_color_range(bg),
              Primary_color_range(bb),
              Primary_color_range(ba)
            );
          when 65_536 =>
            Put_Pixel(
              256 * Primary_color_range(br),
              256 * Primary_color_range(bg),
              256 * Primary_color_range(bb),
              256 * Primary_color_range(ba)
            );
          when others =>
            raise invalid_primary_color_range;
        end case;
      end Out_Pixel;

      procedure Out_Pixel_Palette(ix: U8) is
      pragma Inline(Out_Pixel_Palette);
        color_idx: constant Natural:= Integer(ix);
      begin
        Out_Pixel(
          image.palette(color_idx).red,
          image.palette(color_idx).green,
          image.palette(color_idx).blue,
          255
        );
      end Out_Pixel_Palette;

      procedure Inc_XY is
      pragma Inline(Inc_XY);
      begin
        if x = Width_range'Last then
          x:= Width_range'First;
          if y < Height_range'Last then
            y:= y + 1;
            curr_row:= 1-curr_row;
            Feedback((y*100)/image.height);
          end if;
        else
          x:= x + 1;
        end if;
      end Inc_XY;

      uf: Byte_array(0..15); -- unfiltered bytes for a pixel

    begin
      if some_trace then
        Ada.Text_IO.Put("[UO]");
      end if;
      -- Depending on the row size, bpp, etc., we can have
      -- several rows, or less than one, being displayed
      -- with the uncompressed data.
      --
      -- !! all branches (image.subformat_id) inside loop as generic
      --
      i:= data'First;
      if i > data'Last then
        reject:= 0;
        return; -- data is empty, do nothing
      end if;
      --
      -- Main loop over data
      --
      loop
        if x = Width_range'First then
          exit when i > data'Last;
          begin
            current_filter:= Filter_method_0'Val(data(i));
            if some_trace then
              filter_stat(current_filter):= filter_stat(current_filter) + 1;
            end if;
          exception
            when Constraint_Error =>
              Raise_exception(
                error_in_image_data'Identity,
                "PNG: wrong filter code, row #" &
                Integer'Image(y) & " code:" & U8'Image(data(i))
              );
          end;
          Set_X_Y(0, image.height - y - 1);
          i:= i + 1;
        else
          exit when i > data'Last - (bytes_pp - 1);
          case image.subformat_id is
            when 0 => -- Greyscale
              null; -- !!
            when 2 => -- RGB
              Unfilter_bytes(data(i..i+2), uf(0..2));
              Out_Pixel(uf(0), uf(1), uf(2), 255);
              i:= i + 3;
            when 3 => -- RGB with palette
              Unfilter_bytes(data(i..i), uf(0..0));
              color_idx:= Integer(uf(0));
              Out_Pixel(
                image.palette(color_idx).red,
                image.palette(color_idx).green,
                image.palette(color_idx).blue,
                255
              );
              i:= i + 1;
            when 4 => -- Greyscale & Alpha
              null; -- !!
            when 6 => -- RGBA
              null; -- !!
            when others =>
              null; -- !!
          end case;
        end if;
        Inc_XY;
      end loop;
      -- i is between data'Last-(bytes_pp-2) and data'Last+1
      reject:= (data'Last + 1) - i;
      if reject > 0 then
        if some_trace then
          Ada.Text_IO.Put(
            "[Bytes across pixel:" &
            Integer'Image(reject) &
            ']'
          );
        end if;
      end if;
    end Output_uncompressed;

    ---------------------------------------------------------------------
    -- 10: Compression                                                 --
    -- Excerpt and simplification from UnZip.Decompress (Inflate only) --
    ---------------------------------------------------------------------

    --  Size of sliding dictionary and output buffer
    wsize: constant:= 16#10000#;

    --------------------------------------
    -- Specifications of UnZ_* packages --
    --------------------------------------

    use Interfaces;

    package UnZ_Glob is
      -- I/O Buffers
      -- > Sliding dictionary for unzipping, and output buffer as well
      slide: Byte_Array( 0..wsize );
      slide_index: Integer:= 0; -- Current Position in slide
      Zip_EOF  : constant Boolean:= False;
      crc32val : Unsigned_32;  -- crc calculated from data
    end UnZ_Glob;

    package UnZ_IO is

      procedure Init_Buffers;

      procedure Read_raw_byte ( bt : out U8 );
        pragma Inline(Read_raw_byte);

      package Bit_buffer is
        procedure Init;
        -- Read at least n bits into the bit buffer, returns the n first bits
        function Read ( n: Natural ) return Integer;
          pragma Inline(Read);
        function Read_U32 ( n: Natural ) return Unsigned_32;
          pragma Inline(Read_U32);
        -- Dump n bits no longer needed from the bit buffer
        procedure Dump ( n: Natural );
          pragma Inline(Dump);
        procedure Dump_to_byte_boundary;
        function Read_and_dump( n: Natural ) return Integer;
          pragma Inline(Read_and_dump);
        function Read_and_dump_U32( n: Natural ) return Unsigned_32;
          pragma Inline(Read_and_dump_U32);
      end Bit_buffer;

      procedure Flush ( x: Natural ); -- directly from slide to output stream

      procedure Flush_if_full(W: in out Integer);
        pragma Inline(Flush_if_full);

      procedure Copy(
        distance, length:        Natural;
        index           : in out Natural );
      pragma Inline(Copy);

    end UnZ_IO;

    package UnZ_Meth is
      deflate_e_mode: constant Boolean:= False;
      procedure Inflate;
    end UnZ_Meth;

    ------------------------------
    -- Bodies of UnZ_* packages --
    ------------------------------
    package body UnZ_IO is

      procedure Init_Buffers is
      begin
        UnZ_Glob.slide_index := 0;
        Bit_buffer.Init;
        CRC32.Init( UnZ_Glob.crc32val );
      end Init_Buffers;

      procedure Read_raw_byte ( bt : out U8 ) is
      begin
        U8'Read(image.stream, bt);
      end Read_raw_byte;

      package body Bit_buffer is
        B : Unsigned_32;
        K : Integer;

        procedure Init is
        begin
          B := 0;
          K := 0;
        end Init;

        procedure Need( n : Natural ) is
          pragma Inline(Need);
          bt: U8;
        begin
          while K < n loop
            Read_raw_byte( bt );
            B:= B or Shift_Left( Unsigned_32( bt ), K );
            K:= K + 8;
          end loop;
        end Need;

        procedure Dump ( n : Natural ) is
        begin
          B := Shift_Right(B, n );
          K := K - n;
        end Dump;

        procedure Dump_to_byte_boundary is
        begin
          Dump ( K mod 8 );
        end Dump_to_byte_boundary;

        function Read_U32 ( n: Natural ) return Unsigned_32 is
        begin
          Need(n);
          return B and (Shift_Left(1,n) - 1);
        end Read_U32;

        function Read ( n: Natural ) return Integer is
        begin
          return Integer(Read_U32(n));
        end Read;

        function Read_and_dump( n: Natural ) return Integer is
          res: Integer;
        begin
          res:= Read(n);
          Dump(n);
          return res;
        end Read_and_dump;

        function Read_and_dump_U32( n: Natural ) return Unsigned_32 is
          res: Unsigned_32;
        begin
          res:= Read_U32(n);
          Dump(n);
          return res;
        end Read_and_dump_U32;

      end Bit_buffer;

      old_bytes: Natural:= 0;
      -- how many bytes to be resent from last Inflate output
      byte_mem: Byte_array(1..8);

      procedure Flush ( x: Natural ) is
        use Ada.Streams;
      begin
        if full_trace then
          Ada.Text_IO.Put("[Flush...");
        end if;
        CRC32.Update( UnZ_Glob.crc32val, UnZ_Glob.slide( 0..x-1 ) );
        if old_bytes > 0 then
          declare
            app: constant Byte_array:=
              byte_mem(1..old_bytes) & UnZ_Glob.slide(0..x-1);
          begin
            Output_uncompressed(app, old_bytes);
            -- In extreme cases (x very small), we might have some of
            -- the rejected bytes from byte_mem.
            if old_bytes > 0 then
              byte_mem(1..old_bytes):= app(app'Last-(old_bytes-1)..app'Last);
            end if;
          end;
        else
          Output_uncompressed(UnZ_Glob.slide(0..x-1), old_bytes);
          if old_bytes > 0 then
            byte_mem(1..old_bytes):= UnZ_Glob.slide(x-old_bytes..x-1);
          end if;
        end if;
        if full_trace then
          Ada.Text_IO.Put_Line("finished]");
        end if;
      end Flush;

      procedure Flush_if_full(W: in out Integer) is
      begin
        if W = wsize then
          Flush(wsize);
          W:= 0;
        end if;
      end Flush_if_full;

      ----------------------------------------------------
      -- Reproduction of sequences in the output slide. --
      ----------------------------------------------------

      -- Internal:

      procedure Adjust_to_Slide(
          source         : in out Integer;
          remain         : in out Natural;
          part           :    out Integer;
          index:                  Integer)
      is
        pragma Inline(Adjust_to_Slide);
      begin
        source:= source mod wsize;
        -- source and index are now in 0..WSize-1
        if  source > index then
          part:= wsize-source;
        else
          part:= wsize-index;
        end if;
        -- NB: part is in 1..WSize (part cannot be 0)
        if part > remain then
          part:= remain;
        end if;
        -- Now part <= remain
        remain:= remain - part;
        -- NB: remain cannot be < 0
      end Adjust_to_Slide;

      procedure Copy_range(source, index: in out Natural; amount: Positive) is
        pragma Inline(Copy_range);
      begin
        if abs (index - source) < amount then
          -- if source >= index, the effect of copy is
          -- just like the non-overlapping case
          for count in reverse 1..amount loop
            UnZ_Glob.slide(index):= UnZ_Glob.slide(source);
            index := index  + 1;
            source:= source + 1;
          end loop;
        else -- non-overlapping -> copy slice
          UnZ_Glob.slide( index .. index+amount-1 ):=
            UnZ_Glob.slide( source..source+amount-1 );
          index := index  + amount;
          source:= source + amount;
        end if;
      end Copy_range;

      -- The copying routines:

      procedure Copy(
          distance, length:        Natural;
          index           : in out Natural )
      is
        source,part,remain: Integer;
      begin
        source:= index - distance;
        remain:= length;
        loop
          Adjust_to_Slide(source,remain,part, index);
          Copy_range(source, index, part);
          Flush_if_full(index);
          exit when remain = 0;
        end loop;
      end Copy;

    end UnZ_IO;

    package body UnZ_Meth is

      use GID.Decoding_PNG.Huffman;

      --------[ Method: Inflate ]--------

      procedure Inflate_Codes ( Tl, Td: p_Table_list; Bl, Bd: Integer ) is
        CTE    : p_HufT;       -- current table element
        length : Natural;
        E      : Integer;      -- table entry flag/number of extra bits
        W      : Integer:= UnZ_Glob.slide_index;
        -- more local variable for slide index
      begin
        if full_trace then
          Ada.Text_IO.Put_Line("Begin Inflate_codes");
        end if;

        -- inflate the coded data
        main_loop:
        while not UnZ_Glob.Zip_EOF loop
          CTE:= Tl.table( UnZ_IO.Bit_buffer.Read(Bl) )'Access;

          loop
            E := CTE.extra_bits;
            exit when E <= 16;
            if E = invalid then
              raise error_in_image_data;
            end if;

            -- then it's a literal
            UnZ_IO.Bit_buffer.Dump( CTE.bits );
            E:= E - 16;
            CTE := CTE.next_table( UnZ_IO.Bit_buffer.Read(E) )'Access;
          end loop;

          UnZ_IO.Bit_buffer.Dump ( CTE.bits );

          case E is
            when 16 =>     -- CTE.N is a Litteral
              UnZ_Glob.slide ( W ) :=  U8( CTE.n );
              W:= W + 1;
              UnZ_IO.Flush_if_full(W);

            when 15 =>     -- End of block (EOB)
              if full_trace then
                Ada.Text_IO.Put_Line("Exit  Inflate_codes, e=15 EOB");
              end if;
              exit main_loop;

            when others => -- We have a length/distance

              -- Get length of block to copy:
              length:= CTE.n + UnZ_IO.Bit_buffer.Read_and_dump(E);

              -- Decode distance of block to copy:
              CTE := Td.table( UnZ_IO.Bit_buffer.Read(Bd) )'Access;
              loop
                E := CTE.extra_bits;
                exit when E <= 16;
                if E = invalid then
                  raise error_in_image_data;
                end if;
                UnZ_IO.Bit_buffer.Dump( CTE.bits );
                E:= E - 16;
                CTE := CTE.next_table( UnZ_IO.Bit_buffer.Read(E) )'Access;
              end loop;
              UnZ_IO.Bit_buffer.Dump( CTE.bits );

              UnZ_IO.Copy(
                distance => CTE.n + UnZ_IO.Bit_buffer.Read_and_dump(E),
                length   => length,
                index    => W
              );
          end case;
        end loop main_loop;

        UnZ_Glob.slide_index:= W;

        if full_trace then
          Ada.Text_IO.Put_Line("End   Inflate_codes");
        end if;
      end Inflate_Codes;

      procedure Inflate_stored_block is -- Actually, nothing to inflate
        N : Integer;
      begin
        if full_trace then
          Ada.Text_IO.Put_Line("Begin Inflate_stored_block");
        end if;
        UnZ_IO.Bit_buffer.Dump_to_byte_boundary;

        -- Get the block length and its complement
        N:= UnZ_IO.Bit_buffer.Read_and_dump( 16 );
        if  N /= Integer(
         (not UnZ_IO.Bit_buffer.Read_and_dump_U32(16))
         and 16#ffff#)
        then
          raise error_in_image_data;
        end if;
        while N > 0  and then not UnZ_Glob.Zip_EOF loop
          -- Read and output the non-compressed data
          N:= N - 1;
          UnZ_Glob.slide ( UnZ_Glob.slide_index ) :=
            U8( UnZ_IO.Bit_buffer.Read_and_dump(8) );
          UnZ_Glob.slide_index:= UnZ_Glob.slide_index + 1;
          UnZ_IO.Flush_if_full(UnZ_Glob.slide_index);
        end loop;
        if full_trace then
          Ada.Text_IO.Put_Line("End   Inflate_stored_block");
        end if;
      end Inflate_stored_block;

      -- Copy lengths for literal codes 257..285

      copy_lengths_literal : Length_array( 0..30 ) :=
           (  3,  4,  5,  6,  7,  8,  9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
             35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258, 0, 0 );

      -- Extra bits for literal codes 257..285

      extra_bits_literal : Length_array( 0..30 ) :=
             ( 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
               3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0, invalid, invalid );

      -- Copy offsets for distance codes 0..29 (30..31: deflate_e)

      copy_offset_distance : constant Length_array( 0..31 ) :=
           ( 1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
             257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145,
             8193, 12289, 16385, 24577, 32769, 49153 );

      -- Extra bits for distance codes

      extra_bits_distance : constant Length_array( 0..31 ) :=
           ( 0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
             7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14 );

      max_dist: Integer:= 29; -- changed to 31 for deflate_e

      procedure Inflate_fixed_block is
        Tl,                        -- literal/length code table
          Td : p_Table_list;            -- distance code table
        Bl, Bd : Integer;          -- lookup bits for tl/bd
        huft_incomplete : Boolean;

        -- length list for HufT_build (literal table)
        L: constant Length_array( 0..287 ):=
          ( 0..143=> 8, 144..255=> 9, 256..279=> 7, 280..287=> 8);

      begin
        if full_trace then
          Ada.Text_IO.Put_Line("Begin Inflate_fixed_block");
        end if;

        -- make a complete, but wrong code set
        Bl := 7;
        HufT_build(
          L, 257, copy_lengths_literal, extra_bits_literal,
          Tl, Bl, huft_incomplete
        );

        -- Make an incomplete code set
        Bd := 5;
        begin
          HufT_build(
            (0..max_dist => 5), 0,
            copy_offset_distance, extra_bits_distance,
            Td, Bd, huft_incomplete
          );
          if huft_incomplete then
            if full_trace then
              Ada.Text_IO.Put_Line(
                "td is incomplete, pointer=null: " &
                Boolean'Image(Td=null)
              );
            end if;
          end if;
        exception
          when huft_out_of_memory | huft_error =>
            HufT_free( Tl );
            raise error_in_image_data;
        end;

        Inflate_Codes ( Tl, Td, Bl, Bd );

        HufT_free ( Tl );
        HufT_free ( Td );

        if full_trace then
          Ada.Text_IO.Put_Line("End   Inflate_fixed_block");
        end if;
      end Inflate_fixed_block;

      procedure Inflate_dynamic_block is
        bit_order : constant array ( 0..18 ) of Natural :=
         ( 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 );

        Lbits : constant:= 9;
        Dbits : constant:= 6;

        current_length: Natural;
        defined, number_of_lengths: Natural;

        Tl,                             -- literal/length code tables
          Td : p_Table_list;            -- distance code tables

        CTE : p_HufT;  -- current table element

        Bl, Bd : Integer;                  -- lookup bits for tl/bd
        Nb : Natural;  -- number of bit length codes
        Nl : Natural;  -- number of literal length codes
        Nd : Natural;  -- number of distance codes

        -- literal/length and distance code lengths
        Ll: Length_array( 0 .. 288+32-1 ):= (others=> 0);

        huft_incomplete : Boolean;

        procedure Repeat_length_code( amount: Natural ) is
        begin
          if defined + amount > number_of_lengths then
            raise error_in_image_data;
          end if;
          for c in reverse 1..amount loop
            Ll ( defined ) := current_length;
            defined:= defined + 1;
          end loop;
        end Repeat_length_code;

      begin
        if full_trace then
          Ada.Text_IO.Put_Line("Begin Inflate_dynamic_block");
        end if;

        -- Read in table lengths
        Nl := 257 + UnZ_IO.Bit_buffer.Read_and_dump(5);
        Nd :=   1 + UnZ_IO.Bit_buffer.Read_and_dump(5);
        Nb :=   4 + UnZ_IO.Bit_buffer.Read_and_dump(4);

        if Nl > 288 or else Nd > 32 then
          raise error_in_image_data;
        end if;

        -- Read in bit-length-code lengths.
        -- The rest, Ll( Bit_Order( Nb .. 18 ) ), is already = 0
        for J in  0 .. Nb - 1  loop
          Ll ( bit_order( J ) ) := UnZ_IO.Bit_buffer.Read_and_dump(3);
        end loop;

        -- Build decoding table for trees--single level, 7 bit lookup
        Bl := 7;
        begin
          HufT_build (
            Ll( 0..18 ), 19, empty, empty, Tl, Bl, huft_incomplete
          );
          if huft_incomplete then
            HufT_free(Tl);
            raise error_in_image_data;
          end if;
        exception
          when others =>
            raise error_in_image_data;
        end;

        -- Read in literal and distance code lengths
        number_of_lengths := Nl + Nd;
        defined := 0;
        current_length := 0;

        while  defined < number_of_lengths  loop
          CTE:= Tl.table( UnZ_IO.Bit_buffer.Read(Bl) )'Access;
          UnZ_IO.Bit_buffer.Dump( CTE.bits );

          case CTE.n is
            when 0..15 =>       -- length of code in bits (0..15)
              current_length:= CTE.n;
              Ll (defined) := current_length;
              defined:= defined + 1;

            when 16 =>          -- repeat last length 3 to 6 times
              Repeat_length_code(3 + UnZ_IO.Bit_buffer.Read_and_dump(2));

            when 17 =>          -- 3 to 10 zero length codes
              current_length:= 0;
              Repeat_length_code(3 + UnZ_IO.Bit_buffer.Read_and_dump(3));

            when 18 =>          -- 11 to 138 zero length codes
              current_length:= 0;
              Repeat_length_code(11 + UnZ_IO.Bit_buffer.Read_and_dump(7));

            when others =>
              if full_trace then
                Ada.Text_IO.Put_Line(
                  "Illegal length code: " &
                  Integer'Image(CTE.n)
                );
              end if;

          end case;
        end loop;

        HufT_free ( Tl );        -- free decoding table for trees

        -- Build the decoding tables for literal/length codes
        Bl := Lbits;
        begin
          HufT_build (
            Ll( 0..Nl-1 ), 257,
            copy_lengths_literal, extra_bits_literal,
            Tl, Bl, huft_incomplete
          );
          if huft_incomplete then
            HufT_free(Tl);
            raise error_in_image_data;
          end if;
        exception
          when others =>
            raise error_in_image_data;
        end;

        -- Build the decoding tables for distance codes
        Bd := Dbits;
        begin
          HufT_build (
            Ll( Nl..Nl+Nd-1 ), 0,
            copy_offset_distance, extra_bits_distance,
            Td, Bd, huft_incomplete
          );
          if huft_incomplete then -- do nothing!
            if full_trace then
              Ada.Text_IO.Put_Line("PKZIP 1.93a bug workaround");
            end if;
          end if;
        exception
          when huft_out_of_memory | huft_error =>
            HufT_free(Tl);
            raise error_in_image_data;
        end;

        -- Decompress until an end-of-block code

        Inflate_Codes ( Tl, Td, Bl, Bd );
        HufT_free ( Tl );
        HufT_free ( Td );

        if full_trace then
          Ada.Text_IO.Put_Line("End   Inflate_dynamic_block");
        end if;
      end Inflate_dynamic_block;

      procedure Inflate_Block( last_block: out Boolean ) is
      begin
        last_block:= Boolean'Val(UnZ_IO.Bit_buffer.Read_and_dump(1));
        case UnZ_IO.Bit_buffer.Read_and_dump(2) is -- Block type = 0,1,2,3
          when 0 =>      Inflate_stored_block;
          when 1 =>      Inflate_fixed_block;
          when 2 =>      Inflate_dynamic_block;
          when others => raise error_in_image_data; -- Bad block type (3)
        end case;
      end Inflate_Block;

      procedure Inflate is
        is_last_block: Boolean;
        blocks: Positive:= 1;
      begin
        if deflate_e_mode then
          copy_lengths_literal(28):= 3; -- instead of 258
          extra_bits_literal(28):= 16;  -- instead of 0
          max_dist:= 31;
        end if;
        loop
          Inflate_Block ( is_last_block );
          exit when is_last_block;
          blocks:= blocks+1;
        end loop;
        UnZ_IO.Flush( UnZ_Glob.slide_index );
        UnZ_Glob.slide_index:= 0;
        if full_trace then
          Ada.Text_IO.Put("# blocks:" & Integer'Image(blocks));
        end if;
        UnZ_Glob.crc32val := CRC32.Final( UnZ_Glob.crc32val );
      end Inflate;

    end UnZ_Meth;

    ch: Chunk_head;
    b: U8;
    z_crc, dummy: U32;

  begin
    case image.subformat_id is
      when 0 => -- Greyscale
        bytes_pp:= 1;
      when 2 => -- RGB
        bytes_pp:= 3;
      when 3 => -- RGB with palette
        bytes_pp:= 1;
      when 4 => -- Greyscale & Alpha
        bytes_pp:= 2;
      when 6 => -- RGBA
        bytes_pp:= 4;
      when others =>
        null;
    end case;
    loop
      Read(image, ch);
      case ch.kind is
        when IEND =>
          exit;
        when IDAT =>
          U8'Read(image.stream, b); -- zlib compression method/flags code
          U8'Read(image.stream, b); -- Additional flags/check bits
          UnZ_IO.Init_Buffers;
          UnZ_Meth.Inflate;
          Big_endian(z_crc, image.stream); -- zlib Check value
          --  if z_crc /= U32(UnZ_Glob.crc32val) then
          --    ada.text_io.put(z_crc 'img &  UnZ_Glob.crc32val'img);
          --    Raise_exception(
          --      error_in_image_data'Identity,
          --      "PNG: deflate stream corrupt"
          --    );
          --  end if;
          --  ** Mystery: this check fail even with images which decompress perfectly
          --  ** Is CRC init value different between zip and zlib ?
          Big_endian(dummy, image.stream); -- chunk's CRC (then, on compressed data)
          --
        when others =>
          -- Skip chunk data and CRC
          for i in 1..ch.length + 4 loop
            U8'Read(image.stream, b);
          end loop;
      end case;
    end loop;
    if some_trace then
      for f in Filter_method_0 loop
        Ada.Text_IO.Put_Line(
          "Filter: " &
          Filter_method_0'Image(f) &
          Integer'Image(filter_stat(f))
        );
      end loop;
    end if;
  end Load;

end GID.Decoding_PNG;
