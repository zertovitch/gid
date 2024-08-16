--  This is a mix of 3 sources with MIT and BSD license:
--  https://www.nayuki.io/page/dumb-png-output-java (MIT)
--  This project (GID)'s PNG decoder for CRC32 checksum (MIT)
--  https://github.com/jrcarter/Z_Compression for Adler32 checksum (BSD)

package body Dumb_PNG is

  use Ada.Streams, Interfaces;

  procedure Deflate
    (data   : in     Byte_Array;
     output :    out Byte_Array;
     last   :    out Integer);

  procedure writeChunk
    (chunk_type, chunk_data : in     Byte_Array;
     s                      : in out Root_Stream_Type'Class);

  procedure writeInt32
    (x : in     Unsigned_32;
     s : in out Root_Stream_Type'Class);

  package CRC32 is
    procedure Init (crc : out Unsigned_32);
    function  Final (crc : Unsigned_32) return Unsigned_32;
    procedure Update (crc : in out Unsigned_32; in_buf : Byte_Array);
  end CRC32;

  --

  function To_Bytes_Big_Endian (x : Unsigned_32) return Byte_Array is
    result : Byte_Array (1 .. 4);
  begin
    result (1) := Unsigned_8 (Shift_Right (x, 24));
    result (2) := Unsigned_8 (Shift_Right (x, 16) and 255);
    result (3) := Unsigned_8 (Shift_Right (x,  8) and 255);
    result (4) := Unsigned_8              (x      and 255);
    return result;
  end To_Bytes_Big_Endian;

  procedure write
    (data   : in     Byte_Array;  --  Raw, packed, 8-bit-per-channel RGB data
     width  : in     Integer;     --  Image width
     height : in     Integer;     --  Image height
     s      : in out Ada.Streams.Root_Stream_Type'Class)
  is
    ihdr          : Byte_Array (0 .. 12);
    rowSize       : Integer;
    idat          : p_Byte_Array;
    idat_deflated : p_Byte_Array;
    index_data    : Integer;
    index         : Integer;
    deflated_size : Integer;
    deflated_last : Integer;

    function To_Bytes (s : String) return Byte_Array is
      result : Byte_Array (s'Range);
    begin
      for i in s'Range loop
        result (i) := Character'Pos (s (i));
      end loop;
      return result;
    end To_Bytes;

  begin
    --  PNG header
    Byte_Array'Write
      (s'Access,
       16#89# & To_Bytes ("PNG") & (13, 10, 16#1A#, 10));

    --  IHDR chunk
    ihdr  (0 .. 3) := To_Bytes_Big_Endian (Unsigned_32 (width));
    ihdr  (4 .. 7) := To_Bytes_Big_Endian (Unsigned_32 (height));
    ihdr  (8)      := 8;  --  Bit depth: 8 bits per sample
    ihdr  (9)      := 2;  --  Color type: True color RGB
    ihdr (10)      := 0;  --  Compression method: DEFLATE
    ihdr (11)      := 0;  --  Filter method: Adaptive
    ihdr (12)      := 0;  --  Interlace method: None
    writeChunk (To_Bytes ("IHDR"), ihdr, s);

    --  IDAT chunk (pixel values and row filters)
    --  Note: One additional byte at the beginning of each
    --        row specifies the filtering method.
    rowSize := width * 3 + 1;
    idat := new Byte_Array (0 .. rowSize * height - 1);
    for y in 0 .. height - 1 loop
      idat (y * rowSize) := 0;  --  Filter type: None
      for x in  0 .. width - 1 loop
        index := y * rowSize + 1 + x * 3;
        index_data := data'First + y * width * 3 + x * 3;
        idat (index + 0) := data (index_data + 0);  --  Red
        idat (index + 1) := data (index_data + 1);  --  Green
        idat (index + 2) := data (index_data + 2);  --  Blue
      end loop;
    end loop;

    --  The "compressed" size is larger than the uncompressed one :-)
    --
    deflated_size := 6 + idat'Length + 5 * (1 + idat'Length / 16#FFFF#);

    idat_deflated := new Byte_Array (0 .. deflated_size);

    Deflate (idat.all, idat_deflated.all, deflated_last);
    writeChunk (To_Bytes ("IDAT"), idat_deflated (0 .. deflated_last), s);

    Dispose (idat);
    Dispose (idat_deflated);

    writeChunk (To_Bytes ("IEND"), (1 .. 0 => 0), s);
  end write;

  procedure Deflate
    (data   : in     Byte_Array;
     output :    out Byte_Array;
     last   :    out Integer)
  is
    procedure Write (b : Unsigned_8) is
    begin
      last := last + 1;
      output (last) := b;
    end Write;

    offset : Integer := 0;
    start  : Integer;
    curBlockSize : Integer;
    Adler_1 : Unsigned_32 := 1;
    Adler_2 : Unsigned_32 := 0;
    Modulus : constant := 65521;
  begin
    last := output'First - 1;
    --  zlib header
    Write (16#08#);
    Write (16#1D#);

    --  Deflate data
    loop
      curBlockSize := Integer'Min (data'Length - offset, 16#FFFF#);
      --  Block type: Store; final flag if last block.
      Write (if offset + curBlockSize = data'Length then 1 else 0);
      Write     (Unsigned_8  (curBlockSize        rem 256));
      Write     (Unsigned_8 ((curBlockSize / 256) rem 256));
      Write (not Unsigned_8  (curBlockSize        rem 256));
      Write (not Unsigned_8 ((curBlockSize / 256) rem 256));
      start := data'First + offset;
      for i in start .. start + curBlockSize - 1 loop
        Write (data (i));
      end loop;
      offset := offset + curBlockSize;
      exit when offset >= data'Length;
    end loop;

    for b of data loop
       Adler_1 := (Adler_1 + Unsigned_32 (b)) rem Modulus;
       Adler_2 := (Adler_2 + Adler_1) rem Modulus;
    end loop;
    Write (Unsigned_8 (Adler_2  /  256));
    Write (Unsigned_8 (Adler_2 rem 256));
    Write (Unsigned_8 (Adler_1  /  256));
    Write (Unsigned_8 (Adler_1 rem 256));
  end Deflate;

  procedure writeChunk
    (chunk_type, chunk_data : in     Byte_Array;
     s                      : in out Root_Stream_Type'Class)
  is
    c : Unsigned_32;
  begin
    CRC32.Init (c);
    CRC32.Update (c, chunk_type);
    CRC32.Update (c, chunk_data);
    writeInt32 (chunk_data'Length, s);
    Byte_Array'Write (s'Access, chunk_type);
    Byte_Array'Write (s'Access, chunk_data);
    writeInt32 (CRC32.Final (c), s);
  end writeChunk;

  procedure writeInt32
    (x : in     Unsigned_32;
     s : in out Root_Stream_Type'Class)
  is
  begin
    Byte_Array'Write (s'Access, To_Bytes_Big_Endian (x));
  end writeInt32;

  package body CRC32 is

    CRC32_Table : array (Unsigned_32'(0) .. 255) of Unsigned_32;

    procedure Prepare_Table is
      --  CRC-32 algorithm, ISO-3309
      Seed : constant := 16#EDB88320#;
      l : Unsigned_32;
    begin
      for i in CRC32_Table'Range loop
        l := i;
        for bit in 0 .. 7 loop
          if (l and 1) = 0 then
            l := Shift_Right (l, 1);
          else
            l := Shift_Right (l, 1) xor Seed;
          end if;
        end loop;
        CRC32_Table (i) := l;
      end loop;
    end Prepare_Table;

    procedure Update (crc : in out Unsigned_32; in_buf : Byte_Array) is
      local_CRC : Unsigned_32;
    begin
      local_CRC := crc;
      for i in in_buf'Range loop
        local_CRC :=
          CRC32_Table (16#FF# and (local_CRC xor Unsigned_32 (in_buf (i))))
          xor
          Shift_Right (local_CRC, 8);
      end loop;
      crc := local_CRC;
    end Update;

    table_empty : Boolean := True;

    procedure Init (crc : out Unsigned_32) is
    begin
      if table_empty then
        Prepare_Table;
        table_empty := False;
      end if;
      crc := 16#FFFF_FFFF#;
    end Init;

    function Final (crc : Unsigned_32) return Unsigned_32 is
    begin
      return not crc;
    end Final;

  end CRC32;

end Dumb_PNG;
