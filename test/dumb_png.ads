--  Minimal PNG encoder, without compression nor transparency channel.
--  For a full-featured encoder, look at the Image_IO crate.

with Ada.Streams;
with Ada.Unchecked_Deallocation;

with Interfaces;

package Dumb_PNG is

  type Byte_Array is array (Integer range <>) of Interfaces.Unsigned_8;
  type p_Byte_Array is access Byte_Array;
  procedure Dispose is new Ada.Unchecked_Deallocation (Byte_Array, p_Byte_Array);

  procedure Write
    (data   : in     Byte_Array;  --  Raw, packed, 8-bit-per-channel RGB data
     width  : in     Integer;     --  Image width
     height : in     Integer;     --  Image height
     s      : in out Ada.Streams.Root_Stream_Type'Class);

end Dumb_PNG;

--  For a quick test:
--
--  with Ada.Streams.Stream_IO;
--  with Dumb_PNG;
--
--  procedure test_dumb_png is
--    use Ada.Streams.Stream_IO;
--    f : File_Type;
--  begin
--    Create (f, Out_File, "test_dumb.png");
--    Dumb_PNG.write
--      ((255, 0, 0,        --  Red   pixel
--        0, 255, 0,        --  Green pixel
--        0, 0, 255,        --  Blue  pixel
--        0, 0, 0,          --  Black pixel
--        128, 128, 128,    --  Gray  pixel
--        255, 255, 255),   --  White pixel
--        3,  --  width
--        2,  --  height
--        Stream (f).all);
--    Close (f);
--  end test_dumb_png;
