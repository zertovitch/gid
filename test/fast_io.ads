with Ada.Streams;

generic

  type Byte is private;
  type Byte_Array is array (Integer range <>) of Byte;

package Fast_IO is

  --  `Read` needs some extra info not available with Ada.Streams.Root_Stream_Type.
  --  See zip.adb of the Zip-Ada project for an implementation.

  procedure Write (s : in out Ada.Streams.Root_Stream_Type'Class; b : in Byte_Array);

end Fast_IO;
