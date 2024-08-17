package body Fast_IO is

  procedure Write (s : in out Ada.Streams.Root_Stream_Type'Class; b : in Byte_Array) is
    --  Workaround for the severe xxx'Read xxx'Write performance
    --  problems in the GNAT and ObjectAda compilers (as in 2009)
    --  This is possible if and only if Byte = Stream_Element and
    --  arrays types are both packed the same way.
    --
    subtype Size_test_a is Byte_Array (1 .. 19);
    subtype Size_test_b is Ada.Streams.Stream_Element_Array (1 .. 19);
    workaround_possible : constant Boolean :=
      Size_test_a'Size = Size_test_b'Size and then
      Size_test_a'Alignment = Size_test_b'Alignment;
    --
  begin
    if workaround_possible then
      declare
        use Ada.Streams;
        SE_Buffer   : Stream_Element_Array (0 .. Stream_Element_Offset (b'Length - 1));
        for SE_Buffer'Address use b'Address;
        pragma Import (Ada, SE_Buffer);
      begin
        Ada.Streams.Write (s, SE_Buffer (0 .. Stream_Element_Offset (b'Length - 1)));
      end;
    else
      Byte_Array'Write (s'Access, b);  --  The workaround is about this line...
    end if;
  end Write;

end Fast_IO;
