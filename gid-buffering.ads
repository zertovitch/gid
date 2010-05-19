private package GID.Buffering is

  type Input_buffer is private;

  procedure Attach_Stream(
    b   :    out Input_buffer;
    stm : in     Stream_Access
  );

  procedure Get_Byte(b: in out Input_buffer; byte: out U8);
  pragma Inline(Get_Byte);

  -- is_mapping_possible: Compile-time test to check if
  -- a Byte_Array is equivalemnt to a Ada.Streams.Stream_Element_Array.
  --
  -- Used internally by GID.Buffering; but can be used for similar buffers
  -- like GIF's variable-size buffers.
  is_mapping_possible: constant Boolean;

private

  type Input_buffer is record
    data       : Byte_Array(1..1024);
    stm_a      : Stream_Access;
    InBufIdx   : Positive;   --  Points to next char in buffer to be read
    MaxInBufIdx: Natural;    --  Count of valid chars in input buffer
    InputEoF   : Boolean;    --  End of file indicator
  end record;

  subtype Size_test_a is Byte_Array(1..19);
  subtype Size_test_b is Ada.Streams.Stream_Element_Array(1..19);
  --
  is_mapping_possible: constant Boolean:=
    Size_test_a'Size = Size_test_b'Size and
    Size_test_a'Alignment = Size_test_b'Alignment;

end GID.Buffering;
