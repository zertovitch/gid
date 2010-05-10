private package GID.Buffering is

  type Input_buffer is private;

  procedure Attach_Stream(
    b   :    out Input_buffer;
    stm : in     Stream_Access
  );

  procedure Get_Byte(b: in out Input_buffer; byte: out U8);
  pragma Inline(Get_Byte);

private

  type Input_buffer is record
    data       : Byte_Array(1..1024);
    stm_a      : Stream_Access;
    InBufIdx   : Positive;   --  Points to next char in buffer to be read
    MaxInBufIdx: Natural;    --  Count of valid chars in input buffer
    InputEoF   : Boolean;    --  End of file indicator
  end record;

end GID.Buffering;
