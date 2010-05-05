with Ada.IO_Exceptions;

package body GID.Buffering is

  -- Workaround for the severe xxx'Read xxx'Write performance
  -- problems in the GNAT and ObjectAda compilers (as in 2009)
  -- This is possible if and only if Byte = Stream_Element and
  -- arrays types are both packed the same way.
  --
  subtype Size_test_a is Byte_Array(1..19);
  subtype Size_test_b is Ada.Streams.Stream_Element_Array(1..19);
  workaround_possible: constant Boolean:=
    Size_test_a'Size = Size_test_b'Size and then
    Size_test_a'Alignment = Size_test_b'Alignment;
  --

  procedure Fill_Buffer(b: in out Input_buffer);
  -- ^ Spec here to avoid in Get_Byte below (GNAT 2009):
  -- warning: call to subprogram with no separate spec prevents inlining

  procedure Fill_Buffer(b: in out Input_buffer)
  is
    --
    procedure BlockRead(
      buffer       :    out Byte_Array;
      actually_read:    out Natural
    )
    is
      use Ada.Streams;
      Last_Read: Stream_Element_Offset;
    begin
      if workaround_possible then
        declare
          SE_Buffer: Stream_Element_Array (1 .. buffer'Length);
          -- direct mapping: buffer = SE_Buffer
          for SE_Buffer'Address use buffer'Address;
          pragma Import (Ada, SE_Buffer);
        begin
          Read(b.stm_a.all, SE_Buffer, Last_Read);
        end;
      else
        declare
          SE_Buffer: Stream_Element_Array (1 .. buffer'Length);
          -- need to copy array
        begin
          Read(b.stm_a.all, SE_Buffer, Last_Read);
          for i in buffer'Range loop
            buffer(i):= U8(SE_Buffer(Stream_Element_Offset(i-buffer'First)+SE_buffer'First));
          end loop;
        end;
      end if;
      actually_read:= Natural(Last_Read);
    end BlockRead;
    --
  begin
    BlockRead(
      buffer        => b.data,
      actually_read => b.MaxInBufIdx
    );
    b.InputEoF:= b.MaxInBufIdx = 0;
    b.InBufIdx := 1;
  end Fill_Buffer;

  procedure Attach_Stream(
    b   :    out Input_buffer;
    stm : in     Stream_Access
  )
  is
  begin
    b.stm_a:= stm;
    Fill_Buffer(b);
  end Attach_Stream;

  procedure Get_Byte(b: in out Input_buffer; byte: out U8) is
  begin
    if b.InBufIdx > b.MaxInBufIdx then
      Fill_Buffer(b);
      if b.InputEoF then
        raise Ada.IO_Exceptions.End_Error;
      end if;
    end if;
    byte:= b.data(b.InBufIdx);
    b.InBufIdx:= b.InBufIdx + 1;
  end Get_Byte;

end GID.Buffering;
