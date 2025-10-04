with Ada.IO_Exceptions;

package body GID.Buffering is

  procedure Fill_Buffer (b : in out Input_Buffer);
  --  ^ Spec here to avoid warning by 'Get_Byte' below (GNAT 2009):
  --  warning: call to subprogram with no separate spec prevents inlining

  procedure Fill_Buffer (b : in out Input_Buffer)
  is
    --
    procedure Block_Read
      (buffer        : out Byte_Array;
       actually_read : out Natural)
    is
      use Ada.Streams;
      Last_Read : Stream_Element_Offset;
    begin
      if is_mapping_possible then
        declare
          SE_Buffer_mapped : Stream_Element_Array (1 .. buffer'Length);
          --  direct mapping: buffer = SE_Buffer_mapped
          for SE_Buffer_mapped'Address use buffer'Address;
          pragma Import (Ada, SE_Buffer_mapped);
        begin
          Read (b.stream.all, SE_Buffer_mapped, Last_Read);
        end;
      else
        declare
          SE_Buffer : Stream_Element_Array (1 .. buffer'Length);
          --  need to copy array (slightly slower)
        begin
          Read (b.stream.all, SE_Buffer, Last_Read);
          for i in buffer'Range loop
            buffer (i) := U8 (SE_Buffer (Stream_Element_Offset (i - buffer'First) + SE_Buffer'First));
          end loop;
        end;
      end if;
      actually_read := Natural (Last_Read);
    end Block_Read;
    --
  begin
    Block_Read
      (buffer        => b.data,
       actually_read => b.max_input_index);
    b.input_end_of_stream := b.max_input_index = 0;
    b.input_index := 1;
  end Fill_Buffer;

  procedure Reset (b : in out Input_Buffer) is
  begin
    b.input_index     := 1;
    b.max_input_index := 0;
  end Reset;

  procedure Attach_Stream
    (b   : in out Input_Buffer;
     stm : in     Stream_Access)
  is
  begin
    Reset (b);
    --  Fill_Buffer (b) will be performed on first call of Get_Byte.
    b.stream := stm;
  end Attach_Stream;

  function Is_Stream_Attached (b : Input_Buffer) return Boolean is
  begin
    return b.stream /= null;
  end Is_Stream_Attached;

  procedure Get_Byte (b : in out Input_Buffer; byte : out U8) is
  begin
    if b.input_index > b.max_input_index then
      Fill_Buffer (b);
      if b.input_end_of_stream then
        raise Ada.IO_Exceptions.End_Error;
      end if;
    end if;
    byte := b.data (b.input_index);
    b.input_index := b.input_index + 1;
  end Get_Byte;

end GID.Buffering;
