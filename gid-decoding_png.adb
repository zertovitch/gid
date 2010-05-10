package body GID.Decoding_PNG is

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

  procedure Read_Intel is new Read_Intel_x86_number( U32 );

  ----------
  -- Read --
  ----------

  procedure Read (image: image_descriptor; ch: out Chunk_head) is
    str4: String(1..4);
  begin
    Read_Intel(ch.length, image.stream);
    String'Read(image.stream, str4);
    begin
      ch.kind:= PNG_Chunk_tag'Value(str4);
    exception
      when Constraint_Error =>
        raise unknown_chunk_type;
    end;
  end Read;

  ----------
  -- Load --
  ----------

  procedure Load (image: in Image_descriptor) is
  begin
    --  Generated stub: replace with real body!
    raise Program_Error;
  end Load;

end GID.Decoding_PNG;
