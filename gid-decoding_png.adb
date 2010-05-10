with GID.Headers;

package body GID.Decoding_PNG is

  procedure Read_Intel is new Headers.Read_Intel_x86_number( U16 );
  procedure Read_Intel is new Headers.Read_Intel_x86_number( U32 );

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
--    exception
--      when !!
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
