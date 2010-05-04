---------------------------------
-- GID - Generic Image Decoder --
---------------------------------
--
-- Private child of GID, with helpers for identifying
-- image formats and reading header informations.
--
package body GID.Headers is

  procedure Load_signature (
    image   :    out Image_descriptor;
    from    : in out Ada.Streams.Root_Stream_Type'Class;
    try_tga :        Boolean:= False

  )
  is
    use Bounded_255;
    c: Character;
    signature: String(1..5); -- without the initial
  begin
    Character'Read(from'Access, c);
    case c is
      when 'B' =>
        Character'Read(from'Access, c);
        if c='M' then
          image.detailed_format:= To_Bounded_String("BMP");
          image.format:= BMP;
          return;
        end if;
      when 'S' =>
        String'Read(from'Access, signature);
        if signature = "IMPLE"  then
          image.detailed_format:= To_Bounded_String("FITS");
          image.format:= FITS;
          return;
        end if;
      when 'G' =>
        String'Read(from'Access, signature);
        if signature = "IF87a" or signature = "IF89a" then
          image.detailed_format:= To_Bounded_String('G' & signature & ", ");
          image.format:= GIF;
          return;
        end if;
      when Character'Val(16#FF#) =>
        Character'Read(from'Access, c);
        if c=Character'Val(16#D8#) then
          image.detailed_format:= To_Bounded_String("JPEG");
          image.format:= JPEG;
          return;
        end if;
      when Character'Val(16#89#) =>
        String'Read(from'Access, signature(1..3));
        if signature(1..3) = "PNG" then
          image.detailed_format:= To_Bounded_String("PNG");
          image.format:= PNG;
          return;
        end if;
      when others =>
        if try_tga then
          null;
        else
          raise unknown_image_format;
        end if;
    end case;
    raise unknown_image_format;
  end Load_signature;

  procedure Load_GIF_header (
    image   :    out Image_descriptor;
    from    : in out Ada.Streams.Root_Stream_Type'Class
  )
  is
  begin
    null; -- !!
  end Load_GIF_header;

end;