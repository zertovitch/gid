with GID.Buffering;                     use GID.Buffering;

with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

package body GID.Decoding_PNM is

  ----------
  -- Load --
  ----------

  procedure Load (image: in out Image_descriptor) is

    procedure Row_start(y: Natural) is
    begin
      Set_X_Y(0, image.height-1-y);
    end Row_Start;

    type Pixel is record
      color: RGB_Color;
      alpha: U8;
    end record;

    pix: Pixel;

    procedure Output_Pixel is
    pragma Inline(Output_Pixel);
    begin
      case Primary_color_range'Modulus is
        when 256 =>
          Put_Pixel(
            Primary_color_range(pix.color.red),
            Primary_color_range(pix.color.green),
            Primary_color_range(pix.color.blue),
            Primary_color_range(pix.alpha)
          );
        when 65_536 =>
          Put_Pixel(
            16#101#  * Primary_color_range(pix.color.red),
            16#101#  * Primary_color_range(pix.color.green),
            16#101#  * Primary_color_range(pix.color.blue),
            16#101#  * Primary_color_range(pix.alpha)
            -- 16#101# because max intensity FF goes to FFFF
          );
        when others =>
          raise invalid_primary_color_range;
      end case;
    end Output_Pixel;

    ----------
    --  P1  --
    ----------
    
    procedure Get_Mono_Text_Picture is
    begin
      for y in 0..image.height-1 loop
        Row_start(y);
        for x in 0..image.width-1 loop
          pix.color.red   := U8((1-Get_Integer(image.stream, single_char => True)) * 255);
          pix.color.green := pix.color.red;
          pix.color.blue  := pix.color.red;
          Output_Pixel;
        end loop;
        Feedback(((y+1)*100)/image.height);
      end loop;
    end Get_Mono_Text_Picture;

    ----------
    --  P2  --
    ----------
    
    procedure Get_Grey_Text_Picture is
    begin
      for y in 0..image.height-1 loop
        Row_start(y);
        for x in 0..image.width-1 loop
          pix.color.red   := U8(Get_Integer(image.stream));
          pix.color.green := pix.color.red;
          pix.color.blue  := pix.color.red;
          Output_Pixel;
        end loop;
        Feedback(((y+1)*100)/image.height);
      end loop;
    end Get_Grey_Text_Picture;
    
    ----------
    --  P3  --
    ----------
    
    procedure Get_RGB_Text_Picture is
    begin
      for y in 0..image.height-1 loop
        Row_start(y);
        for x in 0..image.width-1 loop
          pix.color.red   := U8(Get_Integer(image.stream));
          pix.color.green := U8(Get_Integer(image.stream));
          pix.color.blue  := U8(Get_Integer(image.stream));
          Output_Pixel;
        end loop;
        Feedback(((y+1)*100)/image.height);
      end loop;
    end Get_RGB_Text_Picture;

    ----------
    --  P4  --
    ----------
    
    procedure Get_Mono_Binary_Picture is
      bbf: U8;  --  Bit buffer 
    begin
      for y in 0..image.height-1 loop
        Row_start(y);
        for x in 0..image.width-1 loop
          if x mod 8 = 0 then
            Get_Byte(image.buffer, bbf);
          end if;
          pix.color.red   := 255 * (1-Shift_Right(bbf,7));
          bbf:= bbf * 2;
          pix.color.green := pix.color.red;
          pix.color.blue  := pix.color.red;
          Output_Pixel;
        end loop;
        Feedback(((y+1)*100)/image.height);
      end loop;
    end Get_Mono_Binary_Picture;

    ----------
    --  P5  --
    ----------
    
    procedure Get_Grey_Binary_Picture is
    begin
      for y in 0..image.height-1 loop
        Row_start(y);
        for x in 0..image.width-1 loop
          Get_Byte(image.buffer, pix.color.red);
          pix.color.green := pix.color.red;
          pix.color.blue  := pix.color.red;
          Output_Pixel;
        end loop;
        Feedback(((y+1)*100)/image.height);
      end loop;
    end Get_Grey_Binary_Picture;

    ----------
    --  P6  --
    ----------
    
    procedure Get_RGB_Binary_Picture is
    begin
      for y in 0..image.height-1 loop
        Row_start(y);
        for x in 0..image.width-1 loop
          Get_Byte(image.buffer, pix.color.red);
          Get_Byte(image.buffer, pix.color.green);
          Get_Byte(image.buffer, pix.color.blue);
          Output_Pixel;
        end loop;
        Feedback(((y+1)*100)/image.height);
      end loop;
    end Get_RGB_Binary_Picture;

  begin
    pix.alpha:= 255; -- opaque is default
    if image.subformat_id >= 4 then  --  Binary
      Attach_Stream(image.buffer, image.stream);
    end if;
    --
    case image.subformat_id is
      when 1 =>
        Get_Mono_Text_Picture;
      when 2 =>
        Get_Grey_Text_Picture;
      when 3 =>
        Get_RGB_Text_Picture;
      when 4 =>
        Get_Mono_Binary_Picture;
      when 5 =>
        Get_Grey_Binary_Picture;
      when 6 =>
        Get_RGB_Binary_Picture;
      when others => null;
    end case;
  end Load;

  function Get_Token(
    stream      : Stream_Access; 
    needs_EOL   : Boolean:= False;
    single_char : Boolean:= False
  ) 
  return String
  is
    c: Character;
    res: Unbounded_String;
    procedure Skip_comment is
    begin
      if c = '#' then
        loop
          Character'Read(stream, c);
          exit when c = ASCII.LF;
        end loop;
      end if;
    end;
  begin
    loop
      Character'Read(stream, c);
      Skip_comment;
      exit when c > ' ';
    end loop;
    loop
      if c > ' ' then
        res:= res & c;
      end if;
      if single_char then
        exit when Length(res) = 1;
      end if;
      Character'Read(stream, c);
      Skip_comment;
      if needs_EOL then
        exit when c = ASCII.LF;
      else
        exit when c <= ' ';
      end if;
    end loop;
    return To_String(res);
  end Get_Token;

  function Get_Integer(
    stream      : Stream_Access; 
    needs_EOL   : Boolean:= False;
    single_char : Boolean:= False
  ) 
  return Integer
  is
  begin
    return Integer'Value(Get_Token(stream, needs_EOL, single_char));
  end;

end GID.Decoding_PNM;
