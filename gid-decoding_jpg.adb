-- Steps for decoding a JPEG image
--
-- 1. RLE and Huffman decompression
-- 2. Inverse quantization (DQT)
-- 3. Inverse DCT
-- 4. Oversampling
-- 5. Color transformation
-- 6. Image reconstruction
--
-- http://en.wikipedia.org/wiki/JPEG

with GID.Buffering;

with Ada.Text_IO, Ada.Exceptions; --, Interfaces;

package body GID.Decoding_JPG is

  use Ada.Exceptions;
  use GID.Buffering;

  generic
    type Number is mod <>;
  procedure Big_endian_number(
    from : in out Input_buffer;
    n    :    out Number
  );
    pragma Inline(Big_endian_number);

  procedure Big_endian_number(
    from : in out Input_buffer;
    n    :    out Number
  )
  is
    b: U8;
  begin
    n:= 0;
    for i in 1..Number'Size/8 loop
      Get_Byte(from, b);
      n:= n * 256 + Number(b);
    end loop;
  end Big_endian_number;

  procedure Big_endian is new Big_endian_number( U16 );

  procedure Read( image: in out image_descriptor; sh: out Segment_head) is
    b: U8;
    id: constant array(JPEG_marker) of U8:=
    ( SOI      => 16#D8#,
      --
      SOF_0  => 16#C0#, SOF_1  => 16#C1#, SOF_2  => 16#C2#, SOF_3  => 16#C3#,
      SOF_5  => 16#C5#, SOF_6  => 16#C6#, SOF_7  => 16#C7#, SOF_8  => 16#C8#,
      SOF_9  => 16#C9#, SOF_10 => 16#CA#, SOF_11 => 16#CB#, SOF_13 => 16#CD#,
      SOF_14 => 16#CE#, SOF_15 => 16#CF#,
      --
      DHT      => 16#C4#,
      DAC      => 16#CC#,
      DQT      => 16#DB#,
      --
      APP0  => 16#E0#,  APP1  => 16#E1#,  APP2  => 16#E2#,
      APP13 => 16#ED#,  APP14 => 16#EE#,
      --
      COM      => 16#FE#,
      SOS      => 16#DA#,
      EOI      => 16#D9#
    );
  begin
    Get_Byte(image.buffer, b);
    if b /= 16#FF# then
      Raise_exception(
        error_in_image_data'Identity,
        "JPEG: expected marker here"
      );
    end if;
    Get_Byte(image.buffer, b);
    for m in id'Range loop
      if id(m)= b then
        sh.kind:= m;
        Big_endian(image.buffer, sh.length);
        sh.length:= sh.length - 2;
        -- We consider length of contents, without the FFxx marker.
        if some_trace then
          Ada.Text_IO.Put_Line(
            '[' & JPEG_marker'Image(sh.kind) &
            "], length:" & U16'Image(sh.length));
        end if;
        return;
      end if;
    end loop;
    Raise_exception(
      error_in_image_data'Identity,
      "JPEG: unknown marker here: FF, " & U8'Image(b)
    );
  end Read;

  procedure Read_DQT(image: in out Image_descriptor; data_length: Natural) is
    remaining: Integer:= data_length;
    b, q8: U8; q16: U16;
    qt_idx: Natural;
    high_prec: Boolean;
  begin
    loop
      Get_Byte(image.buffer, b);
      remaining:= remaining - 1;
      high_prec:= b >= 8;
      qt_idx:= Natural(b and 7);
      if full_trace then
        Ada.Text_IO.Put_Line("QT #" & U8'Image(b));
      end if;
      for i in JPEG_QT'Range loop
        if high_prec then
          Big_endian(image.buffer, q16);
          remaining:= remaining - 2;
          image.JPEG_stuff.qt_list(qt_idx)(i):= Natural(q16);
        else
          Get_Byte(image.buffer, q8);
          remaining:= remaining - 1;
          image.JPEG_stuff.qt_list(qt_idx)(i):= Natural(q8);
        end if;
      end loop;
      exit when remaining <= 0;
    end loop;
  end Read_DQT;

  --------------------
  -- Image decoding --
  --------------------

  procedure Load (
    image     : in out Image_descriptor;
    next_frame:    out Ada.Calendar.Day_Duration
  )
  is

    procedure Read_DHT is
    begin
      null;--!!
    end Read_DHT;

    --
    sh: Segment_head;
    b: U8;
  begin
    if some_trace then
      Ada.Text_IO.Put_Line("Frame has following components:");
      for c in JPEG_component loop
        Ada.Text_IO.Put_Line(
          JPEG_Component'Image(c) & " -> " &
          Boolean'Image(image.JPEG_stuff.components(c))
        );
      end loop;
    end if;
    if image.JPEG_stuff.components /= YCbCr then
      Raise_exception(
        error_in_image_data'Identity,
        "JPEG: only YCbCr currently supported"
      );
    end if;
    loop
      Read(image, sh);
      case sh.kind is
        when DQT =>
          Read_DQT(image, Natural(sh.length));
        when DHT =>
          Read_DHT;
        when EOI =>
          exit;
        when others =>
          -- Skip segment data
          for i in 1..sh.length loop
            Get_Byte(image.buffer, b);
          end loop;
      end case;
    end loop;
    --
    raise known_but_unsupported_image_format; -- !!
  end Load;

end GID.Decoding_JPG;
