-- Steps for decoding a JPEG image
--
-- 1. Huffman decompression
-- 2. Inverse quantization
-- 3. Inverse cosine transform
-- 4. Upsampling
-- 5. Color transformation
-- 6. Image reconstruction
--
-- The JPEG decoder is largely inspired
-- by the NanoJPEG code by Martin J. Fiedler
--   http://keyj.s2000.ws/?p=137
-- With the author's permission. Many thanks!
--
-- Other informations:
-- http://en.wikipedia.org/wiki/JPEG

-- !! profiling with some 2**n (perhaps should use shifts sometimes)
-- !! color_space, ssx, ssy ,ssxmax, ssymax
--    as generic parameters + specialized instances

with GID.Buffering;

with Ada.Text_IO, Ada.Exceptions, Ada.IO_Exceptions, Interfaces;

package body GID.Decoding_JPG is

  use Ada.Text_IO, Ada.Exceptions;
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
      DRI      => 16#DD#,
      --
      APP_0  => 16#E0#, APP_1  => 16#E1#, APP_2  => 16#E2#, APP_3  => 16#E3#,
      APP_4  => 16#E4#, APP_5  => 16#E5#, APP_6  => 16#E6#, APP_7  => 16#E7#,
      APP_8  => 16#E8#, APP_9  => 16#E9#, APP_10 => 16#EA#, APP_11 => 16#EB#,
      APP_12 => 16#EC#, APP_13 => 16#ED#, APP_14 => 16#EE#,
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
          Put_Line(
            "Segment [" & JPEG_marker'Image(sh.kind) &
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

  -- SOF - Start Of Frame (the real header)
  procedure Read_SOF(image: in out Image_descriptor; sh: Segment_head) is
    use Bounded_255;
    b, bits_pp_primary: U8;
    w, h: U16;
    compo: JPEG_Component;
  begin
    case sh.kind is
      when SOF_0 =>
        image.detailed_format:= To_Bounded_String("JPEG, Baseline DCT (SOF_0)");
      when SOF_2 =>
        image.detailed_format:= To_Bounded_String("JPEG, Progressive DCT (SOF_2)");
        image.interlaced:= True;
      when others =>
        Raise_exception(
          unsupported_image_subformat'Identity,
          "JPEG: image type not yet supported: " & JPEG_marker'Image(sh.kind)
        );
    end case;
    Get_Byte(image.buffer, bits_pp_primary);
    if bits_pp_primary /= 8 then
      Raise_exception(
        unsupported_image_subformat'Identity,
        "Bits per primary color=" & U8'Image(bits_pp_primary)
      );
    end if;
    image.bits_per_pixel:= 3 * Positive(bits_pp_primary);
    Big_endian(image.buffer, h);
    Big_endian(image.buffer, w);
    image.width:= Natural(w);
    image.height:= Natural(h);
    -- number of components:
    Get_Byte(image.buffer, b);
    image.subformat_id:= Integer(b);
    -- for each component: 3 bytes
    for i in 1..image.subformat_id loop
      -- component id (1 = Y, 2 = Cb, 3 = Cr, 4 = I, 5 = Q)
      Get_Byte(image.buffer, b);
      compo:= JPEG_Component'Val(b - 1);
      image.JPEG_stuff.components(compo):= True;
      declare
        info: JPEG_info_per_component_A renames image.JPEG_stuff.info(compo);
      begin
        -- sampling factors (bit 0-3 vert., 4-7 hor.)
        Get_Byte(image.buffer, b);
        info.samples_ver:= Natural(b mod 16);
        info.samples_hor:= Natural(b  /  16);
        -- quantization table number
        Get_Byte(image.buffer, b);
        info.qt_assoc:= Natural(b);
      end;
    end loop;
    if Natural(sh.length) < 6 + 3 * image.subformat_id then
      Raise_exception(
        error_in_image_data'Identity,
        "JPEG: SOF segment too short"
      );
    end if;
    if some_trace then
      Put_Line("Frame has following components:");
      for c in JPEG_component loop
        Put_Line(
          JPEG_Component'Image(c) & " -> " &
          Boolean'Image(image.JPEG_stuff.components(c))
        );
      end loop;
    end if;
    if image.JPEG_stuff.components = YCbCr_set then
      image.JPEG_stuff.color_space:= YCbCr;
    elsif image.JPEG_stuff.components = Y_Grey_set then
      image.JPEG_stuff.color_space:= Y_Grey;
    elsif image.JPEG_stuff.components = CMYK_set then
      image.JPEG_stuff.color_space:= CMYK;
    else
      Raise_exception(
        unsupported_image_subformat'Identity,
        "JPEG: only YCbCr, Y_Grey and CMYK color spaces are currently supported"
      );
    end if;
    image.detailed_format:= image.detailed_format & ", " &
      JPEG_supported_color_space'Image(image.JPEG_stuff.color_space);
    if some_trace then
      Put_Line(
        "Color space: " &
        JPEG_supported_color_space'Image(image.JPEG_stuff.color_space)
      );
    end if;
    if image.JPEG_stuff.color_space= CMYK then
      Raise_exception(
        unsupported_image_subformat'Identity,
        "JPEG: CMYK color space is currently not properly decoded"
      );
    end if;
  end Read_SOF;

  procedure Read_DQT(image: in out Image_descriptor; data_length: Natural) is
    remaining: Integer:= data_length; -- data remaining in segment
    b, q8: U8; q16: U16;
    qt_idx: Natural;
    high_prec: Boolean;
  begin
    multi_tables:
    loop
      Get_Byte(image.buffer, b);
      remaining:= remaining - 1;
      high_prec:= b >= 8;
      qt_idx:= Natural(b and 7);
      if some_trace then
        Put_Line("Quantization Table (QT) #" & U8'Image(b));
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
    end loop multi_tables;
  end Read_DQT;

  --------------------
  -- Image decoding --
  --------------------

  procedure Load (
    image     : in out Image_descriptor;
    next_frame:    out Ada.Calendar.Day_Duration
  )
  is

    type AC_DC is (AC, DC);

    type VLC_code is record
      bits, code: U8;
    end record;

    type VLC_table is array(0..65_535) of VLC_code;

    vlc_defs: array(AC_DC, 0..7) of VLC_table;

    procedure Read_DHT(data_length: Natural) is
      remaining: Integer:= data_length; -- data remaining in segment
      b: U8;
      ht_idx: Natural;
      kind: AC_DC;
      counts: array(1..16) of Natural;
      remain_vlc, spread, currcnt, idx: Natural;
    begin
      multi_tables:
      loop
        Get_Byte(image.buffer, b);
        remaining:= remaining - 1;
        if b >= 8 then
          kind:= AC;
        else
          kind:= DC;
        end if;
        ht_idx:= Natural(b and 7);
        if some_trace then
          Put_Line(
            "Huffman Table (HT) #" &
            Natural'Image(ht_idx) & ", " & AC_DC'Image(kind)
          );
        end if;
        for i in counts'range loop
          Get_Byte(image.buffer, b);
          remaining:= remaining - 1;
          counts(i):= Natural(b);
        end loop;
        remain_vlc:= 65_536;
        spread:= 65_536;
        idx:= 0;
        for codelen in counts'Range loop
          spread:= spread / 2;
          currcnt:= counts(codelen);
          if currcnt > 0 then
            if remaining < currcnt then
              Raise_exception(
                error_in_image_data'Identity,
                "JPEG: DHT data too short"
              );
            end if;
            remain_vlc:= remain_vlc - currcnt * spread;
            if remain_vlc < 0 then
              Raise_exception(
                error_in_image_data'Identity,
                "JPEG: DHT table too short for data"
              );
            end if;
            for i in reverse 1..currcnt loop
              Get_Byte(image.buffer, b);
              for j in reverse 1..spread loop
                vlc_defs(kind, ht_idx)(idx):= (bits => U8(codelen), code => b);
                idx:= idx + 1;
              end loop;
            end loop;
            remaining:= remaining - currcnt;
          end if;
        end loop;
        while remain_vlc > 0 loop
          remain_vlc:= remain_vlc - 1;
          vlc_defs(kind, ht_idx)(idx).bits:= 0;
          idx:= idx + 1;
        end loop;
        exit when remaining <= 0;
      end loop multi_tables;
    end Read_DHT;

    --
    -- Bit buffer
    --

    buf: U32:= 0;
    bufbits: Natural:= 0;

    function ShowBits(bits: Natural) return Natural is
      newbyte, marker: U8;
      res: Natural;
      use Interfaces;
    begin
--ada.text_IO.put_line("bufbits (before)= " & bufbits'img);
      if bits=0 then
        return 0;
      end if;
      while bufbits < bits loop
        begin
        Get_Byte(image.buffer, newbyte);
        bufbits:= bufbits + 8;
        buf:= buf * 256 + U32(newbyte);
        if newbyte = 16#FF# then
          Get_Byte(image.buffer, marker);
          case marker is
            when 0 =>
              null;
            when 16#D9# =>
              null; -- !! signal end
            when 16#D0# .. 16#D7# =>
              bufbits:= bufbits + 8;
              buf:= buf * 256 + U32(marker);
            when others =>
              Raise_exception(
                error_in_image_data'Identity,
                "JPEG: Invalid code (bit buffer)"
              );
          end case;
        end if;
        exception
          when Ada.IO_Exceptions.End_Error =>
            newbyte:= 16#FF#;
            bufbits:= bufbits + 8;
            buf:= buf * 256 + U32(newbyte);
        end;
      end loop;
--ada.text_IO.put_line("bufbits (after)= " & bufbits'img);
--      Ada.Text_IO.Put_Line("buf=" & buf'img);
      res:= Natural( Shift_Right(Unsigned_32(buf), bufbits - bits) and (2**bits-1) );
--      Ada.Text_IO.Put_Line("showbits" & bits'img & "->" & res'img);
      return res;
    end ShowBits;

    procedure SkipBits(bits: Natural) is
      dummy: Integer;
      pragma Warnings(off, dummy);
    begin
      if bufbits < bits then
        dummy:= ShowBits(bits);
      end if;
      bufbits:= bufbits - bits;
    end;

    function GetBits(bits: Natural) return INteger is
      res: constant Integer:= ShowBits(bits);
    begin
      SkipBits(bits);
      return res;
    end;

    procedure ByteAlign is
    begin
      bufbits:= Natural(U32(bufbits) and 16#F8#);
    end;

    --

    type JPEG_info_per_component_B is record
      ht_idx_AC : Natural;
      ht_idx_DC : Natural;
      width, height, stride: Natural;
      dcpred: Integer:= 0;
    end record;

    info_A: JPEG_component_info_A renames image.JPEG_stuff.info;
    info_B: array(JPEG_Component) of JPEG_info_per_component_B;

    procedure GetVLC(
      vlc: VLC_table;
      code: out U8;
      value_ret: out Integer
    )
    is
      -- Step 1 happens here: Huffman decompression
      value: Integer:= ShowBits(16);
      bits: Integer:= Integer(vlc(value).bits);
    begin
      if bits = 0 then
        Raise_exception(
          error_in_image_data'Identity,
          "JPEG: VLC table: bits = 0"
        );
      end if;
      SkipBits(bits);
      value:= Integer(vlc(value).code);
      code:= U8(value);
      bits:= value mod 16;
      value_ret:= 0;
      if bits /= 0 then
        value:= GetBits(bits);
        if value < 2 ** (bits - 1) then
          value:= value + 1 - 2 ** bits;
        end if;
        value_ret:= value;
      end if;
--ada.text_IO.put_line(value_ret'img);
    end GetVLC;

    function Clip(x: Integer) return Integer is
    pragma Inline(Clip);
    begin
      if x < 0 then
        return 0;
      elsif x > 255 then
        return 255;
      else
        return x;
      end if;
    end Clip;

    type Block_8x8 is array(0..63) of Integer;

    -- Ordering within a 8x8 block, in zig-zag
    zig_zag: constant Block_8x8:=
     ( 0,  1,  8, 16,  9,  2,  3, 10, 17, 24, 32, 25, 18,
      11,  4,  5, 12, 19, 26, 33, 40, 48, 41, 34, 27, 20,
      13,  6,  7, 14, 21, 28, 35, 42, 49, 56, 57, 50, 43,
      36, 29, 22, 15, 23, 30, 37, 44, 51, 58, 59, 52, 45,
      38, 31, 39, 46, 53, 60, 61, 54, 47, 55, 62, 63 );

    procedure Decode_Block(c: JPEG_Component; block: in out Block_8x8) is
      value, coef: Integer;
      code: U8;
      qt: JPEG_QT renames image.JPEG_stuff.qt_list(info_A(c).qt_assoc);
      --
      W1: constant:= 2841;
      W2: constant:= 2676;
      W3: constant:= 2408;
      W5: constant:= 1609;
      W6: constant:= 1108;
      W7: constant:=  565;
      --
      procedure RowIDCT(start: Integer) is
      pragma Inline(RowIDCT);
        x0, x1, x2, x3, x4, x5, x6, x7, x8, val: Integer;
      begin
        x1:= block(start + 4) * 2**11;
        x2:= block(start + 6);
        x3:= block(start + 2);
        x4:= block(start + 1);
        x5:= block(start + 7);
        x6:= block(start + 5);
        x7:= block(start + 3);
        if x1=0 and x2=0 and x3=0 and x4=0 and x5=0 and x6=0 and x7=0 then
          val:= block(start + 0) * 8;
          block(start + 0 .. start + 7):= (others => val);
        else
          x0:= (block(start + 0) * 2**11) + 128;
          x8:= W7 * (x4 + x5);
          x4:= x8 + (W1 - W7) * x4;
          x5:= x8 - (W1 + W7) * x5;
          x8:= W3 * (x6 + x7);
          x6:= x8 - (W3 - W5) * x6;
          x7:= x8 - (W3 + W5) * x7;
          x8:= x0 + x1;
          x0:= x0 - x1;
          x1:= W6 * (x3 + x2);
          x2:= x1 - (W2 + W6) * x2;
          x3:= x1 + (W2 - W6) * x3;
          x1:= x4 + x6;
          x4:= x4 - x6;
          x6:= x5 + x7;
          x5:= x5 - x7;
          x7:= x8 + x3;
          x8:= x8 - x3;
          x3:= x0 + x2;
          x0:= x0 - x2;
          x2:= (181 * (x4 + x5) + 128) / 256;
          x4:= (181 * (x4 - x5) + 128) / 256;
          block(start + 0):= (x7 + x1) / 256;
          block(start + 1):= (x3 + x2) / 256;
          block(start + 2):= (x0 + x4) / 256;
          block(start + 3):= (x8 + x6) / 256;
          block(start + 4):= (x8 - x6) / 256;
          block(start + 5):= (x0 - x4) / 256;
          block(start + 6):= (x3 - x2) / 256;
          block(start + 7):= (x7 - x1) / 256;
        end if;
--ada.text_IO.put("RowDCT:");
--for i in 0..7 loop
--ada.text_IO.put(" " & block(start + i)'img);
--end loop;
--ada.text_IO.New_Line;
      end RowIDCT;

      procedure ColIDCT(start: Integer) is
      pragma Inline(ColIDCT);
        x0, x1, x2, x3, x4, x5, x6, x7, x8, val: Integer;
      begin
        x1:= block(start + 8*4) * 256;
        x2:= block(start + 8*6);
        x3:= block(start + 8*2);
        x4:= block(start + 8*1);
        x5:= block(start + 8*7);
        x6:= block(start + 8*5);
        x7:= block(start + 8*3);
        if x1=0 and x2=0 and x3=0 and x4=0 and x5=0 and x6=0 and x7=0 then
          val:= Clip(((block(start) + 32) / 2**6) + 128);
          for row in reverse 0..7 loop
            block(start + row * 8):= val;
          end loop;
        else
          x0:= (block(start) * 256) + 8192;
          x8:= W7 * (x4 + x5) + 4;
          x4:= (x8 + (W1 - W7) * x4) / 8;
          x5:= (x8 - (W1 + W7) * x5) / 8;
          x8:= W3 * (x6 + x7) + 4;
          x6:= (x8 - (W3 - W5) * x6) / 8;
          x7:= (x8 - (W3 + W5) * x7) / 8;
          x8:= x0 + x1;
          x0:= x0 - x1;
          x1:= W6 * (x3 + x2) + 4;
          x2:= (x1 - (W2 + W6) * x2) / 8;
          x3:= (x1 + (W2 - W6) * x3) / 8;
          x1:= x4 + x6;
          x4:= x4 - x6;
          x6:= x5 + x7;
          x5:= x5 - x7;
          x7:= x8 + x3;
          x8:= x8 - x3;
          x3:= x0 + x2;
          x0:= x0 - x2;
          x2:= (181 * (x4 + x5) + 128) / 256;
          x4:= (181 * (x4 - x5) + 128) / 256;
          block(start + 8*0):= Clip(((x7 + x1) / 2**14) + 128);
          block(start + 8*1):= Clip(((x3 + x2) / 2**14) + 128);
          block(start + 8*2):= Clip(((x0 + x4) / 2**14) + 128);
          block(start + 8*3):= Clip(((x8 + x6) / 2**14) + 128);
          block(start + 8*4):= Clip(((x8 - x6) / 2**14) + 128);
          block(start + 8*5):= Clip(((x0 - x4) / 2**14) + 128);
          block(start + 8*6):= Clip(((x3 - x2) / 2**14) + 128);
          block(start + 8*7):= Clip(((x7 - x1) / 2**14) + 128);
        end if;
      end ColIDCT;

    begin -- Decode_Block
      --
      -- Step 2 happens here: Inverse quantization
      GetVLC(vlc_defs(DC, info_B(c).ht_idx_DC), code, value);
      -- First value in block (0: top left) uses a predictor.
      info_B(c).dcpred:= info_B(c).dcpred + value;
      block(0):= info_B(c).dcpred * qt(0);
      block(1..63):= (others => 0);
      coef:= 0;
      loop
        GetVLC(vlc_defs(AC, info_B(c).ht_idx_AC), code, value);
        exit when code = 0; -- EOB
        coef:= coef + Integer(code / 16) + 1;
        if coef > 63 then
          Raise_exception(
            error_in_image_data'Identity,
            "JPEG: coefficient for de-quantization is > 63"
          );
        end if;
        block(zig_zag(coef)):= value * qt(coef);
        exit when coef = 63;
      end loop;
--ada.text_IO.put("Block dump TB:");
--for i in 0..63 loop
--ada.text_IO.put(" " & block(i)'img);
--end loop;
--ada.text_IO.New_Line;
      -- Step 3 happens here: Inverse cosine transform
      for row in 0..7 loop
        RowIDCT(row * 8);
      end loop;
      for col in 0..7 loop
        ColIDCT(col);
      end loop;
    end Decode_Block;

    rstinterval: U16:= 0;

    type Macro_block is array(
      JPEG_Component range <>, -- component
      Positive range <>,       -- x sample range
      Positive range <>        -- y sample range
    ) of Block_8x8;

      procedure Out_Pixel_8(br, bg, bb: U8) is
      pragma Inline(Out_Pixel_8);
        ba: constant:= 255;
      begin
        case Primary_color_range'Modulus is
          when 256 =>
            Put_Pixel(
              Primary_color_range(br),
              Primary_color_range(bg),
              Primary_color_range(bb),
              Primary_color_range(ba)
            );
          when 65_536 =>
            Put_Pixel(
              16#101# * Primary_color_range(br),
              16#101# * Primary_color_range(bg),
              16#101# * Primary_color_range(bb),
              16#101# * Primary_color_range(ba)
              -- 16#101# because max intensity FF goes to FFFF
            );
          when others =>
            raise invalid_primary_color_range;
        end case;
      end Out_Pixel_8;

    procedure Upsampling_and_output(
      m: Macro_block;
      x0, y0: Natural
    )
    is
    pragma Inline(Upsampling_and_output);
      ssxmax: constant Positive:= m'Last(2);
      ssymax: constant Positive:= m'Last(3);
      flat: array(
        JPEG_Component,
        0..8*ssxmax-1,
        0..8*ssymax-1
      ) of Integer;
      blk_idx: Integer;
      val: Integer;
      ssx, ssy, upsx, upsy: Positive;
      y_val, cb_val, cr_val, c_val, m_val, w_val: Integer;
      y_val_8: U8;
    begin
      -- Step 4 happens here: Upsampling
      for c in JPEG_Component loop
        if image.JPEG_stuff.components(c) then
          ssx:= info_A(c).samples_hor;
          ssy:= info_A(c).samples_ver;
          upsx:= ssxmax/ssx;
          upsy:= ssymax/ssy;
          for x in 1..ssx loop
            for y in 1..ssy loop
              -- We are at the 8x8 block level
              blk_idx:= 0;
              for y8 in 0..7 loop
                for x8 in 0..7 loop
                  val:= m(c,x,y)(blk_idx);
                  -- Repeat pixels for component c, sample (x,y),
                  -- position (x8,y8).
                  for rx in reverse 0..upsx-1 loop
                    for ry in reverse 0..upsy-1 loop
                      flat(
                        c,
                        rx + upsx * (x8 + 8*(x-1)),
                        ry + upsy * (y8 + 8*(y-1))
                      ):= val;
                    end loop;
                  end loop;
                  blk_idx:= blk_idx + 1;
                end loop;
              end loop;
            end loop;
          end loop;
        end if;
      end loop;
      -- Step 5 and 6 happen here: Color transformation and output
      for ymb in flat'Range(3) loop
        exit when y0+ymb >= image.height;
        Set_X_Y(x0, image.height-1-(y0+ymb));
        for xmb in flat'Range(2) loop
          exit when x0+xmb >= image.width;
          case image.JPEG_stuff.color_space is
            when YCbCR =>
              y_val := flat(Y,  xmb, ymb) * 256;
              cb_val:= flat(Cb, xmb, ymb) - 128;
              cr_val:= flat(Cr, xmb, ymb) - 128;
              Out_pixel_8(
                br => U8(Clip((y_val                + 359 * cr_val + 128) / 256)),
                bg => U8(Clip((y_val -  88 * cb_val - 183 * cr_val + 128) / 256)),
                bb => U8(Clip((y_val + 454 * cb_val                + 128) / 256))
              );
            when Y_Grey =>
              y_val_8:= U8(flat(Y,  xmb, ymb));
              Out_pixel_8(y_val_8, y_val_8, y_val_8);
            when CMYK =>
              -- !! find a working conversion formula.
              --    perhaps it is more complicated (APP_2
              --    color profile must be used ?)
              c_val:= flat(Y,  xmb, ymb);
              m_val:= flat(Cb, xmb, ymb);
              y_val:= flat(Cr, xmb, ymb);
              w_val:= flat(I,  xmb, ymb)-255;
              Out_pixel_8(
                br => U8(255-Clip(c_val+w_val)),
                bg => U8(255-Clip(m_val+w_val)),
                bb => U8(255-Clip(y_val+w_val))
              );
          end case;
        end loop;
      end loop;
    end Upsampling_and_output;

    -- Start Of Scan (and image data which follow)
    --
    procedure Read_SOS is
      components, b: U8;
      compo: JPEG_Component;
      mbx, mby: Natural:= 0;
      ssxmax, ssymax: Natural:= 0;
      mbsizex, mbsizey, mbwidth, mbheight: Natural;
      rstcount: U16:= rstinterval;
      nextrst: U16:= 0;
      w: U16;
      start_spectral_selection,
      end_spectral_selection,
      successive_approximation: U8;
    begin
      Get_Byte(image.buffer, components);
      if some_trace then
        Put_Line(
          "Start of Scan (SOS), with" &
          U8'Image(components) & " components"
        );
      end if;
      if image.subformat_id /= Natural(components) then
        Raise_exception(
          error_in_image_data'Identity,
          "JPEG: components mismatch in Scan segment"
        );
      end if;
      for i in 1..components loop
        Get_Byte(image.buffer, b);
        compo:= JPEG_Component'Val(b - 1);
        if not image.JPEG_stuff.components(compo) then
          Raise_exception(
            error_in_image_data'Identity,
            "JPEG: component " & JPEG_Component'Image(compo) &
            " has not been defined in the SOF segment"
          );
        end if;
        -- Huffman table selection
        Get_Byte(image.buffer, b);
        info_B(compo).ht_idx_AC:= Natural(b mod 16);
        info_B(compo).ht_idx_DC:= Natural(b  /  16);
        ssxmax:= Integer'Max(ssxmax, info_A(compo).samples_hor);
        ssymax:= Integer'Max(ssymax, info_A(compo).samples_ver);
      end loop;
      -- Parameters for progressive display format (SOF_2)
      Get_Byte(image.buffer, start_spectral_selection);
      Get_Byte(image.buffer, end_spectral_selection);
      Get_Byte(image.buffer, successive_approximation);
      --
      -- End of SOS segment, image data follow.
      --
      mbsizex:= ssxmax * 8; -- pixels in a row of a macro-block
      mbsizey:= ssymax * 8; -- pixels in a column of a macro-block
      mbwidth := (image.width + mbsizex - 1) / mbsizex;
      -- width in macro-blocks
      mbheight:= (image.height + mbsizey - 1) / mbsizey;
      -- height in macro-blocks
      if some_trace then
        Put_Line(" mbsizex = " & Integer'Image(mbsizex));
        Put_Line(" mbsizey = " & Integer'Image(mbsizey));
        Put_Line(" mbwidth  = " & Integer'Image(mbwidth));
        Put_Line(" mbheight = " & Integer'Image(mbheight));
      end if;
      for c in JPEG_Component loop
        if image.JPEG_stuff.components(c) then
          info_B(c).width := (image.width  * info_A(c).samples_hor + ssxmax - 1) / ssxmax;
          info_B(c).height:= (image.height * info_A(c).samples_ver + ssymax - 1) / ssymax;
          info_B(c).stride:= (mbwidth * mbsizex * info_A(c).samples_hor) / ssxmax;
          if some_trace then
            Put_Line("  Details for component " & JPEG_Component'Image(c));
            Put_Line("    samples in x " & Integer'Image(info_A(c).samples_hor));
            Put_Line("    samples in y " & Integer'Image(info_A(c).samples_ver));
            Put_Line("    width " & Integer'Image(info_B(c).width));
            Put_Line("    height " & Integer'Image(info_B(c).height));
            Put_Line("    stride " & Integer'Image(info_B(c).stride));
            Put_Line(
              "    AC/DC table index " &
              Integer'Image(info_B(compo).ht_idx_AC) & ", " &
              Integer'Image(info_B(compo).ht_idx_DC)
            );
          end if;
          if (info_B(c).width < 3 and info_A(c).samples_hor /= ssxmax) or
             (info_B(c).height < 3 and info_A(c).samples_ver /= ssymax)
          then
            Raise_exception(
              error_in_image_data'Identity,
              "JPEG: component " & JPEG_Component'Image(c) &
              ": sample dimension mismatch"
            );
          end if;
        end if;
      end loop;
      --
      if image.interlaced then
        Raise_exception(
          unsupported_image_subformat'Identity,
          "JPEG: progressive format not yet functional"
        );
      end if;
      declare
        mb: Macro_block(JPEG_Component, 1..ssxmax, 1..ssymax);
        x0, y0: Integer:= 0;
      begin
      macro_blocks_loop:
      loop
        components_loop:
        for c in JPEG_Component loop
          if image.JPEG_stuff.components(c) then
            samples_y_loop:
            for sby in 1..info_A(c).samples_ver loop
              samples_x_loop:
              for sbx in 1..info_A(c).samples_hor loop
                Decode_Block(c, mb(c, sbx, sby));
              end loop samples_x_loop;
            end loop samples_y_loop;
          end if;
        end loop components_loop;
        -- All components of the current macro-block are decoded.
        -- Step 4, 5, 6 happen here: Upsampling, color transformation, output
        Upsampling_and_output(mb, x0, y0);
        --
        mbx:= mbx + 1;
        x0:= x0 + ssxmax * 8;
        if mbx >= mbwidth then
          mbx:= 0;
          x0:= 0;
          mby:= mby + 1;
          y0:= y0 + ssymax * 8;
          Feedback((100*mby)/mbheight);
          exit macro_blocks_loop when mby >= mbheight;
        end if;
        if rstinterval > 0 then
          rstcount:= rstcount - 1;
          if rstcount = 0 then
            -- Here the restart occurs:
            ByteAlign;
            w:= U16(GetBits(16));
            if some_trace then
              Put_Line(
                "  Restart #" & U16'Image(nextrst) &
                "  Code " & U16'Image(w) &
                " after" & U16'Image(rstinterval) & " macro blocks"
              );
            end if;
            if w not in 16#FFD0# .. 16#FFD8# or
              (w and 7) /= nextrst
            then
              Raise_exception(
                error_in_image_data'Identity,
                "JPEG: expected RST (restart) marker Nb " &
                U16'Image(nextrst)
              );
            end if;
            nextrst:= (nextrst + 1) and 7;
            rstcount:= rstinterval;
            -- Block-to-block predictor variables are reset.
            for c in JPEG_Component loop
              info_B(c).dcpred:= 0;
            end loop;
          end if;
        end if;
      end loop macro_blocks_loop;
      end;
      if some_trace then
        Put_Line("Image decoded !");
      end if;
    end Read_SOS;

    --
    sh: Segment_head;
    b: U8;
  begin -- Load
    loop
      Read(image, sh);
      case sh.kind is
        when DQT => -- Quantization Table
          Read_DQT(image, Natural(sh.length));
        when DHT => -- Huffman Table
          Read_DHT(Natural(sh.length));
        when DRI => -- Reset Interval
          Big_endian(image.buffer, rstinterval);
        when EOI => -- End Of Input
          exit;
        when SOS => -- Start Of Scan
          Read_SOS;
          exit;
        when others =>
          -- Skip segment data
          for i in 1..sh.length loop
            Get_Byte(image.buffer, b);
          end loop;
      end case;
    end loop;
    next_frame:= 0.0; -- still picture
  end Load;

end GID.Decoding_JPG;
