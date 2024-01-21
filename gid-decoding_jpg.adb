--  The GID JPEG decoder is largely inspired
--  by the NanoJPEG code by Martin J. Fiedler.
--  With the author's permission. Web link:
--  https://keyj.emphy.de/nanojpeg/
--
--  The progressive decoding is largely inspired
--  by the PyJpegDecoder by Tiago Becerra Paolini,
--  available under MIT Licence. Web link:
--  https://github.com/tbpaolini/PyJpegDecoder

--  Other informations:
--    JPEG standard
--      ISO/IEC 10918-1 : 1993(E)
--      CCITT Rec. T.81 (1992 E)
--      https://www.w3.org/Graphics/JPEG/itu-t81.pdf
--
--    General information:
--      http://en.wikipedia.org/wiki/JPEG

--  Steps for decoding a JPEG image
--
--      1. Huffman decompression
--      2. Inverse quantization
--      3. Inverse cosine transform
--      4. Upsampling
--      5. Color transformation
--      6. Image reconstruction

--  !! ** Some optimizations to consider **
--  !! ssx, ssy ,ssxmax, ssymax
--       as generic parameters + specialized instances
--  !! consider only power-of-two upsampling factors ?
--  !! simplify upsampling loops in case of power-of-two upsampling factors
--       using Shift_Right
--  !! Col_IDCT output direct to "flat", or something similar to NanoJPEG

with GID.Buffering;

with Ada.Integer_Text_IO, Ada.IO_Exceptions,
     Ada.Text_IO, Ada.Unchecked_Deallocation;

package body GID.Decoding_JPG is

  use Buffering, Ada.Text_IO, Interfaces;

  generic
    type Number is mod <>;
  procedure Big_Endian_Number
    (from : in out Input_Buffer;
     n    :    out Number);
  pragma Inline (Big_Endian_Number);

  procedure Big_Endian_Number
    (from : in out Input_Buffer;
     n    :    out Number)
  is
    b : U8;
  begin
    n := 0;
    for i in 1 .. Number'Size / 8 loop
      Get_Byte (from, b);
      n := n * 256 + Number (b);
    end loop;
  end Big_Endian_Number;

  procedure Big_Endian is new Big_Endian_Number (U16);

  procedure Read (image : in out Image_Descriptor; sh : out Segment_Head) is
    b : U8;
    id : constant array (JPEG_Marker) of U8 :=
     (SOI => 16#D8#,
      --
      SOF_0  => 16#C0#, SOF_1  => 16#C1#, SOF_2  => 16#C2#, SOF_3  => 16#C3#,
      SOF_5  => 16#C5#, SOF_6  => 16#C6#, SOF_7  => 16#C7#, SOF_8  => 16#C8#,
      SOF_9  => 16#C9#, SOF_10 => 16#CA#, SOF_11 => 16#CB#, SOF_13 => 16#CD#,
      SOF_14 => 16#CE#, SOF_15 => 16#CF#,
      --
      DHT => 16#C4#,
      DAC => 16#CC#,
      DQT => 16#DB#,
      DRI => 16#DD#,
      --
      APP_0  => 16#E0#, APP_1  => 16#E1#, APP_2  => 16#E2#, APP_3  => 16#E3#,
      APP_4  => 16#E4#, APP_5  => 16#E5#, APP_6  => 16#E6#, APP_7  => 16#E7#,
      APP_8  => 16#E8#, APP_9  => 16#E9#, APP_10 => 16#EA#, APP_11 => 16#EB#,
      APP_12 => 16#EC#, APP_13 => 16#ED#, APP_14 => 16#EE#,
      --
      COM => 16#FE#,
      SOS => 16#DA#,
      EOI => 16#D9#);
  begin
    Get_Byte (image.buffer, b);
    if b /= 16#FF# then
      raise error_in_image_data with "JPEG: expected marker (16#FF#) here";
    end if;
    Get_Byte (image.buffer, b);
    if full_trace then
      Put_Line ("Segment Marker has been read");
    end if;
    for m in id'Range loop
      if id (m) = b then
        sh.kind := m;
        Big_Endian (image.buffer, sh.length);
        sh.length := sh.length - 2;
        --  We consider length of contents, without the FFxx marker.
        if some_trace then
          Put_Line
            ("Segment [" & sh.kind'Image & "], length:" & sh.length'Image);
        end if;
        return;
      end if;
    end loop;
    raise error_in_image_data
      with "JPEG: unknown marker here: 16#FF#, then" & b'Image;
  end Read;

  shift_arg : constant array (0 .. 15) of Integer :=
    (1 => 0, 2 => 1, 4 => 2, 8 => 3, others => -1);

  --  SOF - Start Of Frame (the real header)
  procedure Read_SOF (image : in out Image_Descriptor; sh : Segment_Head) is
    use Bounded_255;
    b, bits_pp_primary, id_base : U8;
    w, h : U16;
    compo : JPEG_Defs.Component;
  begin
    case sh.kind is
      when SOF_0 =>
        image.detailed_format := To_Bounded_String ("JPEG, Baseline DCT (SOF_0)");
      when SOF_2 =>
        image.detailed_format := To_Bounded_String ("JPEG, Progressive DCT (SOF_2)");
        image.progressive := True;
      when others =>
        raise unsupported_image_subformat with
          "JPEG: image type not yet supported: " & sh.kind'Image;
    end case;
    Get_Byte (image.buffer, bits_pp_primary);
    if bits_pp_primary /= 8 then
      raise unsupported_image_subformat with
        "JPEG: bits per primary color=" & bits_pp_primary'Image & " (not supported)";
    end if;
    image.bits_per_pixel := 3 * Positive (bits_pp_primary);
    Big_Endian (image.buffer, h);
    Big_Endian (image.buffer, w);
    if w = 0 then
      raise error_in_image_data with "JPEG: zero image width";
    end if;
    if h = 0 then
      raise error_in_image_data with "JPEG: zero image height";
    end if;
    image.width  := Positive_32 (w);
    image.height := Positive_32 (h);
    --  Number of components:
    Get_Byte (image.buffer, b);
    image.subformat_id := Integer (b);
    --
    image.JPEG_stuff.max_samples_hor := 0;
    image.JPEG_stuff.max_samples_ver := 0;
    id_base := 1;
    --  For each component: 3 bytes information: ID, sampling factors, quantization table number
    for i in 1 .. image.subformat_id loop
      --  Component ID (1 = Y, 2 = Cb, 3 = Cr, 4 = I, 5 = Q)
      Get_Byte (image.buffer, b);
      if b = 0 then
        --  Workaround for a bug in some encoders, for instance Intel(R) JPEG Library,
        --  version [2.0.18.50] as in some Photoshop versions : IDs are numbered 0, 1, 2.
        id_base := 0;
      end if;
      if b - id_base > Component'Pos (Component'Last) then
        raise error_in_image_data with "JPEG: SOF: invalid component ID: " & b'Image;
      end if;
      compo := JPEG_Defs.Component'Val (b - id_base);
      image.JPEG_stuff.compo_set (compo) := True;
      declare
        stuff : JPEG_Stuff_Type renames image.JPEG_stuff;
        info : JPEG_Defs.Info_per_Component_A renames stuff.info (compo);
      begin
        --  Sampling factors (bit 0-3 vert., 4-7 hor.)
        Get_Byte (image.buffer, b);
        info.samples_hor := Natural (b  /  16);
        info.samples_ver := Natural (b mod 16);
        info.repeat      := info.samples_hor * info.samples_ver;
        info.shape_x     := info.samples_hor * 8;
        info.shape_y     := info.samples_ver * 8;
        stuff.max_samples_hor :=
          Integer'Max (stuff.max_samples_hor, info.samples_hor);
        stuff.max_samples_ver :=
          Integer'Max (stuff.max_samples_ver, info.samples_ver);
        --  Quantization table number
        Get_Byte (image.buffer, b);
        info.qt_assoc := Natural (b);
      end;
    end loop;

    for c in Component loop
      if image.JPEG_stuff.compo_set (c) then
        declare
          stuff : JPEG_Stuff_Type renames image.JPEG_stuff;
          info : JPEG_Defs.Info_per_Component_A renames stuff.info (c);
        begin
          info.up_factor_x := stuff.max_samples_hor / info.samples_hor;
          info.up_factor_y := stuff.max_samples_ver / info.samples_ver;
          info.shift_x := shift_arg (info.up_factor_x);
          info.shift_y := shift_arg (info.up_factor_y);
        end;
      end if;
    end loop;

    if Natural (sh.length) < 6 + 3 * image.subformat_id then
      raise error_in_image_data with "JPEG: SOF segment too short";
    end if;
    if some_trace then
      Put_Line ("Frame has following components:");
      for c in JPEG_Defs.Component loop
        Put_Line (c'Image & " -> " & image.JPEG_stuff.compo_set (c)'Image);
      end loop;
    end if;
    if image.JPEG_stuff.compo_set = YCbCr_set then
      image.JPEG_stuff.color_space := YCbCr;
    elsif image.JPEG_stuff.compo_set = Y_Grey_set then
      image.JPEG_stuff.color_space := Y_Grey;
      image.greyscale := True;
    elsif image.JPEG_stuff.compo_set = CMYK_set then
      image.JPEG_stuff.color_space := CMYK;
    else
      raise unsupported_image_subformat with
        "JPEG: only YCbCr, Y_Grey and CMYK color spaces are currently supported";
    end if;
    image.detailed_format := image.detailed_format & ", " &
      image.JPEG_stuff.color_space'Image;
    if some_trace then
      Put_Line ("Color space: " & image.JPEG_stuff.color_space'Image);
    end if;
    if image.JPEG_stuff.color_space = CMYK then
      raise unsupported_image_subformat with
        "JPEG: CMYK color space is currently not properly decoded";
    end if;
  end Read_SOF;

  procedure Read_DHT (image : in out Image_Descriptor; data_length : Natural) is
    remaining : Integer_M32 := Integer_M32 (data_length);
    --  ^ data remaining in segment
    b : U8;
    ht_idx : Natural;
    kind : AC_DC;
    counts : array (1 .. 16) of Integer_M32;
    idx : Natural;
    currcnt, spread, remain_vlc : Integer_M32;
  begin
    multi_tables :
    loop
      Get_Byte (image.buffer, b);
      remaining := remaining - 1;
      if b >= 8 then
        kind := AC;
      else
        kind := DC;
      end if;
      ht_idx := Natural (b and 7);
      if some_trace then
        Put_Line
          ("  Huffman Table (HT) #" & ht_idx'Image &
           ", of kind (AC/DC): " & kind'Image);
      end if;
      if image.JPEG_stuff.vlc_defs (kind, ht_idx) = null then
        image.JPEG_stuff.vlc_defs (kind, ht_idx) := new VLC_table;
      end if;
      for i in counts'Range loop
        Get_Byte (image.buffer, b);
        remaining := remaining - 1;
        counts (i) := Integer_M32 (b);
      end loop;
      remain_vlc := 65_536;
      spread := 65_536;
      idx := 0;
      for codelen in counts'Range loop
        spread := spread / 2;
        currcnt := counts (codelen);
        if currcnt > 0 then
          if remaining < currcnt then
            raise error_in_image_data with "JPEG: DHT data too short";
          end if;
          remain_vlc := remain_vlc - currcnt * spread;
          if remain_vlc < 0 then
            raise error_in_image_data with "JPEG: DHT table too short for data";
          end if;
          for i in reverse 1 .. currcnt loop
            Get_Byte (image.buffer, b);
            for j in reverse 1 .. spread loop
              image.JPEG_stuff.vlc_defs (kind, ht_idx)(idx) :=
                (bits => U8 (codelen), code => b);
              idx := idx + 1;
            end loop;
          end loop;
          remaining := remaining - currcnt;
        end if;
      end loop;
      while remain_vlc > 0 loop
        remain_vlc := remain_vlc - 1;
        image.JPEG_stuff.vlc_defs (kind, ht_idx)(idx).bits := 0;
        idx := idx + 1;
      end loop;
      exit multi_tables when remaining <= 0;
    end loop multi_tables;
  end Read_DHT;

  procedure Read_DQT (image : in out Image_Descriptor; data_length : Natural) is
    remaining : Integer := data_length; -- data remaining in segment
    b, q8 : U8; q16 : U16;
    qt_idx : Natural;
    high_prec : Boolean;
  begin
    multi_tables :
    loop
      Get_Byte (image.buffer, b);
      remaining := remaining - 1;
      high_prec := b >= 8;
      qt_idx := Natural (b and 7);
      if some_trace then
        Put_Line ("  Quantization Table (QT) #" & b'Image);
      end if;
      for i in QT'Range loop
        if high_prec then
          Big_Endian (image.buffer, q16);
          remaining := remaining - 2;
          image.JPEG_stuff.qt_list (qt_idx)(i) := Natural (q16);
        else
          Get_Byte (image.buffer, q8);
          remaining := remaining - 1;
          image.JPEG_stuff.qt_list (qt_idx)(i) := Natural (q8);
        end if;
      end loop;
      exit multi_tables when remaining <= 0;
    end loop multi_tables;
  end Read_DQT;

  procedure Read_DRI (image : in out Image_Descriptor) is
    ri : U16;
  begin
    Big_Endian (image.buffer, ri);
    if some_trace then
      Put_Line ("  Restart interval set to:" & ri'Image);
    end if;
    image.JPEG_stuff.restart_interval := Natural (ri);
  end Read_DRI;

  procedure Read_EXIF (image : in out Image_Descriptor; data_length : Natural) is
    b, orientation_value : U8;
    x, ifd0_entries : Natural;
    Exif_signature : constant String := "Exif" & ASCII.NUL & ASCII.NUL;
    signature : String (1 .. 6);
    IFD_tag : U16;
    endianness : Character;
    --  'M' (Motorola) or 'I' (Intel): EXIF chunks may have different endiannesses,
    --  even though the whole JPEG format has a fixed endianness!
  begin
    if some_trace then
      Put_Line ("APP1");
    end if;
    if data_length < 6 then
      --  Skip segment data
      for i in 1 .. data_length loop
        Get_Byte (image.buffer, b);
      end loop;
    else
      for i in 1 .. 6 loop
        Get_Byte (image.buffer, b);
        signature (i) := Character'Val (b);
      end loop;
      if signature /= Exif_signature then
        for i in 7 .. data_length loop -- Skip remaining of APP1 data
          Get_Byte (image.buffer, b); -- since we don't know how to use it.
        end loop;
        if some_trace then
          Put_Line ("APP1 is not Exif");
        end if;
        return;
      end if;
      Get_Byte (image.buffer, b); -- TIFF 6.0 header (1st of 8 bytes)
      endianness := Character'Val (b);
      if some_trace then
        Put_Line ("APP1 is Exif; endianness is " & endianness);
      end if;
      for i in 8 .. 14 loop -- TIFF 6.0 header (2-8 of 8 bytes)
        Get_Byte (image.buffer, b);
      end loop;
      --  Number of IFD0 entries (2 bytes)
      ifd0_entries := 0;
      Get_Byte (image.buffer, b);
      ifd0_entries := Natural (b);
      Get_Byte (image.buffer, b);
      if endianness = 'I' then
        ifd0_entries := ifd0_entries + 16#100# * Natural (b);
      else
        ifd0_entries := Natural (b) + 16#100# * ifd0_entries;
      end if;
      if some_trace then
        Put_Line ("EXIF's IFD0 has" & ifd0_entries'Image & " entries.");
      end if;
      x := 17;
      while x <= data_length - 12 loop
        Get_Byte (image.buffer, b);
        IFD_tag := U16 (b);
        Get_Byte (image.buffer, b);
        if endianness = 'I' then
          IFD_tag := IFD_tag + 16#100# * U16 (b);
        else
          IFD_tag := U16 (b) + 16#100# * IFD_tag;
        end if;
        if some_trace then
          Put ("IFD tag:"); Ada.Integer_Text_IO.Put (Natural (IFD_tag), Base => 16); New_Line;
        end if;
        for i in 3 .. 8 loop
          Get_Byte (image.buffer, b);
        end loop;
        if endianness = 'I' then
          Get_Byte (image.buffer, orientation_value);
          for i in 10 .. 12 loop
            Get_Byte (image.buffer, b);
          end loop;
        else
          Get_Byte (image.buffer, b);
          Get_Byte (image.buffer, orientation_value);
          Get_Byte (image.buffer, b);
          Get_Byte (image.buffer, b);
        end if;
        x := x + 12;
        if IFD_tag = 16#112# then
          case orientation_value is
            when 1 =>
              image.display_orientation := Unchanged;
            when 8 =>
              image.display_orientation := Rotation_90;
            when 3 =>
              image.display_orientation := Rotation_180;
            when 6 =>
              image.display_orientation := Rotation_270;
            when others =>
              image.display_orientation := Unchanged;
          end case;
          if some_trace then
            Put_Line
              ("IFD tag 0112: Orientation set to: " &
               image.display_orientation'Image);
          end if;
          exit;
        end if;
      end loop;
      --  Skip rest of data
      for i in x .. data_length loop
        Get_Byte (image.buffer, b);
      end loop;
    end if;
  end Read_EXIF;

  --------------------
  -- Image decoding --
  --------------------

  procedure Load (image : in out Image_Descriptor) is
    --
    --  Bit buffer
    --
    buf : U32 := 0;
    bufbits : Natural := 0;

    function Show_Bits (bits : Natural) return Natural is
      newbyte, marker : U8;
    begin
      if bits = 0 then
        return 0;
      end if;
      while bufbits < bits loop
        begin
          Get_Byte (image.buffer, newbyte);
          bufbits := bufbits + 8;
          buf := buf * 256 + U32 (newbyte);
          if newbyte = 16#FF# then
            Get_Byte (image.buffer, marker);
            case marker is
              when 0 =>
                null;
              when 16#D8# =>  --  SOI here ?
                null;
                --  2015-04-26: occured in one (of many) picture
                --  taken by an Olympus VG120,D705. See test/img/bcase_1.jpg
              when 16#D9# =>  --  EOI here ?
                null;  --  !! signal end
              when 16#D0# .. 16#D7# =>
                bufbits := bufbits + 8;
                buf := buf * 256 + U32 (marker);
              when others =>
                raise error_in_image_data with
                  "JPEG: Invalid code (bit buffer): " & marker'Image;
            end case;
          end if;
        exception
          when Ada.IO_Exceptions.End_Error =>
            newbyte := 16#FF#;
            bufbits := bufbits + 8;
            buf := buf * 256 + U32 (newbyte);
        end;
      end loop;
      return
        Natural
          (Shift_Right (buf, bufbits - bits)
           and
           (Shift_Left (1, bits) - 1));
    end Show_Bits;

    procedure Skip_Bits (bits : Natural) is
    pragma Inline (Skip_Bits);
      dummy : Integer;
      pragma Unreferenced (dummy);
    begin
      if bufbits < bits then
        dummy := Show_Bits (bits);
      end if;
      bufbits := bufbits - bits;
    end Skip_Bits;

    function Get_Bits (bits : Natural) return Integer is
    pragma Inline (Get_Bits);
      res : constant Integer := Show_Bits (bits);
    begin
      Skip_Bits (bits);
      return res;
    end Get_Bits;

    --

    type Info_per_component_B is record
      ht_idx_AC : Natural;
      ht_idx_DC : Natural;
      width, height, stride : Natural;
      dc_predictor : Integer := 0;
    end record;

    info_A : Component_Info_A renames image.JPEG_stuff.info;
    info_B : array (Component) of Info_per_component_B;

    procedure Get_VLC
      (vlc       : in     VLC_table;
       code      :    out U8;
       value_ret :    out Integer)
    is
      --------------------------------------------------
      --  Step 1 happens here: Huffman decompression  --
      --------------------------------------------------
      value : Integer := Show_Bits (16);
      bits : Natural := Natural (vlc (value).bits);
    begin
      if bits = 0 then
        raise error_in_image_data with "JPEG: VLC table: bits = 0";
      end if;
      Skip_Bits (bits);
      value := Integer (vlc (value).code);
      code := U8 (value);
      bits := Natural (U32 (value) and 15);
      value_ret := 0;
      if bits /= 0 then
        value := Get_Bits (bits);
        if value < Integer (Shift_Left (U32'(1), bits - 1)) then
          value := value + 1 - Integer (Shift_Left (U32'(1), bits));
        end if;
        value_ret := value;
      end if;
    end Get_VLC;

    function Clip (x : Integer) return Integer is
    pragma Inline (Clip);
    begin
      if x < 0 then
        return 0;
      elsif x > 255 then
        return 255;
      else
        return x;
      end if;
    end Clip;

    type Block_8x8 is array (0 .. 63) of Integer;

    --  Ordering within a 8x8 block, in zig-zag
    zig_zag : constant Block_8x8 :=
      (0,  1,  8, 16,  9,  2,  3, 10, 17, 24, 32, 25, 18,
      11,  4,  5, 12, 19, 26, 33, 40, 48, 41, 34, 27, 20,
      13,  6,  7, 14, 21, 28, 35, 42, 49, 56, 57, 50, 43,
      36, 29, 22, 15, 23, 30, 37, 44, 51, 58, 59, 52, 45,
      38, 31, 39, 46, 53, 60, 61, 54, 47, 55, 62, 63);

    procedure Decode_8x8_Block (c : Component; block : in out Block_8x8) is
      value, coef : Integer;
      code : U8;
      qt_local : JPEG_Defs.QT renames image.JPEG_stuff.qt_list (info_A (c).qt_assoc);
      --
      W1 : constant := 2841;
      W2 : constant := 2676;
      W3 : constant := 2408;
      W5 : constant := 1609;
      W6 : constant := 1108;
      W7 : constant :=  565;
      --
      procedure Row_IDCT (start : Integer) is
      pragma Inline (Row_IDCT);
        x0, x1, x2, x3, x4, x5, x6, x7, x8, val : Integer;
      begin
        x1 := block (start + 4) * 2**11;
        x2 := block (start + 6);
        x3 := block (start + 2);
        x4 := block (start + 1);
        x5 := block (start + 7);
        x6 := block (start + 5);
        x7 := block (start + 3);
        if x1 = 0 and x2 = 0 and x3 = 0 and x4 = 0 and x5 = 0 and x6 = 0 and x7 = 0 then
          val := block (start + 0) * 8;
          block (start + 0 .. start + 7) := (others => val);
        else
          x0 := (block (start + 0) * 2**11) + 128;
          x8 := W7 * (x4 + x5);
          x4 := x8 + (W1 - W7) * x4;
          x5 := x8 - (W1 + W7) * x5;
          x8 := W3 * (x6 + x7);
          x6 := x8 - (W3 - W5) * x6;
          x7 := x8 - (W3 + W5) * x7;
          x8 := x0 + x1;
          x0 := x0 - x1;
          x1 := W6 * (x3 + x2);
          x2 := x1 - (W2 + W6) * x2;
          x3 := x1 + (W2 - W6) * x3;
          x1 := x4 + x6;
          x4 := x4 - x6;
          x6 := x5 + x7;
          x5 := x5 - x7;
          x7 := x8 + x3;
          x8 := x8 - x3;
          x3 := x0 + x2;
          x0 := x0 - x2;
          x2 := (181 * (x4 + x5) + 128) / 256;
          x4 := (181 * (x4 - x5) + 128) / 256;
          block (start + 0) := (x7 + x1) / 256;
          block (start + 1) := (x3 + x2) / 256;
          block (start + 2) := (x0 + x4) / 256;
          block (start + 3) := (x8 + x6) / 256;
          block (start + 4) := (x8 - x6) / 256;
          block (start + 5) := (x0 - x4) / 256;
          block (start + 6) := (x3 - x2) / 256;
          block (start + 7) := (x7 - x1) / 256;
        end if;
      end Row_IDCT;

      procedure Col_IDCT (start : Integer) is
      pragma Inline (Col_IDCT);
        x0, x1, x2, x3, x4, x5, x6, x7, x8, val : Integer;
      begin
        x1 := block (start + 8 * 4) * 256;
        x2 := block (start + 8 * 6);
        x3 := block (start + 8 * 2);
        x4 := block (start + 8 * 1);
        x5 := block (start + 8 * 7);
        x6 := block (start + 8 * 5);
        x7 := block (start + 8 * 3);
        if x1 = 0 and x2 = 0 and x3 = 0 and x4 = 0 and x5 = 0 and x6 = 0 and x7 = 0 then
          val := Clip (((block (start) + 32) / 2**6) + 128);
          for row in reverse 0 .. 7 loop
            block (start + row * 8) := val;
          end loop;
        else
          x0 := (block (start) * 256) + 8192;
          x8 := W7 * (x4 + x5) + 4;
          x4 := (x8 + (W1 - W7) * x4) / 8;
          x5 := (x8 - (W1 + W7) * x5) / 8;
          x8 := W3 * (x6 + x7) + 4;
          x6 := (x8 - (W3 - W5) * x6) / 8;
          x7 := (x8 - (W3 + W5) * x7) / 8;
          x8 := x0 + x1;
          x0 := x0 - x1;
          x1 := W6 * (x3 + x2) + 4;
          x2 := (x1 - (W2 + W6) * x2) / 8;
          x3 := (x1 + (W2 - W6) * x3) / 8;
          x1 := x4 + x6;
          x4 := x4 - x6;
          x6 := x5 + x7;
          x5 := x5 - x7;
          x7 := x8 + x3;
          x8 := x8 - x3;
          x3 := x0 + x2;
          x0 := x0 - x2;
          x2 := (181 * (x4 + x5) + 128) / 256;
          x4 := (181 * (x4 - x5) + 128) / 256;
          block (start + 8 * 0) := Clip (((x7 + x1) / 2**14) + 128);
          block (start + 8 * 1) := Clip (((x3 + x2) / 2**14) + 128);
          block (start + 8 * 2) := Clip (((x0 + x4) / 2**14) + 128);
          block (start + 8 * 3) := Clip (((x8 + x6) / 2**14) + 128);
          block (start + 8 * 4) := Clip (((x8 - x6) / 2**14) + 128);
          block (start + 8 * 5) := Clip (((x0 - x4) / 2**14) + 128);
          block (start + 8 * 6) := Clip (((x3 - x2) / 2**14) + 128);
          block (start + 8 * 7) := Clip (((x7 - x1) / 2**14) + 128);
        end if;
      end Col_IDCT;

    begin  --  Decode_8x8_Block
      -------------------------------------------------
      --  Step 2 happens here: Inverse quantization  --
      -------------------------------------------------
      --  DC value:
      Get_VLC (image.JPEG_stuff.vlc_defs (DC, info_B (c).ht_idx_DC).all, code, value);
      --  First value in block (0: top left) uses a predictor.
      info_B (c).dc_predictor := info_B (c).dc_predictor + value;
      block := (0 => info_B (c).dc_predictor * qt_local (0), others => 0);
      coef := 0;
      loop
        --  AC value:
        Get_VLC (image.JPEG_stuff.vlc_defs (AC, info_B (c).ht_idx_AC).all, code, value);
        exit when code = 0;  --  EOB
        if (code and 16#0F#) = 0 and code /= 16#F0# then
          raise error_in_image_data with "JPEG: error in VLC AC code for de-quantization";
        end if;
        coef := coef + Integer (Shift_Right (code, 4)) + 1;
        if coef > 63 then
          raise error_in_image_data with "JPEG: coefficient for de-quantization is > 63";
        end if;
        --  Undo the zigzag scan and apply de-quantization:
        block (zig_zag (coef)) := value * qt_local (coef);
        exit when coef = 63;
      end loop;
      -----------------------------------------------------
      --  Step 3 happens here: Inverse cosine transform  --
      -----------------------------------------------------
      for row in 0 .. 7 loop
        Row_IDCT (row * 8);
      end loop;
      for column in 0 .. 7 loop
        Col_IDCT (column);
      end loop;
    end Decode_8x8_Block;

    procedure Out_Pixel_8 (br, bg, bb : U8) is
    pragma Inline (Out_Pixel_8);
      function Times_257 (x : Primary_Color_Range) return Primary_Color_Range is
      pragma Inline (Times_257);
      begin
        --  Returns x if type Primary_Color_Range is mod 2**8.
        return 16 * (16 * x) + x;
        --  ^ This is 257 * x, that is 16#0101# * x
        --  All literal numbers are 8-bit -> no OA warning at instanciation.
      end Times_257;
      full_opaque : constant Primary_Color_Range := Primary_Color_Range'Last;
    begin
      case Primary_Color_Range'Modulus is
        when 256 =>
          Put_Pixel
            (Primary_Color_Range (br),
             Primary_Color_Range (bg),
             Primary_Color_Range (bb),
             full_opaque);
        when 65_536 =>
          Put_Pixel
            (Times_257 (Primary_Color_Range (br)),
             Times_257 (Primary_Color_Range (bg)),
             Times_257 (Primary_Color_Range (bb)),
             full_opaque);
             --  Times_257 makes max intensity FF go to FFFF
        when others =>
          raise invalid_primary_color_range
            with "JPEG: color range not supported";
      end case;
    end Out_Pixel_8;

    ssxmax : constant Natural := image.JPEG_stuff.max_samples_hor;
    ssymax : constant Natural := image.JPEG_stuff.max_samples_ver;

    type Macro_8x8_Block is array
      (Component range <>,  --  component
       Positive range <>,   --  x sample range
       Positive range <>)   --  y sample range
    of Block_8x8;

    procedure Upsampling_and_Output (m : Macro_8x8_Block; x0, y0 : Natural) is

      flat : array (Component, 0 .. 8 * ssxmax - 1, 0 .. 8 * ssymax - 1) of Integer;

      generic
        color_space : Supported_color_space;
      procedure Color_Transformation_and_Output;
      --
      procedure Color_Transformation_and_Output is
        y_val, cb_val, cr_val, c_val, m_val, w_val : Integer;
        y_val_8 : U8;
      begin
        for ymb in flat'Range (3) loop
          exit when y0 + ymb >= Integer (image.height);
          Set_X_Y (x0, Integer (image.height) - 1 - (y0 + ymb));
          for xmb in flat'Range (2) loop
            exit when x0 + xmb >= Integer (image.width);
            case color_space is
              when YCbCr =>
                y_val  := flat (Y,  xmb, ymb) * 256;
                cb_val := flat (Cb, xmb, ymb) - 128;
                cr_val := flat (Cr, xmb, ymb) - 128;
                Out_Pixel_8
                  (br => U8 (Clip ((y_val                + 359 * cr_val + 128) / 256)),
                   bg => U8 (Clip ((y_val -  88 * cb_val - 183 * cr_val + 128) / 256)),
                   bb => U8 (Clip ((y_val + 454 * cb_val                + 128) / 256)));
              when Y_Grey =>
                y_val_8 := U8 (flat (Y,  xmb, ymb));
                Out_Pixel_8 (y_val_8, y_val_8, y_val_8);
              when CMYK =>
                --  !! find a working conversion formula.
                --     perhaps it is more complicated (APP_2
                --     color profile must be used ?)
                c_val := flat (Y,  xmb, ymb);
                m_val := flat (Cb, xmb, ymb);
                y_val := flat (Cr, xmb, ymb);
                w_val := flat (I,  xmb, ymb) - 255;
                Out_Pixel_8
                  (br => U8 (255 - Clip (c_val + w_val)),
                   bg => U8 (255 - Clip (m_val + w_val)),
                   bb => U8 (255 - Clip (y_val + w_val)));
            end case;
          end loop;
        end loop;
      end Color_Transformation_and_Output;
      --
      procedure Ct_YCbCr  is new Color_Transformation_and_Output (YCbCr);
      procedure Ct_Y_Grey is new Color_Transformation_and_Output (Y_Grey);
      procedure Ct_CMYK   is new Color_Transformation_and_Output (CMYK);

      blk_idx : Integer;
      upsx, upsy : Natural;
    begin
      ---------------------------------------
      --  Step 4 happens here: Upsampling  --
      ---------------------------------------
      for c in Component loop
        if image.JPEG_stuff.compo_set (c) then
          upsx := info_A (c).up_factor_x;
          upsy := info_A (c).up_factor_y;
          for x in reverse 1 .. info_A (c).samples_hor loop
            for y in reverse 1 .. info_A (c).samples_ver loop
              --  We are at the 8x8 block level
              blk_idx := 63;
              for y8 in reverse 0 .. 7 loop
                for x8 in reverse 0 .. 7 loop
                  declare
                    val : constant Integer := m (c, x, y)(blk_idx);
                    big_pixel_x : constant Natural := upsx * (x8 + 8 * (x - 1));
                    big_pixel_y : constant Natural := upsy * (y8 + 8 * (y - 1));
                  begin
                    --  Repeat pixels for component c, sample (x,y),
                    --  position (x8,y8).
                    for rx in reverse 0 .. upsx - 1 loop
                      for ry in reverse 0 .. upsy - 1 loop
                        flat (c, rx + big_pixel_x, ry + big_pixel_y) := val;
                      end loop;
                    end loop;
                  end;
                  blk_idx := blk_idx - 1;
                end loop;
              end loop;
            end loop;
          end loop;
        end if;
      end loop;
      -----------------------------------------------------------------
      --  Step 5 and 6 happen here: Color transformation and output  --
      -----------------------------------------------------------------
      case image.JPEG_stuff.color_space is
        when YCbCr =>
          Ct_YCbCr;
        when Y_Grey =>
          Ct_Y_Grey;
        when CMYK =>
          Ct_CMYK;
      end case;
    end Upsampling_and_Output;

    mcu_count, mcu_count_h, mcu_count_v : Natural;

    --  RST (restart) markers:
    rst_count : Natural;
    next_rst : U16;

    procedure Check_Restart (do_reset_predictors : Boolean) is
      w : U16;
    begin
      if image.JPEG_stuff.restart_interval > 0 then
        rst_count := rst_count - 1;
        if rst_count = 0 then
          --  Here begins the restart.
          bufbits := Natural (U32 (bufbits) and 16#F8#);  --  Byte alignment
          --  Now the restart marker.
          w := U16 (Get_Bits (16));
          if some_trace then
            Put_Line
              ("  Restart #" & next_rst'Image &
               "  Code " & w'Image &
               " after" & image.JPEG_stuff.restart_interval'Image &
               " macro blocks");
          end if;
          if w not in 16#FFD0# .. 16#FFD7# or (w and 7) /= next_rst then
            raise error_in_image_data with
              "JPEG: expected RST (restart) marker Nb " & next_rst'Image;
          end if;
          next_rst := (next_rst + 1) and 7;
          rst_count := image.JPEG_stuff.restart_interval;
          if do_reset_predictors then
            --  Block-to-block predictor variables are reset.
            for c in Component loop
              info_B (c).dc_predictor := 0;
            end loop;
          end if;
        end if;
      end if;
    end Check_Restart;

    procedure Baseline_DCT_Decoding_Scan is
      mb : Macro_8x8_Block (Component, 1 .. ssxmax, 1 .. ssymax);
      x0, y0 : Integer := 0;
      mb_x, mb_y : Natural := 0;
    begin
      rst_count := image.JPEG_stuff.restart_interval;
      next_rst := 0;

      macro_blocks_loop :
      loop
        components_loop :
        for c in Component loop
          if image.JPEG_stuff.compo_set (c) then
            samples_y_loop :
            for sby in 1 .. info_A (c).samples_ver loop
              samples_x_loop :
              for sbx in 1 .. info_A (c).samples_hor loop
                --  Steps 1, 2, 3 happen here:
                Decode_8x8_Block (c, mb (c, sbx, sby));
              end loop samples_x_loop;
            end loop samples_y_loop;
          end if;
        end loop components_loop;
        --  All components of the current macro-block are now decoded.
        --  Steps 4, 5, 6 happen here:
        Upsampling_and_Output (mb, x0, y0);
        --
        mb_x := mb_x + 1;
        x0 := x0 + ssxmax * 8;
        if mb_x >= mcu_count_h then
          mb_x := 0;
          x0 := 0;
          mb_y := mb_y + 1;
          y0 := y0 + ssymax * 8;
          Feedback ((100 * mb_y) / mcu_count_v);
          exit macro_blocks_loop when mb_y >= mcu_count_v;
        end if;
        Check_Restart (True);
      end loop macro_blocks_loop;
    end Baseline_DCT_Decoding_Scan;

    image_array : Progressive_Bitmap_Access := null;

    components_amount : U8;

    procedure Progressive_DCT_Decoding_Scan
      (spectral_selection_start,
       spectral_selection_end,
       bit_position_high,
       bit_position_low : Integer)

    --  NB: this procedure is called multiple times for
    --      a single image encoded as progressive JPEG.
    is
      refining : Boolean;
      x, y : Natural_32;
      repeat : Natural;
      compo_idx : Natural;
      dc_value : Integer;
      code : U8;

      procedure Progressive_DC_Scan is
      begin
        if not refining then
          for c in Component loop
            info_B (c).dc_predictor := 0;
          end loop;
        end if;
        for current_mcu in 0 .. mcu_count - 1 loop
          compo_idx := 1;  --  Compact index (without holes).
          --  Loop through all color components
          for c in Component loop
            if image.JPEG_stuff.compo_set (c) then
              declare
                info : JPEG_Defs.Info_per_Component_A
                  renames image.JPEG_stuff.info (c);
                block_x, block_y, delta_x, delta_y : Natural_32;
                new_bit : Integer;
              begin
                --  (x, y) coordinates, on the image, of current MCU's corner
                x := Natural_32 ((current_mcu mod mcu_count_h) * info.shape_x);
                y := Natural_32 ((current_mcu  /  mcu_count_h) * info.shape_y);
                if components_amount > 1 then
                  repeat := info.repeat;
                else
                  repeat := 1;
                end if;
                --  Blocks of 8 x 8 pixels for the color component
                for block_count in 0 .. repeat - 1 loop
                  --  Coordinates of the block on the current MCU
                  block_x := Natural_32 (block_count mod info.samples_hor);
                  block_y := Natural_32 (block_count  /  info.samples_hor);
                  delta_x := 8 * block_x;
                  delta_y := 8 * block_y;
                  if refining then
                    --  Refining scan for the DC values
                    new_bit := Get_Bits (1);
                    image_array (x + delta_x, y + delta_y, compo_idx) :=
                      image_array (x + delta_x, y + delta_y, compo_idx) or
                        Shift_Left (U8 (new_bit), bit_position_low);
                  else
                    --  Decode DC value
                    Get_VLC
                      (image.JPEG_stuff.vlc_defs (DC, info_B (c).ht_idx_DC).all,
                       code,
                       dc_value);
                    dc_value := dc_value + info_B (c).dc_predictor;
                    info_B (c).dc_predictor := dc_value;
                    --  Store the partial DC value on the image array
                    image_array (x + delta_x, y + delta_y, compo_idx) :=
                      Shift_Left (U8 (dc_value), bit_position_low);
                  end if;
                end loop;
              end;
              Check_Restart (not refining);
              compo_idx := compo_idx + 1;
            end if;
          end loop;
        end loop;
      end Progressive_DC_Scan;

      procedure Progressive_AC_Scan is
        -- LINE 1075 of jpeg.py
      begin
        if components_amount > 1 then
          raise error_in_image_data with
            "JPEG, progressive: an AC progressive scan can only" &
            " have a single color component";
        end if;
        --  !!  TBD
        raise unsupported_image_subformat
          with "JPEG: progressive format not yet functional";
      end Progressive_AC_Scan;

      kind : AC_DC;

    begin
      rst_count := image.JPEG_stuff.restart_interval;
      next_rst := 0;

      if spectral_selection_start = 0 and then spectral_selection_end = 0 then
        kind := DC;
      elsif spectral_selection_start > 0
        and then spectral_selection_end >= spectral_selection_start
      then
        kind := AC;
      else
        raise error_in_image_data with
          "JPEG, progressive: invalid spectral selection values";
      end if;

      if bit_position_high = 0 then
        refining := False;
      elsif bit_position_high - bit_position_low = 1 then
        refining := True;
      else
        raise error_in_image_data with
          "JPEG, progressive: precision improvement has to be by one bit.";
          --  G.1.1.1.2 Successive approximation control:
          --    "Each scan which follows the first scan for a given band
          --     progressively improves the precision of the coefficients
          --     by one bit, until full precision is reached."
      end if;

      case kind is
        when DC => Progressive_DC_Scan;  --  First scan
        when AC => Progressive_AC_Scan;  --  Further scans
      end case;
    end Progressive_DCT_Decoding_Scan;

    --  Start Of Scan (and image data which follow)
    --
    procedure Read_SOS is
      b, id_base : U8;
      compo : Component := Component'First;
      mcu_width, mcu_height : Natural;
      --  Parameters for progressive decoding :
      start_spectral_selection,
      end_spectral_selection,
      successive_approximation : U8;
    begin
      Get_Byte (image.buffer, components_amount);
      if some_trace then
        Put_Line
          ("  Start of Scan (SOS), with" & components_amount'Image & " components");
      end if;
      if image.subformat_id /= Natural (components_amount) then
        raise error_in_image_data with "JPEG: components mismatch in Scan segment";
      end if;
      id_base := 1;
      for i in 1 .. components_amount loop
        Get_Byte (image.buffer, b);
        if b = 0 then
          --  Workaround for bugged encoder (see above)
          id_base := 0;
        end if;
        if b - id_base > Component'Pos (Component'Last) then
          raise error_in_image_data with "JPEG: Scan: invalid ID:" & b'Image;
        end if;
        compo := Component'Val (b - id_base);
        if not image.JPEG_stuff.compo_set (compo) then
          raise error_in_image_data with
            "JPEG: component " & compo'Image &
            " has not been defined in the header (SOF) segment";
        end if;
        --  Huffman table selection
        Get_Byte (image.buffer, b);
        info_B (compo).ht_idx_AC := Natural (b mod 16);
        info_B (compo).ht_idx_DC := Natural (b  /  16);
      end loop;
      --  Parameters for progressive display format (SOF_2)
      Get_Byte (image.buffer, start_spectral_selection);
      Get_Byte (image.buffer, end_spectral_selection);
      Get_Byte (image.buffer, successive_approximation);
      --
      --  End of SOS segment, image data follow.
      --
      mcu_width := ssxmax * 8;   --  Pixels in a row of a MCU (Minimum Coded Unit) block
      mcu_height := ssymax * 8;  --  Pixels in a column of a MCU block
      mcu_count_h := (Integer (image.width)  + mcu_width - 1) / mcu_width;
      mcu_count_v := (Integer (image.height) + mcu_height - 1) / mcu_height;
      mcu_count := mcu_count_h * mcu_count_v;

      if some_trace then
        New_Line;
        Put_Line ("    mcu_width   = " & mcu_width'Image);
        Put_Line ("    mcu_height  = " & mcu_height'Image);
        Put_Line ("    mcu_count_h = " & mcu_count_h'Image);
        Put_Line ("    mcu_count_v = " & mcu_count_v'Image);
        if image.progressive then
          New_Line;
          Put_Line ("    Progressive image parameters:");
          Put_Line ("      start_spectral_selection = " & start_spectral_selection'Image);
          Put_Line ("      end_spectral_selection   = " & end_spectral_selection'Image);
          Put_Line ("      successive_approximation = " & successive_approximation'Image);
        end if;
      end if;

      for c in Component loop
        if image.JPEG_stuff.compo_set (c) then
          info_B (c).width  := (Integer (image.width)  * info_A (c).samples_hor + ssxmax - 1) / ssxmax;
          info_B (c).height := (Integer (image.height) * info_A (c).samples_ver + ssymax - 1) / ssymax;
          info_B (c).stride := (mcu_count_h * mcu_width * info_A (c).samples_hor) / ssxmax;
          if some_trace then
            New_Line;
            Put_Line ("    Details for component " & c'Image);
            Put_Line ("      samples in x " & info_A (c).samples_hor'Image);
            Put_Line ("      samples in y " & info_A (c).samples_ver'Image);
            Put_Line ("      width  " & info_B (c).width'Image);
            Put_Line ("      height " & info_B (c).height'Image);
            Put_Line ("      stride " & info_B (c).stride'Image);
            Put_Line
              ("      AC/DC table index: " &
               info_B (compo).ht_idx_AC'Image & ", " &
               info_B (compo).ht_idx_DC'Image);
          end if;
          if (info_B (c).width < 3 and info_A (c).samples_hor /= ssxmax) or
             (info_B (c).height < 3 and info_A (c).samples_ver /= ssymax)
          then
            raise error_in_image_data with
              "JPEG: component " & c'Image & ": sample dimension mismatch";
          end if;
        end if;
      end loop;

      if image.progressive then
        Progressive_DCT_Decoding_Scan
          (Integer (start_spectral_selection),
           Integer (end_spectral_selection),
           Integer (successive_approximation  /  16),
           Integer (successive_approximation mod 16));
      else
        Baseline_DCT_Decoding_Scan;
      end if;

    end Read_SOS;

    sh : Segment_Head;
    b : U8;

    procedure Dispose is
      new Ada.Unchecked_Deallocation
        (Progressive_Bitmap, Progressive_Bitmap_Access);

  begin  --  Load
    if image.progressive then
      image_array :=
        new Progressive_Bitmap
          (0 .. image.width  - 1,
           0 .. image.height - 1,
           1 .. image.subformat_id);
    end if;
    loop
      if full_trace then
        Put_Line ("Reading Segment Marker");
      end if;
      Read (image, sh);
      case sh.kind is
        when DQT =>
          --  Quantization Table
          Read_DQT (image, Natural (sh.length));
        when DHT =>
          --  Huffman Table
          Read_DHT (image, Natural (sh.length));
        when DRI =>
          --  Restart Interval
          Read_DRI (image);
        when EOI =>
          --  End Of Input
          if some_trace then
            New_Line;
            Put_Line ("EOI marker");
          end if;
          exit;
        when SOS =>
          --  Start Of Scan
          Read_SOS;
          exit when no_trace and not image.progressive;
          --  ^  When there is a trace we are interested in
          --       what appears after the scan.
          --     When the image is progressive we need to
          --       continue because there are multiple scans.
          if full_trace then
            New_Line;
            Put_Line ("SOS marker done");
          end if;
        when COM =>
          --  B.2.4.5 Comment
          if some_trace then
            New_Line;
            Put_Line ("JPEG Comment:  --------");
            for i in 1 .. sh.length loop
              Get_Byte (image.buffer, b);
              Put (Character'Val (b));
            end loop;
            New_Line;
          end if;
        when others =>
          --  Any other segment: skip segment data.
          for i in 1 .. sh.length loop
            Get_Byte (image.buffer, b);
          end loop;
      end case;
    end loop;
    Dispose (image_array);
  end Load;

end GID.Decoding_JPG;
