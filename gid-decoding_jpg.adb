--  GID's JPEG baseline decoder is largely inspired
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

  --  B.1.1.3 Marker assignments

  COM_code   : constant := 16#FE#;
  DHT_code   : constant := 16#C4#;
  DRI_code   : constant := 16#DD#;
  EOI_code   : constant := 16#D9#;
  --
  RST_0_code : constant := 16#D0#;
  RST_1_code : constant := 16#D1#;
  RST_2_code : constant := 16#D2#;
  RST_3_code : constant := 16#D3#;
  RST_4_code : constant := 16#D4#;
  RST_5_code : constant := 16#D5#;
  RST_6_code : constant := 16#D6#;
  RST_7_code : constant := 16#D7#;
  --
  SOS_code   : constant := 16#DA#;

  marker_id : constant array (JPEG_Marker) of U8 :=
   (SOI => 16#D8#,
    --
    SOF_0  => 16#C0#, SOF_1  => 16#C1#, SOF_2  => 16#C2#, SOF_3  => 16#C3#,
    SOF_5  => 16#C5#, SOF_6  => 16#C6#, SOF_7  => 16#C7#, SOF_8  => 16#C8#,
    SOF_9  => 16#C9#, SOF_10 => 16#CA#, SOF_11 => 16#CB#, SOF_13 => 16#CD#,
    SOF_14 => 16#CE#, SOF_15 => 16#CF#,
    --
    DAC => 16#CC#,
    DHT => DHT_code,
    DQT => 16#DB#,
    DRI => DRI_code,
    --
    RST_0 => RST_0_code,
    RST_1 => RST_1_code,
    RST_2 => RST_2_code,
    RST_3 => RST_3_code,
    RST_4 => RST_4_code,
    RST_5 => RST_5_code,
    RST_6 => RST_6_code,
    RST_7 => RST_7_code,
    --
    APP_0  => 16#E0#, APP_1  => 16#E1#, APP_2  => 16#E2#, APP_3  => 16#E3#,
    APP_4  => 16#E4#, APP_5  => 16#E5#, APP_6  => 16#E6#, APP_7  => 16#E7#,
    APP_8  => 16#E8#, APP_9  => 16#E9#, APP_10 => 16#EA#, APP_11 => 16#EB#,
    APP_12 => 16#EC#, APP_13 => 16#ED#, APP_14 => 16#EE#,
    --
    COM => COM_code,
    SOS => SOS_code,
    EOI => EOI_code);

  function Marker_Image (m : U8) return String is
    package BIO is new Modular_IO (U8);
    hexa : String (1 .. 6);
  begin
    BIO.Put (hexa, m, 16);
    for jm in JPEG_Marker loop
      if marker_id (jm) = m then
        return hexa & ", marker: " & jm'Image;
      end if;
    end loop;
    return hexa;
  end Marker_Image;

  procedure Read
    (image           : in out Image_Descriptor;
     known_marker    : in     Boolean;
     buffered_marker : in     U8;
     head            :    out Segment_Head)
  is
    b : U8;
  begin
    if known_marker then
      if full_trace then
        Put_Line ("Segment Marker has been read previously.");
      end if;
      b := buffered_marker;
    else
      Get_Byte (image.buffer, b);
      if b /= 16#FF# then
        raise error_in_image_data
          with "JPEG: expected marker prefix (16#FF#) here";
      end if;
      Get_Byte (image.buffer, b);
      if full_trace then
        Put_Line ("Segment Marker has been just read from stream.");
      end if;
    end if;
    for m in JPEG_Marker loop
      if marker_id (m) = b then
        head.kind := m;
        case m is
          when EOI =>
            --  No header following this marker (there are perhaps others).
            head.length := 0;
          when others =>
            Big_Endian (image.buffer, head.length);
            --  We consider length of contents, without the FFxx marker.
            head.length := head.length - 2;
        end case;
        if some_trace then
          Put_Line
            ("Segment [" & head.kind'Image & "], length:" & head.length'Image);
        end if;
        return;
      end if;
    end loop;
    raise error_in_image_data
      with "JPEG: unknown marker here: 16#FF#, then" & Marker_Image (b);
  end Read;

  procedure Skip_Segment_Data
    (image : in out Image_Descriptor;
     head  : in     Segment_Head)
  is
    dummy : U8;
  begin
    if full_trace then
      Put_Line ("Skipping segment: " & head.kind'Image);
    end if;
    for i in 1 .. head.length loop
      Get_Byte (image.buffer, dummy);
    end loop;
 end Skip_Segment_Data;

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
        if full_trace then
          Put_Line ("SOF: Off-by one error in image data: component Id");
        end if;
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
        "JPEG: only YCbCr, Y_Grey and CMYK color spaces are currently defined";
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
    current_count, spread, remain_vlc : Integer_M32;
  begin
    Multi_DHT_Tables :
    while remaining > 0 loop
      --  ^ Test is at the beginning of the loop because
      --    some encoders produce empty DHT segments!
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
      for code_len in counts'Range loop
        spread := spread / 2;
        current_count := counts (code_len);
        if current_count > 0 then
          if remaining < current_count then
            raise error_in_image_data
              with
                "JPEG: DHT data too short [1]: remaining =" & remaining'Image;
          end if;
          remain_vlc := remain_vlc - current_count * spread;
          if remain_vlc < 0 then
            raise error_in_image_data
              with
                "JPEG: DHT data too short [2]: remain_vlc =" &
                remain_vlc'Image;
          end if;
          for i in reverse 1 .. current_count loop
            Get_Byte (image.buffer, b);
            for j in reverse 1 .. spread loop
              image.JPEG_stuff.vlc_defs (kind, ht_idx)(idx) :=
                (bits => U8 (code_len), code => b);
              idx := idx + 1;
            end loop;
          end loop;
          remaining := remaining - current_count;
        end if;
      end loop;
      while remain_vlc > 0 loop
        remain_vlc := remain_vlc - 1;
        image.JPEG_stuff.vlc_defs (kind, ht_idx)(idx).bits := 0;
        idx := idx + 1;
      end loop;
    end loop Multi_DHT_Tables;
  end Read_DHT;

  procedure Read_DQT (image : in out Image_Descriptor; data_length : Natural) is
    remaining : Integer := data_length;  --  Data remaining in segment
    b, q8 : U8; q16 : U16;
    qt_idx : Natural;
    high_prec : Boolean;
  begin
    Multi_DQT_Tables :
    while remaining > 0 loop
      --  ^ Test is at the beginning of the loop because
      --    an encoder could produce an empty DQT segment.
      Get_Byte (image.buffer, b);
      remaining := remaining - 1;
      high_prec := b >= 8;
      qt_idx := Natural (b and 7);
      if some_trace then
        Put_Line ("  Quantization Table (QT) #" & b'Image);
      end if;
      for i in Quantization_Table'Range loop
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
    end loop Multi_DQT_Tables;
  end Read_DQT;

  procedure Read_DRI (image : in out Image_Descriptor) is
    --  B.2.4.4 Restart interval definition syntax
    --  DRI: Define restart interval marker
    ri : U16;
  begin
    Big_Endian (image.buffer, ri);
    if some_trace then
      Put_Line ("  Restart interval (DRI) set to:" & ri'Image);
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
    bit_buffer : U32;
    bit_buffer_length : Natural;
    --  A marker can appear when filling the bit buffer
    --  during a scan's decoding. Actually, at the end of
    --  the scan's data, it *will* appear (usually:
    --  the final EOI, or the next SOS).
    memo_marker : U8 := 0;

    procedure Initialize_Bit_Buffer is
    begin
      bit_buffer := 0;
      bit_buffer_length := 0;
      memo_marker := 0;
    end Initialize_Bit_Buffer;

    function Show_Bits (bits : Natural) return Natural is
      newbyte, possible_marker : U8;
    begin
      if bits = 0 then
        return 0;
      end if;
      while bit_buffer_length < bits loop
        begin
          Get_Byte (image.buffer, newbyte);
          bit_buffer_length := bit_buffer_length + 8;
          bit_buffer := Shift_Left (bit_buffer, 8) + U32 (newbyte);
          if newbyte = 16#FF# then
            Get_Byte (image.buffer, possible_marker);
            if possible_marker = 0 then
              --  Escape code for a normal value 16#FF#.
              --  F.1.2.3 Byte stuffing:
              --    "If a X'00' byte is detected after a X'FF' byte, the
              --     decoder must discard it."
              if full_trace then
                New_Line;
                Put_Line
                  ("Bit buffer: byte stuffing: FF, then 00 -> value FF");
              end if;            else
              --    "If the byte is not zero, a marker has been detected,
              --     and shall be interpreted to the extent needed to
              --     complete the decoding of the scan."
              --  It is the case at least for restart markers (RST).
              if full_trace then
                New_Line;
                Put_Line
                  ("Bit buffer: possible marker found: " &
                   Marker_Image (possible_marker));
              end if;
              bit_buffer_length := bit_buffer_length + 8;
              bit_buffer := Shift_Left (bit_buffer, 8) + U32 (possible_marker);
              --  Many possible markers are naturally
              --  buffered in the bit buffer at the very end of the
              --  scan: EOI, DHT, SOS (next scan), ...
              --  We need not to discard those markers!
              memo_marker := possible_marker;
              --
              case possible_marker is
                when EOI_code =>
                  if full_trace then
                    Put_Line ("Bit buffer: acquired EOI marker");
                  end if;
                when COM_code | DHT_code | DRI_code |
                     RST_0_code .. RST_7_code | SOS_code =>
                  null;
                when others =>
                  raise error_in_image_data with
                    "JPEG: Invalid marker within filling of bit buffer: " &
                    Marker_Image (possible_marker);
              end case;
            end if;
          end if;
        exception
          when Ada.IO_Exceptions.End_Error =>
            newbyte := 16#FF#;
            bit_buffer_length := bit_buffer_length + 8;
            bit_buffer := Shift_Left (bit_buffer, 8) + U32 (newbyte);
        end;
      end loop;
      return
        Natural
          (Shift_Right (bit_buffer, bit_buffer_length - bits)
           and
           (Shift_Left (1, bits) - 1));
    end Show_Bits;

    procedure Skip_Bits (bits : Natural) is
    pragma Inline (Skip_Bits);
      dummy : Integer;
      pragma Unreferenced (dummy);
    begin
      if bit_buffer_length < bits then
        dummy := Show_Bits (bits);
      end if;
      bit_buffer_length := bit_buffer_length - bits;
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

    function Next_Huffval (vlc : VLC_table) return Integer is
      value : constant Integer := Show_Bits (16);
      bits  : constant Natural := Natural (vlc (value).bits);
    begin
      if bits = 0 then
        raise error_in_image_data with "JPEG: Huffman value: bits = 0";
      end if;
      Skip_Bits (bits);
      return Integer (vlc (value).code);
    end Next_Huffval;

    function Bin_Twos_Complement (value, bit_length : Integer) return Integer is
    begin
      if value < Integer (Shift_Left (U32'(1), bit_length - 1)) then
        return value + 1 - Integer (Shift_Left (U32'(1), bit_length));
      else
        return value;
      end if;
    end Bin_Twos_Complement;

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
    --  See: Figure 5 - Preparation of quantized
    --       coefficients for entropy encoding
    --
    zig_zag : constant Block_8x8 :=
      (0,  1,  8, 16,  9,  2,  3, 10,
      17, 24, 32, 25, 18, 11,  4,  5,
      12, 19, 26, 33, 40, 48, 41, 34,
      27, 20, 13,  6,  7, 14, 21, 28,
      35, 42, 49, 56, 57, 50, 43, 36,
      29, 22, 15, 23, 30, 37, 44, 51,
      58, 59, 52, 45, 38, 31, 39, 46,
      53, 60, 61, 54, 47, 55, 62, 63);

    procedure Decode_8x8_Block (c : Component; block : in out Block_8x8) is
      value, coef : Integer;
      code : U8;
      qt_local : JPEG_Defs.Quantization_Table
        renames image.JPEG_stuff.qt_list (info_A (c).qt_assoc);
    begin
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
        --  Undo the zig-zag scan and apply de-quantization:
        block (zig_zag (coef)) := value * qt_local (coef);
        exit when coef = 63;
      end loop;
    end Decode_8x8_Block;

    procedure Inverse_DCT_8x8_Block (block : in out Block_8x8) is
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

    begin
      -----------------------------------------------------
      --  Step 3 happens here: Inverse cosine transform  --
      -----------------------------------------------------
      for row in 0 .. 7 loop
        Row_IDCT (row * 8);
      end loop;
      for column in 0 .. 7 loop
        Col_IDCT (column);
      end loop;
    end Inverse_DCT_8x8_Block;

    procedure Out_Pixel_8 (br, bg, bb : U8) is
    pragma Inline (Out_Pixel_8);
      function Times_257 (x : Primary_Color_Range) return Primary_Color_Range is
      pragma Inline (Times_257);
      begin
        --  Returns x if type Primary_Color_Range is mod 2**8.
        return 16 * (16 * x) + x;
        --  ^ This is 257 * x, that is 16#0101# * x
        --  All literal numbers are 8-bit ->
        --  no OA warning at instantiation.
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

    sample_shape_max_x : constant Natural := 8 * ssxmax;
    sample_shape_max_y : constant Natural := 8 * ssymax;

    scan_compo_set : Compo_Set_Type;

    type Macro_8x8_Block is array
      (Component range <>,  --  component
       Positive range <>,   --  x sample range
       Positive range <>)   --  y sample range
    of Block_8x8;

    procedure Upsampling_and_Output
      (macro_block : Macro_8x8_Block;
       x0, y0      : Natural_32)
    is

      flat :
        array
          (Component,
           0 .. sample_shape_max_x - 1,
           0 .. sample_shape_max_y - 1) of Integer;

      generic
        color_space : Supported_color_space;
      procedure Color_Transformation_and_Output;
      --
      procedure Color_Transformation_and_Output is
        y_val, cb_val, cr_val, c_val, m_val, w_val : Integer;
        y_val_8 : U8;
      begin
        for ymb in flat'Range (3) loop
          exit when y0 + Integer_32 (ymb) >= image.height;
          Set_X_Y (Integer (x0), Integer (image.height - 1 - (y0 + Integer_32 (ymb))));
          for xmb in flat'Range (2) loop
            exit when x0 + Integer_32 (xmb) >= image.width;
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
                    val : constant Integer := macro_block (c, x, y)(blk_idx);
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
    mcu_count_image_h, mcu_count_image_v : Natural := 0;

    --  RST (restart) markers:
    rst_count : Natural;
    next_rst : U16;

    procedure Check_Restart (do_reset_predictors : Boolean) is

      procedure Restart is
        w, expected : U16;
        package BIO is new Modular_IO (U16);
        hexa_w, hexa_exp : String (1 .. 8);
      begin
        --  Byte alignment:
        bit_buffer_length := Natural (U32 (bit_buffer_length) and 16#F8#);
        --  Now, the restart marker:
        w := U16 (Get_Bits (16));
        BIO.Put (hexa_w, w, 16);
        if some_trace then
          Put_Line
            ("  Restart #" & next_rst'Image &
             "  Code " & hexa_w &
             " after" & image.JPEG_stuff.restart_interval'Image &
             " macro blocks");
        end if;
        expected := 16#FFD0# + next_rst;
        if w /= expected then
          BIO.Put (hexa_exp, expected, 16);
          raise error_in_image_data with
            "JPEG: expected RST (restart) marker Nb" & next_rst'Image &
            "; code found " & hexa_w &
            "; expected " & hexa_exp;
        end if;
        next_rst := (next_rst + 1) and 7;
        rst_count := image.JPEG_stuff.restart_interval;
        if do_reset_predictors then
          --  Block-to-block predictor variables are reset.
          for c in Component loop
            info_B (c).dc_predictor := 0;
          end loop;
        end if;
      end Restart;

    begin
      if image.JPEG_stuff.restart_interval > 0 then
        rst_count := rst_count - 1;
        if rst_count = 0 then
          Restart;
        end if;
      end if;
    end Check_Restart;

    procedure Baseline_DCT_Decoding_Scan is
      mb : Macro_8x8_Block (Component, 1 .. ssxmax, 1 .. ssymax);
      x0, y0 : Integer_32 := 0;
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
                --  Steps 1 and 2 happen here:
                Decode_8x8_Block (c, mb (c, sbx, sby));
                --  Step 3 happens here:
                Inverse_DCT_8x8_Block (mb (c, sbx, sby));
              end loop samples_x_loop;
            end loop samples_y_loop;
          end if;
        end loop components_loop;
        --  All components of the current macro-block are now decoded.
        --  Steps 4, 5, 6 happen here:
        Upsampling_and_Output (mb, x0, y0);
        --
        mb_x := mb_x + 1;
        x0 := x0 + Integer_32 (sample_shape_max_x);
        if mb_x >= mcu_count_image_h then
          mb_x := 0;
          x0 := 0;
          mb_y := mb_y + 1;
          y0 := y0 + Integer_32 (sample_shape_max_y);
          Feedback ((100 * mb_y) / mcu_count_image_v);
          exit macro_blocks_loop when mb_y >= mcu_count_image_v;
        end if;
        Check_Restart (do_reset_predictors => True);
      end loop macro_blocks_loop;
    end Baseline_DCT_Decoding_Scan;

    --  For the Progressive JPEG format, we need to store the entire bitmap
    --  in the selected colour space (usually, YCbCr).

    type Progressive_Bitmap is
      array (Natural_32 range <>, Natural_32 range <>, Positive range <>) of Integer;

    type Progressive_Bitmap_Access is access Progressive_Bitmap;

    image_array : Progressive_Bitmap_Access := null;
    array_width  : Natural_32;
    array_height : Natural_32;

    components_amount : U8;
    compo : Component;

    scan_count : Natural := 0;

    dump_file : File_Type;
    dump_sep  : constant Character := ';';
    max_refine_index_last : Natural := 0;  --  For statistics

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

      procedure Progressive_DC_Scan is
        compo_idx : Natural;
        dc_value : Integer;
        code : U8;
      begin
        if full_trace then
          Put_Line
            (dump_file,
             "DC" & scan_count'Image & dump_sep &
             "refining = "   & refining'Image);
        end if;
        if not refining then
          for c in Component loop
            info_B (c).dc_predictor := 0;
          end loop;
        end if;
        MCU_Loop :
        for current_mcu in 0 .. mcu_count - 1 loop
          if full_trace then
            Put_Line
              (dump_file,
               dump_sep & "current_mcu =" & current_mcu'Image);
          end if;
          compo_idx := 1;  --  Compact index (without holes).
          --  Loop through all color components
          Component_Loop :
          for c in Component loop
            if scan_compo_set (c) then
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
                if full_trace then
                  Put_Line
                    (dump_file,
                     dump_sep & dump_sep &
                     "repeat =" & repeat'Image & dump_sep &
                     "component =" & c'Image);
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
                      image_array (x + delta_x, y + delta_y, compo_idx) +
                        new_bit * (2 ** bit_position_low);
                  else
                    --  Decode DC value
                    Get_VLC
                      (image.JPEG_stuff.vlc_defs (DC, info_B (c).ht_idx_DC).all,
                       code,
                       dc_value);
                    dc_value := dc_value + info_B (c).dc_predictor;
                    info_B (c).dc_predictor := dc_value;
                    --  Store the partial DC value on the image array
                    --  Note that dc_value can be (and is often) negative.
                    image_array (x + delta_x, y + delta_y, compo_idx) :=
                      dc_value * (2 ** bit_position_low);
                  end if;
                  if full_trace then
                    Put_Line
                      (dump_file,
                       dump_sep & dump_sep & dump_sep &
                       Integer_32'Image (x + delta_x) & dump_sep &
                       Integer_32'Image (y + delta_y) & dump_sep &
                       image_array
                         (x + delta_x, y + delta_y, compo_idx)'Image);
                  end if;
                end loop;
              end;
              compo_idx := compo_idx + 1;
            end if;
          end loop Component_Loop;
          if current_mcu < mcu_count - 1 then
            --  B.2.1 High-level syntax
            --  Restart marker: a conditional marker which is placed
            --  *between* entropy-coded segments.
            Check_Restart (do_reset_predictors => not refining);
          end if;
          if scan_count = 1 then
            Feedback ((50 * current_mcu) / mcu_count);
          end if;
        end loop MCU_Loop;
      end Progressive_DC_Scan;

      zag_zig : constant array (0 .. 63, 1 .. 2) of Integer_32 :=
         ((0, 0), (1, 0), (0, 1), (0, 2), (1, 1), (2, 0), (3, 0), (2, 1),
          (1, 2), (0, 3), (0, 4), (1, 3), (2, 2), (3, 1), (4, 0), (5, 0),
          (4, 1), (3, 2), (2, 3), (1, 4), (0, 5), (0, 6), (1, 5), (2, 4),
          (3, 3), (4, 2), (5, 1), (6, 0), (7, 0), (6, 1), (5, 2), (4, 3),
          (3, 4), (2, 5), (1, 6), (0, 7), (1, 7), (2, 6), (3, 5), (4, 4),
          (5, 3), (6, 2), (7, 1), (7, 2), (6, 3), (5, 4), (4, 5), (3, 6),
          (2, 7), (3, 7), (4, 6), (5, 5), (6, 4), (7, 3), (7, 4), (6, 5),
          (5, 6), (4, 7), (5, 7), (6, 6), (7, 5), (7, 6), (6, 7), (7, 7));

      procedure Progressive_AC_Scan is
        --  !! TBD: fix glitches in the AC scan.
        --
        --  Images with Restart markers crash the decoder in the AC scan
        --  phase: for some reason, the RST_0 marker appears too early.
        --  Note that the decoder works fine for images without Restart
        --  markers (which is the case for progressive JPEG images to be
        --  found on social networks).
        --  A way of investigating the issue would be to run
        --  GID and PyJpegDecoder step-by-step on the same data.
        --

        compo_idx : Integer := 0;

        --  AC values that will be refined

        max_refine_index : constant := 2000;
        refine_index_last : Natural := 0;
        refine_point_x, refine_point_y :
          array (1 .. max_refine_index) of Integer_32;

        procedure Append (x, y : Integer_32) is
        begin
          refine_index_last := refine_index_last + 1;
          if refine_index_last > max_refine_index then
            --  This check is unnecessary... unless
            --  the built-in range checks have been disabled.
            raise Constraint_Error
              with
                "Progressive JPEG: refining buffer capacity" &
                max_refine_index'Image & " exceeded";
          end if;
          refine_point_x (refine_index_last) := x;
          refine_point_y (refine_index_last) := y;
        end Append;

        --  Refining procedure
        procedure Refine_AC is

          idx : Integer := 1;
          amount : Integer := refine_index_last;

          procedure Refine_Batch (length : Integer) is
            --  Perform the refinement of the AC values on a progressive scan.
            refine_bits : constant Unsigned_32 := Unsigned_32 (Get_Bits (length));
            mask : Unsigned_32 := 2 ** (length - 1);
            new_bit, mem : Integer;
            x, y : Integer_32;
          begin
            for i in 1 .. length loop
              x := refine_point_x (idx);
              y := refine_point_y (idx);
              if (refine_bits and mask) /= 0 then
                new_bit := 2 ** bit_position_low;
              else
                new_bit := 0;
              end if;
              mask := Shift_Right (mask, 1);
              if full_trace then
                mem := image_array (x, y, compo_idx);
              end if;
              if image_array (x, y, compo_idx) > 0 then
                image_array (x, y, compo_idx) :=
                  image_array (x, y, compo_idx) + new_bit;
              else
                image_array (x, y, compo_idx) :=
                  image_array (x, y, compo_idx) - new_bit;
              end if;
              if full_trace then
                Put_Line
                  (dump_file,
                   dump_sep & dump_sep & dump_sep &
                   Integer_32'Image (x) & dump_sep &
                   Integer_32'Image (y) & dump_sep &
                   mem'Image & dump_sep &
                   image_array
                   (x, y, compo_idx)'Image & dump_sep &
                   "refine_ac" & dump_sep &
                   "new_bit =" & new_bit'Image);
              end if;
              idx := idx + 1;
            end loop;
            amount := amount - length;
          end Refine_Batch;

          batch_size : constant := 16;

        begin
          if full_trace and then refine_index_last > 0 then
            --  Put_Line
            --    (dump_file,
            --     dump_sep & dump_sep &
            --     "Refining list, length:" & refine_index_last'Image);
            max_refine_index_last :=
              Integer'Max (max_refine_index_last, refine_index_last);
          end if;
          while amount > batch_size loop
            Refine_Batch (batch_size);
          end loop;
          Refine_Batch (amount);
          refine_index_last := 0;
        end Refine_AC;

        eob_run, zero_run : Natural := 0;
        index : Integer;
        huffman_value, run_magnitute : Integer;
        ac_bits, ac_bits_length, eob_bits : Integer;
        ac_value, current_value, mem_ac_value : Integer;
        ac_x, ac_y, xr, yr : Integer_32;
        current_mcu : Integer;
      begin
        if components_amount > 1 then
          raise error_in_image_data with
            "JPEG, progressive: an AC progressive scan can only" &
            " have a single color component";
        end if;

        if full_trace then
          Put_Line
            (dump_file,
             "AC" & scan_count'Image & dump_sep &
             "refining = "   & refining'Image & dump_sep &
             "single component = " & compo'Image);
        end if;

        for c in Component loop
          if image.JPEG_stuff.compo_set (c) then
            compo_idx := compo_idx + 1;
            exit when c = compo;  --  THE colour component for that scan.
          end if;
        end loop;

        refine_index_last := 0;

        --  Decode and refine the AC values
        current_mcu := 0;

        MCU_Loop :
        while current_mcu < mcu_count loop
          if full_trace then
            Put_Line
              (dump_file,
               dump_sep & "current_mcu =" & current_mcu'Image);
          end if;

          --  Coordinates of the MCU's corner on the image
          x := Natural_32 ((current_mcu mod mcu_count_h) * 8);
          y := Natural_32 ((current_mcu  /  mcu_count_h) * 8);

          --  Loop through the band
          index := spectral_selection_start;

          Within_Band :
          while index <= spectral_selection_end loop
            --  ^ The element at the end of the band is included

            --  Get the next Huffman value from the encoded data
            huffman_value :=
              Next_Huffval
                (image.JPEG_stuff.vlc_defs (AC, info_B (compo).ht_idx_AC).all);
            run_magnitute  := huffman_value  /  16;
            ac_bits_length := huffman_value mod 16;

            --  Determine the run length
            if huffman_value = 0 then
              --  End of band run of 1
              eob_run := 1;
              exit Within_Band;
            elsif huffman_value = 16#F0# then
              zero_run := 16;
            elsif ac_bits_length = 0 then
              --  End of band run
              eob_bits := Get_Bits (run_magnitute);
              eob_run := (2 ** run_magnitute) + eob_bits;
              exit Within_Band;
            else
              --  Amount of zero values to skip
              zero_run := run_magnitute;
            end if;

            --  Perform the zero run
            if refining then
              while zero_run > 0 loop
                xr := zag_zig (index, 1);
                yr := zag_zig (index, 2);
                current_value := image_array (x + xr, y + yr, compo_idx);

                if current_value = 0 then
                  zero_run := zero_run - 1;
                else
                  Append (x + xr, y + yr);
                end if;
                index := index + 1;
              end loop;
            else
              --  First scan
              index := index + zero_run;
              zero_run := 0;
            end if;

            --  Decode the next AC value
            if ac_bits_length > 0 then
              ac_bits := Get_Bits (ac_bits_length);
              ac_value := Bin_Twos_Complement (ac_bits, ac_bits_length);

              --  Store the AC value on the image array
              --  (the zig-zag scan order is undone to find the
              --   position of the value on the image)
              ac_x := zag_zig (index, 1);
              ac_y := zag_zig (index, 2);

              --  In order to create a new AC value, the decoder needs to
              --  be at a zero value (the index is moved until a zero is
              --  found, other values along the way will be refined)
              if refining then
                while image_array (x + ac_x, y + ac_y, compo_idx) /= 0 loop
                  Append (x + ac_x, y + ac_y);
                  index := index + 1;
                  if index > zag_zig'Last (1) then
                    --  This check is unnecessary... unless
                    --  the built-in range checks have been disabled.
                    raise error_in_image_data with
                      "JPEG, progressive: zig-zag index overflow";
                  end if;
                  ac_x := zag_zig (index, 1);
                  ac_y := zag_zig (index, 2);
                end loop;
              end if;

              if full_trace then
                mem_ac_value := image_array (x + ac_x, y + ac_y, compo_idx);
              end if;

              --  Create a new ac_value
              image_array (x + ac_x, y + ac_y, compo_idx) :=
                ac_value * 2 ** bit_position_low;

              if full_trace then
                Put_Line
                  (dump_file,
                   dump_sep & dump_sep & dump_sep &
                   Integer_32'Image (x + ac_x) & dump_sep &
                   Integer_32'Image (y + ac_y) & dump_sep &
                   mem_ac_value'Image & dump_sep &
                   image_array
                     (x + ac_x, y + ac_y, compo_idx)'Image & dump_sep &
                   "new ac_value" & dump_sep &
                   "new bit =" & Integer'Image (ac_value * 2 ** bit_position_low));
              end if;
              --  Move to the next value
              index := index + 1;
            end if;

            --  Refine AC values skipped by the zero run
            if refining then
              Refine_AC;
            end if;

          end loop Within_Band;

          --  Move to the next band if we are at the end of a band
          if index > spectral_selection_end then
            current_mcu := current_mcu + 1;
            if refining then
              --  Coordinates of the MCU's corner on the image
              x := Natural_32 ((current_mcu mod mcu_count_h) * 8);
              y := Natural_32 ((current_mcu  /  mcu_count_h) * 8);
            end if;
          end if;

          --  Perform the end of band run
          if refining then

            while eob_run > 0 loop
              xr := zag_zig (index, 1);
              yr := zag_zig (index, 2);
              current_value := image_array (x + xr, y + yr, compo_idx);

              if current_value /= 0 then
                Append (x + xr, y + yr);
              end if;

              index := index + 1;
              if index > spectral_selection_end then

                --  Move to the next band
                eob_run := eob_run - 1;
                current_mcu := current_mcu + 1;
                index := spectral_selection_start;

                --  Coordinates of the MCU's corner on the image
                x := Natural_32 ((current_mcu mod mcu_count_h) * 8);
                y := Natural_32 ((current_mcu  /  mcu_count_h) * 8);
              end if;
            end loop;

            Refine_AC;

          else
             --  First scan
             current_mcu := current_mcu + eob_run;
             eob_run := 0;
          end if;

          if current_mcu < mcu_count - 1 then
            Check_Restart (do_reset_predictors => False);
          end if;

        end loop MCU_Loop;
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
      if full_trace then
        New_Line;
        Put_Line ("Progressive scan kind : " & kind'Image);
        Put_Line ("Bit position high =" & bit_position_high'Image);
        Put_Line ("Bit position low  =" & bit_position_low'Image);
        Put_Line ("Refining scan : "    & refining'Image);
      end if;

      case kind is
        when DC => Progressive_DC_Scan;  --  First scan
        when AC => Progressive_AC_Scan;  --  Further scans
      end case;
    end Progressive_DCT_Decoding_Scan;

    procedure Finalize_Progressive_DCT_Decoding is
      --  Here we adapt the code of NanoJPEG, which does everything
      --  per MCU and saves us from saving the entire final image
      --  as in PyJpegDecoder.
      mb : Macro_8x8_Block (Component, 1 .. ssxmax, 1 .. ssymax);
      x0, y0 : Integer_32 := 0;
      mb_x, mb_y : Natural := 0;
      c_idx : Natural;

      --  Coordinates in the temporary image_array are *not* extended
      --  for the upsampling, which gives a bit more complexity, especially
      --  since it depends on settings *per component*...
      x_image_array, y_image_array :
        array (Component) of Integer_32 := (others => 0);

      qt_zz : array (Component) of JPEG_Defs.Quantization_Table;

      --  Reverse the zig-zag. If you follow the sequence 0, 1, 2, 3, ...
      --  in the array below you find the original zig-zag sequence.
      --  See also:
      --    Figure A.6 - Zig-zag sequence of quantized DCT coefficients.
      --
      undo_zig_zag : constant array (0 .. 63) of Integer :=
        (0,  1,  5,  6, 14, 15, 27, 28,
         2,  4,  7, 13, 16, 26, 29, 42,
         3,  8, 12, 17, 25, 30, 41, 43,
         9, 11, 18, 24, 31, 40, 44, 53,
        10, 19, 23, 32, 39, 45, 52, 54,
        20, 22, 33, 38, 46, 51, 55, 60,
        21, 34, 37, 47, 50, 56, 59, 61,
        35, 36, 48, 49, 57, 58, 62, 63);

    begin
      c_idx := 0;
      for c in Component loop
        if image.JPEG_stuff.compo_set (c) then
          c_idx := c_idx + 1;
          for i in qt_zz (c)'Range loop
            qt_zz (c)(i) :=
              image.JPEG_stuff.qt_list (info_A (c).qt_assoc)
                (undo_zig_zag (i));
          end loop;
          if full_trace then
            Put_Line (dump_file, "Image array (pre-IDCT) for " & c'Image);
            for y in image_array'Range (2) loop
              for x in image_array'Range (1) loop
                Put (dump_file, image_array (x, y, c_idx)'Image & dump_sep);
              end loop;
              New_Line (dump_file);
            end loop;
            New_Line (dump_file);
          end if;
        end if;
      end loop;
      --
      macro_blocks_loop :
      loop
        c_idx := 0;
        components_loop :
        for c in Component loop
          if image.JPEG_stuff.compo_set (c) then
            c_idx := c_idx + 1;
            samples_y_loop :
            for sby in 1 .. info_A (c).samples_ver loop
              samples_x_loop :
              for sbx in 1 .. info_A (c).samples_hor loop
                declare
                  block : Block_8x8 renames mb (c, sbx, sby);
                  qt_local : JPEG_Defs.Quantization_Table renames qt_zz (c);
                begin
                  --  Copy block data:
                  for yb in 0 .. 7 loop
                    for xb in 0 .. 7 loop
                      block (xb + yb * 8) :=
                        image_array
                          (x_image_array (c) + Integer_32 (8 * (sbx - 1) + xb),
                           y_image_array (c) + Integer_32 (8 * (sby - 1) + yb),
                           c_idx);
                    end loop;
                  end loop;
                  --  Undo quantization on the block:
                  for i in 0 .. 63 loop
                    block (i) := block (i) * qt_local (i);
                  end loop;
                  Inverse_DCT_8x8_Block (block);
                end;
              end loop samples_x_loop;
            end loop samples_y_loop;
          end if;
          x_image_array (c) :=
            x_image_array (c) + Integer_32 (info_A (c).samples_hor * 8);
        end loop components_loop;
        --  All components of the current macro-block are now processed.
        --  Steps 4, 5, 6 happen here:
        Upsampling_and_Output (mb, x0, y0);
        --
        mb_x := mb_x + 1;
        x0 := x0 + Integer_32 (sample_shape_max_x);
        if mb_x >= mcu_count_image_h then
          mb_x := 0;
          x0 := 0;
          mb_y := mb_y + 1;
          y0 := y0 + Integer_32 (sample_shape_max_y);
          for c in Component loop
            x_image_array (c) := 0;
            y_image_array (c) :=
              y_image_array (c) + Integer_32 (info_A (c).samples_ver * 8);
          end loop;
          Feedback (50 + (50 * mb_y) / mcu_count_image_v);
          exit macro_blocks_loop when mb_y >= mcu_count_image_v;
        end if;
      end loop macro_blocks_loop;
    end Finalize_Progressive_DCT_Decoding;

    --  Start Of Scan (and image data which follow)
    --
    procedure Read_SOS is

      procedure Create_Image_Array is
        count_h, count_v, sx, sy : Natural_32;
      begin
        --  3-dimensional array to store the color values of each pixel on the image
        --  array(x-coordinate, y-coordinate, color)
        sx := Natural_32 (sample_shape_max_x);
        sy := Natural_32 (sample_shape_max_y);
        --  Include padding if dimensions are not exactly a multiple of sx or sy.
        count_h := (image.width  / sx) + (if image.width  mod sx = 0 then 0 else 1);
        count_v := (image.height / sy) + (if image.height mod sy = 0 then 0 else 1);
        array_width  := sx * count_h;
        array_height := sy * count_v;

        image_array :=
          new Progressive_Bitmap
            (0 .. array_width - 1,
             0 .. array_height - 1,
             1 .. image.subformat_id);

        for x in image_array'Range (1) loop
          for y in image_array'Range (2) loop
            for c_idx in image_array'Range (3) loop
              image_array (x, y, c_idx) := 0;
            end loop;
          end loop;
        end loop;
      end Create_Image_Array;

      b, id_base : U8;
      mcu_width, mcu_height : Natural;
      --  Parameters for progressive decoding :
      start_spectral_selection,
      end_spectral_selection,
      successive_approximation : U8;
      type LF is digits 15;
      sample_ratio_h, sample_ratio_v : LF;
      layer_width, layer_height : LF;

    begin
      scan_count := scan_count + 1;
      compo := Component'First;
      Get_Byte (image.buffer, components_amount);
      if some_trace then
        New_Line;
        Put_Line
          ("  Start of Scan (SOS) number" & scan_count'Image & ", with" &
           components_amount'Image & " components");
      end if;
      if Natural (components_amount) > image.subformat_id  then
        raise error_in_image_data
          with "JPEG: Scan segment has more color components than the image";
      end if;

      scan_compo_set := (others => False);
      id_base := 1;
      for i in 1 .. components_amount loop
        Get_Byte (image.buffer, b);
        if b = 0 then
          --  Workaround for bugged encoder (see occurrence above
          --  for image's header in Read_SOF)
          id_base := 0;
          if full_trace then
            Put_Line ("SOS: Off-by one error in image data: component Id");
          end if;
        end if;
        if b - id_base > Component'Pos (Component'Last) then
          raise error_in_image_data with "JPEG: Scan: invalid ID:" & b'Image;
        end if;
        compo := Component'Val (b - id_base);
        scan_compo_set (compo) := True;
        if not image.JPEG_stuff.compo_set (compo) then
          raise error_in_image_data with
            "JPEG: scan component " & compo'Image &
            " has not been defined in the image header (SOF) segment";
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

      if components_amount > 1 then
        mcu_width  := sample_shape_max_x;  --  Pixels in a row of a MCU (Minimum Coded Unit) block
        mcu_height := sample_shape_max_y;  --  Pixels in a column of a MCU block
        mcu_count_h := (Integer (image.width)  + mcu_width - 1) / mcu_width;
        mcu_count_v := (Integer (image.height) + mcu_height - 1) / mcu_height;
        mcu_count_image_h := mcu_count_h;
        mcu_count_image_v := mcu_count_v;
      else
        --  PyJpegDecoder:
        --  "If there is only one color component in the scan,
        --   then the MCU size is always 8 x 8."
        mcu_width  := 8;
        mcu_height := 8;
        sample_ratio_h := LF (sample_shape_max_x) / LF (image.JPEG_stuff.info (compo).shape_x);
        sample_ratio_v := LF (sample_shape_max_y) / LF (image.JPEG_stuff.info (compo).shape_y);
        layer_width  := LF (image.width) / sample_ratio_h;
        layer_height := LF (image.height) / sample_ratio_v;
        mcu_count_h := Integer (LF'Ceiling (layer_width / LF (mcu_width)));
        mcu_count_v := Integer (LF'Ceiling (layer_height / LF (mcu_height)));
        --  In case of a monochrome image, all scans have
        --  a single component (Y).
        if mcu_count_image_h = 0 then
          mcu_count_image_h := mcu_count_h;
        end if;
        if mcu_count_image_v = 0 then
          mcu_count_image_v := mcu_count_v;
        end if;
      end if;

      mcu_count := mcu_count_h * mcu_count_v;

      --  Create the image array (if one does not exist already)
      if image.progressive and then image_array = null then
        Create_Image_Array;
      end if;

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
        if scan_compo_set (c) then
          info_B (c).width  := (Integer (image.width)  * info_A (c).samples_hor + ssxmax - 1) / ssxmax;
          info_B (c).height := (Integer (image.height) * info_A (c).samples_ver + ssymax - 1) / ssymax;
          info_B (c).stride := (mcu_count_h * mcu_width * info_A (c).samples_hor) / ssxmax;
          if some_trace then
            New_Line;
            Put_Line ("    Details for component: . . . . . " & c'Image);
            Put_Line ("      samples in x " & info_A (c).samples_hor'Image);
            Put_Line ("      samples in y " & info_A (c).samples_ver'Image);
            Put_Line ("      width  " & info_B (c).width'Image);
            Put_Line ("      height " & info_B (c).height'Image);
            Put_Line ("      stride " & info_B (c).stride'Image);
            Put_Line
              ("      AC/DC table index:  AC:" &
               info_B (compo).ht_idx_AC'Image & ", DC:" &
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

      Initialize_Bit_Buffer;
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

    head : Segment_Head;
    b : U8;

    procedure Dispose is
      new Ada.Unchecked_Deallocation
        (Progressive_Bitmap, Progressive_Bitmap_Access);

  begin  --  Load
    if full_trace then
      Create (dump_file, Out_File, "jpeg_dump.csv");
      --  Put_Line (dump_file, "Progressive : " & image.progressive'Image);
    end if;
    loop
      if full_trace then
        Put_Line ("Reading Segment Marker (Load)");
      end if;
      Read (image, memo_marker /= 0, memo_marker, head);
      memo_marker := 0;
      case head.kind is
        when DQT =>
          --  Quantization Table
          Read_DQT (image, Natural (head.length));
        when DHT =>
          --  Huffman Table
          Read_DHT (image, Natural (head.length));
        when DRI =>
          --  Restart Interval
          Read_DRI (image);
        when EOI =>
          --  End Of Input
          if some_trace then
            New_Line;
            Put_Line ("EOI marker");
          end if;
          if image.progressive then
            Finalize_Progressive_DCT_Decoding;
          end if;
          exit;
        when SOS =>
          --  Start Of Scan
          Read_SOS;
          exit when no_trace and not image.progressive;
          --  ^  When there is a trace we are interested in
          --       what appears after the scan, even a single one.
          --     When the image is progressive we have to
          --       continue because there are multiple scans.
          if full_trace then
            New_Line;
            Put_Line ("SOS marker done");
            Put_Line ("  Bit buffer length:   " & bit_buffer_length'Image);
            Put_Line ("  Bit buffer contents: " & bit_buffer'Image);
            Put_Line ("  Marker buffer: "       & memo_marker'Image);
          end if;
        when COM =>
          --  B.2.4.5 Comment
          if some_trace then
            New_Line;
            Put_Line ("JPEG Comment (during Load):  --------");
            for i in 1 .. head.length loop
              Get_Byte (image.buffer, b);
              Put (Character'Val (b));
            end loop;
            New_Line;
            Put_Line ("-------------------------------------");
            New_Line;
          else
            Skip_Segment_Data (image, head);
          end if;
        when others =>
          Skip_Segment_Data (image, head);
      end case;
    end loop;
    Dispose (image_array);
    if full_trace then
      Close (dump_file);
      if image.progressive then
        New_Line;
        Put_Line
          ("Longest refining list:" &
           max_refine_index_last'Image & " points.");
      end if;
    end if;
  end Load;

end GID.Decoding_JPG;
