with GID.Buffering;

package body GID.Decoding_QOI is
  use Interfaces;

  ----------
  -- Load --
  ----------

  procedure Load (image : in out Image_descriptor) is

    procedure Row_start (y : Natural) is
    begin
      Set_X_Y (0, Integer (image.height) - 1 - y);
    end Row_start;

    type Pixel is record
      r, g, b, a : U8;
    end record;

    run : U8 := 0;                              --  Run-Length encoding
    index : array (U8'(0) .. 63) of Pixel :=    --  Index of recent pixels
             (others => (0, 0, 0, 0));          --    (moving palette)
    px : Pixel := (0, 0, 0, 255);               --  Current pixel

    procedure Output_Pixel is
    pragma Inline (Output_Pixel);
      function Times_257 (x : Primary_color_range) return Primary_color_range is
      pragma Inline (Times_257);
      begin
        return 16 * (16 * x) + x;  --  this is 257 * x, equal to 16#0101# * x
        --  Numbers 8-bit -> no OA warning at instantiation.
        --  Returns x if type Primary_color_range is mod 2**8.
      end Times_257;
    begin
      case Primary_color_range'Modulus is
        when 256 =>
          Put_Pixel (
            Primary_color_range (px.r),
            Primary_color_range (px.g),
            Primary_color_range (px.b),
            Primary_color_range (px.a)
          );
        when 65_536 =>
          Put_Pixel (
            Times_257 (Primary_color_range (px.r)),
            Times_257 (Primary_color_range (px.g)),
            Times_257 (Primary_color_range (px.b)),
            Times_257 (Primary_color_range (px.a))
            --  Times_257 makes max intensity FF go to FFFF
          );
        when others =>
          raise invalid_primary_color_range with "Color range not supported";
      end case;
    end Output_Pixel;

    QOI_OP_INDEX : constant := 2#00_000000#;
    QOI_OP_DIFF  : constant := 2#01_000000#;
    QOI_OP_LUMA  : constant := 2#10_000000#;
    QOI_OP_RUN   : constant := 2#11_000000#;

    QOI_OP_RGB   : constant := 2#11_111110#;
    QOI_OP_RGBA  : constant := 2#11_111111#;

    QOI_MASK_1   : constant := 2#00_111111#;
    QOI_MASK_2   : constant := 2#11_000000#;

    function QOI_COLOR_HASH (C : Pixel) return U8 is
    pragma Inline (QOI_COLOR_HASH);
    begin
      return C.r * 3 + C.g * 5 + C.b * 7 + C.a * 11;
    end QOI_COLOR_HASH;

    b1, b2, vg : U8;
    eos_good : Boolean;

    use GID.Buffering;

  begin
    for y in 0 .. Integer (image.height) - 1 loop
      Row_start (y);
      for x in 0 .. Integer (image.width) - 1 loop
        if run > 0 then
          run := run - 1;
        else
          Get_Byte (image.buffer, b1);
          case b1 and QOI_MASK_2 is
            when QOI_OP_INDEX =>
              px := index (b1);
            when QOI_OP_DIFF =>
              px.r := px.r + (Shift_Right (b1, 4) and 3) - 2;
              px.g := px.g + (Shift_Right (b1, 2) and 3) - 2;
              px.b := px.b + (b1                  and 3) - 2;
            when QOI_OP_LUMA =>
              Get_Byte (image.buffer, b2);
              vg := (b1 and QOI_MASK_1) - 32;
              px.r := px.r + vg - 8 + (Shift_Right (b2, 4) and 16#0f#);
              px.g := px.g + vg;
              px.b := px.b + vg - 8 + (b2                  and 16#0f#);
            when QOI_OP_RUN =>
              case b1 is
                when QOI_OP_RGB =>
                  Get_Byte (image.buffer, px.r);
                  Get_Byte (image.buffer, px.g);
                  Get_Byte (image.buffer, px.b);
                when QOI_OP_RGBA =>
                  Get_Byte (image.buffer, px.r);
                  Get_Byte (image.buffer, px.g);
                  Get_Byte (image.buffer, px.b);
                  Get_Byte (image.buffer, px.a);
                when others =>
                  run := b1 and QOI_MASK_1;
                  --  One extra run iteration is done just here by not changing px.
              end case;
            when others => null;
          end case;
          index (QOI_COLOR_HASH (px) and QOI_MASK_1) := px;
        end if;
        Output_Pixel;
      end loop;
      Feedback (((y + 1) * 100) / Integer (image.height));
    end loop;
    --  Check end of stream signature:
    eos_good := True;
    for count in 1 .. 7 loop
      Get_Byte (image.buffer, b1);
      eos_good := eos_good and b1 = 0;
    end loop;
    Get_Byte (image.buffer, b1);
    eos_good := eos_good and b1 = 1;
    if not eos_good then
      raise error_in_image_data with "QOI format: unexpected end of stream";
    end if;
  end Load;

end GID.Decoding_QOI;
