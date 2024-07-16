with Color_Statistics;

with Ada.Command_Line,
     Ada.Text_IO;

procedure Color_Avg is
  package LFIO is new Ada.Text_IO.Float_IO (Long_Float);
  package IIO is new Ada.Text_IO.Integer_IO (Long_Integer);
  use Ada.Command_Line, Ada.Text_IO, Color_Statistics, LFIO, IIO;

  function To_HTML (v : RGB_Values) return String is
    code : constant Long_Integer :=
      Long_Integer (255.0 * v.r) * 65536 +
      Long_Integer (255.0 * v.g) * 256 +
      Long_Integer (255.0 * v.b);
    --
    hexa : String (1 .. 11);
  begin
    Put (hexa, code + 16#1_00_00_00#, Base => 16);
    return '#' & hexa (5 .. 10);
  end To_HTML;

  procedure Simple (file_name : String) is
    stat : Segmented_Statistic (1 .. 1, 1 .. 1);
    width  : Positive;
    height : Positive;
  begin
    Compute (file_name, stat, width, height, True);
    Put_Line ("Average (range: 0.0 .. 1.0):");
    Put ("  Red   : "); Put (stat (1, 1).average.r, 0, 4, 0); New_Line;
    Put ("  Green : "); Put (stat (1, 1).average.g, 0, 4, 0); New_Line;
    Put ("  Blue  : "); Put (stat (1, 1).average.b, 0, 4, 0); New_Line;
    Put_Line ("  HTML code : " & To_HTML (stat (1, 1).average));
    New_Line;
  end Simple;

  procedure Mosaic (x, y : Positive; file_name : String) is
    stat : Segmented_Statistic (1 .. x, 1 .. y);
    html : File_Type;
    width  : Positive;
    height : Positive;
    w_cell, h_cell : Integer;
  begin
    Create (html, Out_File, "mosaic.html");
    Compute (file_name, stat, width, height, True);
    Put_Line (html, "<html>");
    Put_Line (html, "<table cellspacing=0 cellpadding=0>");
    w_cell := Integer'Max (1, width / x);
    h_cell := Integer'Max (1, height / y);
    for row in 1 .. y loop
      Put_Line (html, "  <tr>");
      for column in 1 .. x loop
        Put_Line
          (html,
           "    <td bgcolor=" & To_HTML (stat (column, row).average) &
           " width=" & w_cell'Image & " height=" & h_cell'Image & ">");
      end loop;
    end loop;
    Put_Line (html, "</table>");
    Put_Line (html, "</html>");
    Close (html);
  end Mosaic;

begin
  if Argument_Count < 1 then
    Put_Line (Current_Error, "Usage: color_avg [-m x y] <image>");
    New_Line (Current_Error);
    Put_Line (Current_Error, "Options:");
    Put_Line (Current_Error, "  -m x y   : create an HTML file with an x*y mosaic of averaged colors");
    New_Line (Current_Error);
    Put ("Press return");
    Skip_Line;
    return;
  end if;
  declare
    arg_1 : constant String := Argument (1);
  begin
    if arg_1 = "-m" then
      Mosaic (Integer'Value (Argument (2)), Integer'Value (Argument (3)), Argument (4));
    else
      Simple (arg_1);
    end if;
  end;
end Color_Avg;
