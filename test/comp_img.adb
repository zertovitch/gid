with Comp_Img_Fct;

with Ada.Command_Line,
     Ada.Text_IO;

procedure Comp_Img is
  use Ada.Command_Line, Ada.Text_IO;
begin
  if Argument_Count < 2 then
    Put_Line ("Usage: comp_img image1 image2");
    return;
  end if;
  Put_Line
    ("Difference score (0: identical, 1: black/white):" &
     Comp_Img_Fct (Argument (1), Argument (2), True)'Image);
end Comp_Img;
