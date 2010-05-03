--
-- Convert any image file to BMP file
--
-- Simple test for the GID (Generic Image Decoder) package
--

with GID;

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with Ada.Text_IO;                       use Ada.Text_IO;

procedure To_BMP is

  procedure Blurb is
  begin
    Put_Line(Standard_Error, "To_BMP * Converts any image file to BMP file");
    Put_Line(Standard_Error, "Simple test for the GID (Generic Image Decoder) package");
    Put_Line(Standard_Error, "Package version " & GID.version & " dated " & GID.reference);
    Put_Line(Standard_Error, "URL: " & GID.web);
    New_Line(Standard_Error);
    Put_Line(Standard_Error, "Syntax:");
    Put_Line(Standard_Error, "to_bmp image1 [image2 ...]");
    New_Line(Standard_Error);
    Put_Line(Standard_Error, "Output: "".bmp"" is added the full input name(s)");
    New_Line(Standard_Error);
  end Blurb;

  -- Image used as background for displaying images having transparency
  background_image: constant String:= "gid.gif";

  procedure Process(name: String) is
    f: Ada.Streams.Stream_IO.File_Type;
    i: GID.Image_descriptor;
  begin
    Open(f, In_File, name);
    Put_Line(Standard_Error, "Processing " & name & "...");
    begin
      GID.Load_image_header(i, Stream(f).all, try_tga => True);
      Put_Line(Standard_Error,
        "  Image format: " & GID.Image_format_type'Image(GID.Image_format(i))
      );
      Put_Line(Standard_Error,
        "  Image detailed format: " & GID.Image_detailed_format(i)
      );
    exception
      when GID.unknown_image_format =>
        Put_Line(Standard_Error, "  Image format is unknown!");
    end;
    Close(f);
  end Process;

begin
  if Argument_Count=0 then
    Blurb;
    return;
  end if;
  for i in 1..Argument_Count loop
    Process(Argument(i));
  end loop;
end To_BMP;