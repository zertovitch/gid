package Color_Statistics is

  type RGB_Values is record
    r, g, b : Long_Float;  --  Between 0 and 1.
  end record;

  type Statistics is record
     average : RGB_Values;
  end record;

  type Segmented_Statistic is array (Positive range <>, Positive range <>) of Statistics;

  --  When stat : Segmented_Statistic is dimentioned (1 .. 1, 1 .. 1), we want a single
  --  statistic for the entire image.

  procedure Compute
    (image_name : in     String;
     stat       :    out Segmented_Statistic;
     width      :    out Positive;
     height     :    out Positive;
     verbose    : in     Boolean);

end Color_Statistics;
