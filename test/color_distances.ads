generic
  type Primary_Color_Range is mod <>;
  type Real is digits <>;

package Color_Distances is

  type RGB is record
    r, g, b : Primary_Color_Range;
  end record;

  subtype Real_Unit is Real range 0.0 .. 1.0;

  depth : constant Real := Real (Primary_Color_Range'Last);

  type RGB_Real is record
    r, g, b : Real;
  end record;

  type Dist_Type is (L1, L2, L3, Linf);

  generic
    dist_choice_1 : Dist_Type;
  function Distance_No_Root_1 (p, q : RGB) return Natural
  with Inline;

  generic
    dist_choice_2 : Dist_Type;
  function Distance_No_Root_2 (p : RGB; q : RGB_Real) return Real
  with Inline;

  generic
    dist_choice_for_nd_1 : Dist_Type;
  function Normalized_Distance_No_Root_1 (p, q : RGB) return Real_Unit
  with Inline;

end Color_Distances;
