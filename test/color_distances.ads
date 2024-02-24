generic
  type Primary_Color_Range is mod <>;
  type Real is digits <>;

package Color_Distances is

  type RGB is record
    r, g, b : Primary_Color_Range;
  end record;

  type Dist_Type is (L1, L2, L3, Linf);

  generic
    dist_choice : Dist_Type;
  function Distance (p, q : RGB) return Natural
  with Inline;

  --  Normalized_Distance returns a number between 0 and 1.

  generic
    dist_choice_for_nd : Dist_Type;
  function Normalized_Distance (p, q : RGB) return Real
  with Inline;

end Color_Distances;
