package body Color_Distances is

  function Distance (p, q : RGB) return Natural is
  begin
    --  The goal of the use of generics for dist_choice
    --  is to optimize out the following case statement
    --  when the function is inlined.
    case dist_choice is
      when L1 =>
        return
          abs (Integer (p.r) - Integer (q.r)) +
          abs (Integer (p.g) - Integer (q.g)) +
          abs (Integer (p.b) - Integer (q.b));
      when L2 =>
        return
          (Integer (p.r) - Integer (q.r)) ** 2 +
          (Integer (p.g) - Integer (q.g)) ** 2 +
          (Integer (p.b) - Integer (q.b)) ** 2;
      when L3 =>
        return
          (abs (Integer (p.r) - Integer (q.r))) ** 3 +
          (abs (Integer (p.g) - Integer (q.g))) ** 3 +
          (abs (Integer (p.b) - Integer (q.b))) ** 3;
      when Linf =>
        return
          Integer'Max
            (Integer'Max
              ((Integer (p.r) - Integer (q.r)),
               (Integer (p.g) - Integer (q.g))),
             (Integer (p.b) - Integer (q.b)));
    end case;
  end Distance;

  function Normalized_Distance (p, q : RGB) return Real is
    function Distance_Instance is new Distance (dist_choice_for_nd);
    raw_value : constant Real := Real (Distance_Instance (p, q));
  begin
    case dist_choice_for_nd is
      when L1 =>
        return raw_value / (3.0 * Real (Primary_Color_Range'Last));
      when L2 =>
        return raw_value / (3.0 * Real (Primary_Color_Range'Last) ** 2);
      when L3 =>
        return raw_value / (3.0 * Real (Primary_Color_Range'Last) ** 3);
      when Linf =>
        return raw_value / Real (Primary_Color_Range'Last);
    end case;
  end Normalized_Distance;

end Color_Distances;
