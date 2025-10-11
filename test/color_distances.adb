package body Color_Distances is

  function Distance_No_Root_1 (p, q : RGB) return Natural is
  begin

    --  The goal of the use of generics for dist_choice
    --  is to optimize out the following case statement
    --  when the function is inlined.

    case dist_choice_1 is

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

  end Distance_No_Root_1;

  function Distance_No_Root_2 (p : RGB; q : RGB_Real) return Real is
  begin

    case dist_choice_2 is

      when L1 =>
        return
          abs (Real (p.r) - q.r) +
          abs (Real (p.g) - q.g) +
          abs (Real (p.b) - q.b);

      when L2 =>
        return
          (Real (p.r) - q.r) ** 2 +
          (Real (p.g) - q.g) ** 2 +
          (Real (p.b) - q.b) ** 2;

      when L3 =>
        return
          (abs (Real (p.r) - q.r)) ** 3 +
          (abs (Real (p.g) - q.g)) ** 3 +
          (abs (Real (p.b) - q.b)) ** 3;

      when Linf =>
        return
          Real'Max
            (Real'Max
              ((Real (p.r) - q.r),
               (Real (p.g) - q.g)),
             (Real (p.b) - q.b));
    end case;

  end Distance_No_Root_2;

  function Normalized_Distance_No_Root_1 (p, q : RGB) return Real_Unit is
    function Distance_Instance is new Distance_No_Root_1 (dist_choice_for_nd_1);
    raw_value : constant Real := Real (Distance_Instance (p, q));
  begin
    case dist_choice_for_nd_1 is
      when L1 =>
        return raw_value / (3.0 * Real (Primary_Color_Range'Last));
      when L2 =>
        return raw_value / (3.0 * Real (Primary_Color_Range'Last) ** 2);
      when L3 =>
        return raw_value / (3.0 * Real (Primary_Color_Range'Last) ** 3);
      when Linf =>
        return raw_value / Real (Primary_Color_Range'Last);
    end case;
  end Normalized_Distance_No_Root_1;

end Color_Distances;
