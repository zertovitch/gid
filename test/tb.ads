-- Build with -bargs -E

with TB_Wrap, To_BMP;
pragma Elaborate_All(TB_Wrap);

procedure TB is new TB_Wrap(To_BMP);