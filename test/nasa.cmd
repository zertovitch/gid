echo Convert high-res image from the NASA  

to_bmp - nasa*.bmp

rem for /l %%i in (1,1,10) do timeit -f gid.dat -k simple_xy to_bmp_xy - nasa*.bmp

for /l %%i in (1,1,20) do timeit -f gid.dat -k adjacent to_bmp - nasa*.bmp

timeit -f gid.dat