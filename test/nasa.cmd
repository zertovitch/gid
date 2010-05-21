echo Convert high-res image from the NASA  

to_bmp - nasa*.gif

rem for /l %%i in (1,1,20) do timeit -f gid.dat -k color_modular to_bmp - nasa*.bmp

rem for /l %%i in (1,1,7) do timeit -f gid.dat -k gif1 to_bmp.1.exe - nasa*.gif
for /l %%i in (1,1,7) do timeit -f gid.dat -k gif3 to_bmp.exe - nasa*.gif

timeit -f gid.dat