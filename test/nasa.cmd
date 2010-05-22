echo Convert high-res image from the NASA  

to_bmp - nasa*.gif

rem for /l %%i in (1,1,20) do timeit -f gid.dat -k color_modular to_bmp - nasa*.bmp

for /l %%i in (1,1,7) do timeit -f gid.dat -k gif10 to_bmp.exe - nasa*.gif

timeit -f gid.dat

pause
