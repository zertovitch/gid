echo Convert large high-res images from the NASA  

call clean
to_bmp - nasa*.jpg nasa*.png

for /l %%i in (1,1,7) do timeit -f gid.dat -k png001 to_bmp.exe - nasa*.png
for /l %%i in (1,1,7) do timeit -f gid.dat -k jpg001 to_bmp.exe - nasa*.jpg

timeit -f gid.dat

pause
