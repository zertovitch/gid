echo Convert large high-res images from the NASA  

copy nasa6.jpg r:
copy nj*.exe r:
copy to_bmp.exe r:

r:
del *.dib
del *.ppm

to_bmp - nasa6.jpg

rem for /l %%i in (1,1,7) do timeit -f gid.dat -k png001 to_bmp.exe - nasa*.png
for /l %%i in (1,1,7) do timeit -f c:gid.dat -k jpg001       to_bmp.exe nasa6.jpg
for /l %%i in (1,1,7) do timeit -f c:gid.dat -k nj_0_1.1     nj0.exe    nasa6.jpg
for /l %%i in (1,1,7) do timeit -f c:gid.dat -k nj_bicub_1.1 nj.exe     nasa6.jpg

c:

timeit -f gid.dat

pause
