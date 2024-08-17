@echo off
echo ** Building GID in various compiler modes
echo.

echo **** To_PNG - Debug ... tp.exe
del test\to_png.exe
gprbuild -p -P gid -XGID_Build_Mode=Debug to_png
copy /b test\to_png.exe test\tp.exe

echo.
echo **** To_BMP - Debug ... tb.exe
del test\to_bmp.exe
gprbuild -p -P gid -XGID_Build_Mode=Debug to_bmp
copy /b test\to_bmp.exe test\tb.exe

echo.
echo **** To_PNG - Fast_but_checked ... tbfc.exe
del test\to_png.exe
gprbuild -p -P gid -XGID_Build_Mode=Fast_but_checked to_png
copy /b test\to_png.exe test\tpfc.exe

echo.
echo **** To_BMP - Fast_but_checked ... tbfc.exe
del test\to_bmp.exe
gprbuild -p -P gid -XGID_Build_Mode=Fast_but_checked to_bmp
copy /b test\to_bmp.exe test\tbfc.exe

echo.
echo **** Everything - Fast_unchecked ... tbfu.exe, tpfu.exe
del test\to_png.exe
del test\to_bmp.exe
gprbuild -p -P gid -XGID_Build_Mode=Fast_unchecked
copy /b test\to_png.exe test\tpfu.exe
copy /b test\to_bmp.exe test\tbfu.exe

echo.
echo **** Smallest ... mini.exe
del test\mini.exe
gprbuild -p -P gid -XGID_Build_Mode=Smallest mini

pause
