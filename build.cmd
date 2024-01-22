@echo off
echo ** Building GID in various compiler modes
echo.
echo **** Debug ... tb.exe
del test\to_bmp.exe
gprbuild -p -P gid -XGID_Build_Mode=Debug to_bmp
copy /b test\to_bmp.exe test\tb.exe

echo.
echo **** Fast_but_checked ... tbfu.exe
del test\to_bmp.exe
gprbuild -p -P gid -XGID_Build_Mode=Fast_unchecked
copy /b test\to_bmp.exe test\tbfu.exe

echo.
echo **** Fast_but_checked ... tbfc.exe
del test\to_bmp.exe
gprbuild -p -P gid -XGID_Build_Mode=Fast_but_checked to_bmp
copy /b test\to_bmp.exe test\tbfc.exe

echo.
echo **** Smallest ... mini.exe
del test\mini.exe
gprbuild -p -P gid -XGID_Build_Mode=Smallest mini

pause
