gprbuild -p -P gid -XGID_Build_Mode=Debug to_bmp
copy test\to_bmp.exe test\tb.exe
gprbuild -p -P gid -XGID_Build_Mode=Fast_unchecked
gprbuild -p -P gid -XGID_Build_Mode=Smallest mini
