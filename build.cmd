gprbuild -p -P gid -XGID_Build_Mode=Debug to_bmp
copy test\to_bmp.exe test\tb.exe
gprbuild -p -P gid -XGID_Build_Mode=Fast
gprbuild -p -P gid -XGID_Build_Mode=Smallest mini
