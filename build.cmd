md obj_debug
md obj_fast
md obj_smallest

gnatmake -P gid -XBuild_Mode=Debug tb
gnatmake -P gid -XBuild_Mode=Fast
gnatmake -P gid -XBuild_Mode=Smallest mini
