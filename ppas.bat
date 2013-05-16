@echo off
SET THEFILE=nmclient.exe
echo Linking %THEFILE%
D:\Mear\Programs\lazarus\fpc\2.6.0\bin\i386-win32\ld.exe -b pei-i386 -m i386pe  --gc-sections  -s --subsystem windows --entry=_WinMainCRTStartup    -o nmclient.exe link.res
if errorlevel 1 goto linkend
D:\Mear\Programs\lazarus\fpc\2.6.0\bin\i386-win32\postw32.exe --subsystem gui --input nmclient.exe --stack 16777216
if errorlevel 1 goto linkend
goto end
:asmend
echo An error occured while assembling %THEFILE%
goto end
:linkend
echo An error occured while linking %THEFILE%
:end
