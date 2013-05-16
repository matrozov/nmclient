@echo off
copy /Y nmclient.exe output\nmclient.exe
copy /Y nmclient output\nmclient_linux
upx309w\upx.exe -9 output\nmclient.exe
upx309w\upx.exe -9 output\nmclient_linux