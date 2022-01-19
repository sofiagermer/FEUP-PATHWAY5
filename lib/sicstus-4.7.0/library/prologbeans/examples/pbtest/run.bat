@echo off

rem Run the Java side of the sample
rem See run.sh for more details

SETLOCAL

rem You can set the environment variable SPINSTALLDIR to override the
rem SICStus installation used
rem E.g.: set SPINSTALLDIR=C:\Program Files\SICStus Prolog VC15 4.4.1

if "%SPINSTALLDIR%"=="" set SPINSTALLDIR=..\..\..\..

echo You need to ensure that the sicstus executable can be found via the PATH environment variable
echo E.g. set PATH=%%PATH%%;%SPINSTALLDIR%\bin

echo Starting Java client

rem You can set JAVA to the full path to the java.exe Java
rem compiler. Otherwise it will be looked for in the PATH environment
rem variable.
rem E.g.: set JAVA=C:\Program Files\Java\jdk-11.0.1\bin\java.exe
if "%JAVA%"=="" set JAVA=java.exe

"%JAVA%" -classpath "%SPINSTALLDIR%\bin\prologbeans.jar;." PBTest
