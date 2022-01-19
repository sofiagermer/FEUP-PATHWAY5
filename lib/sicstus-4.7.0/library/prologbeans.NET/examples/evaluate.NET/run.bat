@echo off

rem Run the C# side of the sample


SETLOCAL

rem You can set the environment variable SPINSTALLDIR to override the
rem SICStus installation used
rem E.g.: set SPINSTALLDIR=C:\Program Files\SICStus Prolog VC15 4.4.1

if "%SPINSTALLDIR%"=="" set SPINSTALLDIR=..\..\..\..
if "%SPBINDIR%"=="" set SPBINDIR=%SPINSTALLDIR%\bin

copy "%SPBINDIR%\prologbeans.dll" .

echo Starting .NET C# client
echo You need to _first_ start Prolog server separately using something like:
echo "%SPBINDIR%\sicstus.exe" -l ../../../prologbeans/examples/evaluate/evaluate.pl --goal "main,halt."

.\EvaluateGUI
