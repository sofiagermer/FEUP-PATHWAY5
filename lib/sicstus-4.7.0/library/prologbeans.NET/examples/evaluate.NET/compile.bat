@echo off

rem Compile the sample code for MS Windows

SETLOCAL

rem You can set the environment variable SPINSTALLDIR to override the
rem SICStus installation used
rem E.g.: set SPINSTALLDIR=C:\Program Files\SICStus Prolog VC15 4.4.1

if "%SPINSTALLDIR%"=="" set SPINSTALLDIR=..\..\..\..

csc /reference:"%SPINSTALLDIR%\bin\prologbeans.dll" EvaluateGUI.cs

