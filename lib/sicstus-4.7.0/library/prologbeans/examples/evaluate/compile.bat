@echo off

rem Compile the sample code for MS Windows
rem See compile.sh for more details

SETLOCAL

rem You can set the environment variable SPINSTALLDIR to override the
rem SICStus installation used
rem E.g.: set SPINSTALLDIR=C:\Program Files\SICStus Prolog VC15 4.4.1

if "%SPINSTALLDIR%"=="" set SPINSTALLDIR=..\..\..\..

rem You can set JAVAC to the full path to the javac.exe Java
rem compiler. Otherwise it will be looked for in the PATH environment
rem variable.
rem E.g.: set JAVAC=C:\Program Files\Java\jdk-11.0.1\bin\javac.exe
if "%JAVAC%"=="" set JAVAC=javac.exe

"%JAVAC%" -classpath "%SPINSTALLDIR%\bin\prologbeans.jar;." EvaluateGUI.java
