@echo off

rem Compile the sample code for MS Windows and build a runtime system
rem See compile2.sh for more details

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

rem Create a saved state from the Prolog code (--nologo --noinfo makes sicstus quiet)
"%SPINSTALLDIR%\bin\sicstus.exe" --nologo --noinfo -l evaluate.pl --goal "save_program('evaluate.sav'),halt."

rem Note: you may need to run the Visual Studio compiler setup file before invoking spld.exe, e.g.:
rem "C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvars64.bat"
rem
rem Build a static runtime system. For usage, see run2.bat.
"%SPINSTALLDIR%\bin\spld.exe" --static -o evaluate.exe evaluate.sav
