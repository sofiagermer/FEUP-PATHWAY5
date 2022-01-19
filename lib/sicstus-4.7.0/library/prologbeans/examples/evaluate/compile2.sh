#! /bin/sh

# Example script for creating a runtime system from the Prolog code
# Also compiles the Java code.

# Assumes that the working directory is in the installation folder
# tree, but you can set set the environment variable SPINSTALLDIR to
# override the SICStus installation used. E.g.
#
# bash$ export SPINSTALLDIR=$HOME/my_local_sp_install
# bash$ run the script

set -e # exit immediately on error

# Sometimes symbolic links can cause parent links to go the wrong way, so canonicalize with pwd

# E.g. /usr/local/sicstus4.4.1
: ${SPINSTALLDIR:="$(cd ../../../../../.. && pwd)"}

# E.g. /usr/local/sicstus4.4.1/lib/sicstus-4.4.1/bin
: ${SPJARDIR:="$(cd "${SPINSTALLDIR}"/lib/sicstus-*/bin && pwd)"}

# E.g. /usr/local/sicstus4.4.1/lib/sicstus-4.4.1/bin/prologbeans.jar
: ${PROLOGBEANS_JAR:="${SPJARDIR}/prologbeans.jar"}

# Compiler the Java code
javac -classpath "${PROLOGBEANS_JAR}:." EvaluateGUI.java

# Create a saved state from the Prolog code (--nologo --noinfo makes sicstus quiet)
"${SPINSTALLDIR}/bin/sicstus" --nologo --noinfo -l evaluate.pl --goal "save_program('evaluate.sav'),halt."

# Build a static runtime system. For usage, see run2.sh.
"${SPINSTALLDIR}/bin/spld" --static -o evaluate evaluate.sav
