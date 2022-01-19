#! /bin/sh

# Example script for compiling the Java code.

# Assumes that the working directory is in the installation folder
# tree, but you can set set the environment variable SPINSTALLDIR to
# override the SICStus installation used. E.g.
#
# bash$ export SPINSTALLDIR=$HOME/my_local_sp_install
# bash$ run the script

set -e # exit immediately on error

# E.g. /usr/local/sicstus4.4.1
: ${SPINSTALLDIR:="$(cd ../../../../../.. && pwd)"}

# E.g. /usr/local/sicstus4.4.1/lib/sicstus-4.4.1/bin
: ${SPJARDIR:="$(cd "${SPINSTALLDIR}"/lib/sicstus-*/bin && pwd)"}

# E.g. /usr/local/sicstus4.4.1/lib/sicstus-4.4.1/bin/prologbeans.jar
: ${PROLOGBEANS_JAR:="${SPJARDIR}/prologbeans.jar"}

javac -classpath "${PROLOGBEANS_JAR}:." EvaluateGUI.java
