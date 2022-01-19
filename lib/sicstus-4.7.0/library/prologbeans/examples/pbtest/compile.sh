#! /bin/sh

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
javac -classpath "${PROLOGBEANS_JAR}:." PBTest.java
