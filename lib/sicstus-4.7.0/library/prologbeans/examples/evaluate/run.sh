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

# # Add it to PATH if it contains a sicstus executable
if test -x "${SPINSTALLDIR}/bin/sicstus"; then
    SICSTUS_BIN="${SPINSTALLDIR}/bin"
    # We do not need it on PATH. The user should start it.
    # PATH="$PATH:${SICSTUS_BIN}"
    :
else
    # We do not know path to sicstus executable.
    SICSTUS_BIN=''
fi

echo 'You need to _first_ start a Prolog server separately using something like:'
echo "'${SICSTUS_BIN}${SICSTUS_BIN:+/}sicstus' -l evaluate.pl --goal 'main,halt.'"

echo 'Starting Java client (Prolog must already be running and waiting for connection)'

java -classpath "${PROLOGBEANS_JAR}:." EvaluateGUI
