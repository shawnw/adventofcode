#!/bin/sh

if [ -z "${CLASSPATH}" ]; then
    CLASSPATH="${KAWA_HOME}/lib/kawa.jar:../kawalib/kawalib.jar:."
else
    CLASSPATH="${CLASSPATH}:${KAWA_HOME}/lib/kawa.jar:../kawalib/kawalib.jar:."
fi

export CLASSPATH

exec java $@

