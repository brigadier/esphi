#!/bin/sh
# /bin/sh on Solaris is not a POSIX compatible shell, but /usr/bin/ksh is.
if [ `uname -s` = 'SunOS' -a "${POSIX_SHELL}" != "true" ]; then
    POSIX_SHELL="true"
    export POSIX_SHELL
    exec /usr/bin/ksh $0 $@
fi
unset POSIX_SHELL # clear it so if we invoke other scripts, they run as ksh as well

SOPHIA_VSN=""


set -e

if [ `basename $PWD` != "c_src" ]; then
    # originally "pushd c_src" of bash
    # but no need to use directory stack push here
    cd c_src
fi

BASEDIR="$PWD"

# detecting gmake and if exists use it
# if not use make
# (code from github.com/tuncer/re2/c_src/build_deps.sh
which gmake 1>/dev/null 2>/dev/null && MAKE=gmake
MAKE=${MAKE:-make}

# Changed "make" to $MAKE

case "$1" in
    rm-deps)
        rm -rf sophia
        ;;

    clean)
        if [ -d sophia ]; then
            (cd sophia && $MAKE clean)
        fi
#        rm -f ../priv/leveldb_repair ../priv/sst_scan ../priv/sst_rewrite ../priv/perf_dump
        ;;

    test)
        export CFLAGS="$CFLAGS -I $BASEDIR/system/include"
        export CXXFLAGS="$CXXFLAGS -I $BASEDIR/system/include"
        export LDFLAGS="$LDFLAGS -L$BASEDIR/system/lib"
        export LD_LIBRARY_PATH="$BASEDIR/system/lib:$LD_LIBRARY_PATH"
        export SOPHIA_VSN="SOPHIA_VSN"

        (cd sophia && $MAKE check)

        ;;

    get-deps)
        if [ ! -d sophia ]; then
            git clone https://github.com/pmwkaa/sophia/
            (cd sophia && git checkout $SOPHIA_VSN)
        fi
        ;;

    *)

        export CFLAGS="$CFLAGS -I $BASEDIR/system/include"
        export CXXFLAGS="$CXXFLAGS -I $BASEDIR/system/include"
        export LDFLAGS="$LDFLAGS -L$BASEDIR/system/lib"
        export LD_LIBRARY_PATH="$BASEDIR/system/lib:$LD_LIBRARY_PATH"
        export SOPHIA_VSN="$SOPHIAROCKSDB_VSN"

        if [ ! -d sophia ]; then
            git clone https://github.com/pmwkaa/sophia/
            (cd sophia && git checkout $SOPHIA_VSN)
        fi
        (cd sophia && $MAKE -j 3)

        ;;
esac
