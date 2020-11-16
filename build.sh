#!/bin/sh
ROOT="$(cd $(dirname "$0")/; pwd -P)"

cd "$ROOT"

OCAMLC=ocamlopt.opt
OCAMLLEX=ocamllex
MENHIR=menhir

compile_lex() {
    "$OCAMLLEX" Qmakelex.mll
    result="$?"
    if [ $result -ne 0 ]; then
        echo '!!! ocamlc failed'
        exit $result
    fi
}

compile_menhir() {
    "$MENHIR" --table Qmakeparse.mly
    result="$?"
    if [ $result -ne 0 ]; then
        echo '!!! ocamlc failed'
        exit $result
    fi
}

compile() {
    mkdir -p out/
    "$OCAMLC" \
        -w -D \
        -w @8 \
        -g \
        -o out/qmp.exe \
        -I extlib/mparser/src \
        -I extlib/ocaml-fileutils/src/lib/fileutils \
        -I src \
        str.cmxa \
        extlib/ocaml-fileutils/src/lib/fileutils/FilePath_type.ml \
        extlib/ocaml-fileutils/src/lib/fileutils/FileStringExt.ml \
        extlib/ocaml-fileutils/src/lib/fileutils/CommonPath.ml \
        extlib/ocaml-fileutils/src/lib/fileutils/UnixPath.ml \
        extlib/ocaml-fileutils/src/lib/fileutils/Win32Path.ml \
        extlib/ocaml-fileutils/src/lib/fileutils/ExtensionPath.ml \
        extlib/ocaml-fileutils/src/lib/fileutils/FilePath.{mli,ml} \
        extlib/mparser/src/mParser_Sig.mli \
        extlib/mparser/src/mParser_Utils.{mli,ml} \
        extlib/mparser/src/mParser_Char_Stream.{mli,ml} \
        extlib/mparser/src/mParser.{mli,ml} \
        src/main_qmp.{mli,ml}
    result="$?"
    if [ $result -ne 0 ]; then
        echo '!!! ocamlc failed'
        exit $result
    fi
}

# compile_lex
# compile_menhir
compile
