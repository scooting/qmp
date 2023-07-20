#!/usr/bin/env bash
#
if [ -z "${1+x}" ]; then
    echo "Missing positional argument 1: path to output root"
    exit 1
fi
ROOT="$1"

if [ -z "${2+x}" ]; then
    echo "Missing positional argument 2: path to qtbase root"
    exit 1
fi
BASEPFX="$2"

hoo1() {
# ROOT='/cygdrive/e/sdk/Qt/sqt-5.12.10/sqt'
# ROOT='/cygdrive/f/projects/qt/sqt-5.15.2/sqt'
# BASEPFX="$ROOT"/src/qt/qtbase
OCAMLRUNPARAM=b ./out/qmp.exe \
    "$ROOT" \
    bootstrap \
    "$BASEPFX"/src/tools/bootstrap/bootstrap.pro \
    corelib \
    "$BASEPFX"/src/corelib/corelib.pro \
    gui \
    "$BASEPFX"/src/gui/gui.pro \
    widgets \
    "$BASEPFX"/src/widgets/widgets.pro \
    platform_windows \
    "$BASEPFX"/src/plugins/platforms/windows/windows.pro \
    platform_windows_windowsuiautomation \
    "$BASEPFX"/src/platformsupport/windowsuiautomation/windowsuiautomation.pro \
    platform_generic_eventdispatchers \
    "$BASEPFX"/src/platformsupport/eventdispatchers/eventdispatchers.pro \
    platform_generic_fontdatabases \
    "$BASEPFX"/src/platformsupport/fontdatabases/fontdatabases.pro \
    platform_generic_themes \
    "$BASEPFX"/src/platformsupport/themes/themes.pro \
    network \
    "$BASEPFX"/src/network/network.pro \
    websockets \
    "$BASEPFX"/../qtwebsockets/src/websockets/websockets.pro \
    multimedia \
    "$BASEPFX"/../qtmultimedia/src/multimedia/multimedia.pro \
    audio_wasapi \
    "$BASEPFX"/../qtmultimedia/src/plugins/wasapi/wasapi.pro
}

hoo2() {
./out/qmp.exe \
    ./ \
    test \
    test.pro
}

hoo1


#OCAMLRUNPARAM=b ./qmp.exe "/cygdrive/e/sdk/Qt/qt-everywhere-src-5.12.10/qtbase/src/tools/bootstrap/bootstrap.pro"
#OCAMLRUNPARAM=b ./qmp.exe "/cygdrive/e/sdk/Qt/qt-everywhere-src-5.12.10/qtbase/src/network/network.pro"
#OCAMLRUNPARAM=b ./qmp.exe "/cygdrive/e/sdk/Qt/qt-everywhere-src-5.12.10/qtwebsockets/src/websockets/websockets.pro"
#OCAMLRUNPARAM=b ./qmp.exe "/cygdrive/e/sdk/Qt/qt-everywhere-src-5.12.10/qtmultimedia/src/src.pro"

# echo '##########################################################'
# echo '##########################################################'
# OCAMLRUNPARAM=b ./qmp.exe "/cygdrive/e/sdk/Qt/qt-everywhere-src-5.12.10/qtbase/src/corelib/global/global.pri"
#
# echo '##########################################################'
# echo '##########################################################'
#
# OCAMLRUNPARAM=b ./qmp.exe "/cygdrive/e/sdk/Qt/qt-everywhere-src-5.12.10/qtbase/src/corelib/kernel/kernel.pri"
# echo '##########################################################'
# echo '##########################################################'
# OCAMLRUNPARAM=b ./qmp.exe test.pro
