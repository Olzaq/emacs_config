#!/bin/bash

SETUP_LOCATION="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

echo "Config location ${SETUP_LOCATION}"

CONFIG_INSTALL_DIR=${HOME}

BASH_ALIASES=${CONFIG_INSTALL_DIR}/.bash_alisases
EMACS_INIT=${CONFIG_INSTALL_DIR}/.emacs
EMACS_MODULES=${CONFIG_INSTALL_DIR}/.emacs_modules
GDB_INIT=${CONFIG_INSTALL_DIR}/.gdbinit
GIT_CONFIG=${CONFIG_INSTALL_DIR}/.gitconfig
GIT_IGNORE=${CONFIG_INSTALL_DIR}/.gitignore
SCREEN_RC=${CONFIG_INSTALL_DIR}/.screenrc

ln -sv ${SETUP_LOCATION}/bash_alias      ${BASH_ALIASES}
ln -sv ${SETUP_LOCATION}/emacs           ${EMACS_INIT}
ln -sv ${SETUP_LOCATION}/gdbinit         ${GDB_INIT}
ln -sv ${SETUP_LOCATION}/gitconfig       ${GIT_CONFIG}
ln -sv ${SETUP_LOCATION}/gitignoreGlobal ${GIT_IGNORE}
ln -sv ${SETUP_LOCATION}/screenrc        ${SCREEN_RC}

ln -sv ${SETUP_LOCATION}/ext  ${EMACS_MODULES}
