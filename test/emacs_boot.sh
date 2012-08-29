#!/bin/sh

bootemacs() {
    EMACS=emacs
    if [ "$#" = 0 ]; then
        EMACS=emacs23
    fi
    ${EMACS} -q --load `pwd`/logalimacs_configs.el
}

bootemacs
