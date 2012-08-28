#!/bin/sh

bootemacs() {
    if [ "$1" != "" ]; then
        emacs23 -q --load `pwd`/logalimacs_configs.el
    else
        emacs -q --load `pwd`/logalimacs_configs.el
    fi
}

bootemacs
