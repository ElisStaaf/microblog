CLISP_DIR="$HOME/common-lisp"

mklib() {
    if [ -d $2 ]; then
        rm -rf $2
    fi
    git clone https://github.com/$1 $2
}

mklib \
    "ElisStaaf/microblog" \
    "$HOME/common-lisp/microblog"
