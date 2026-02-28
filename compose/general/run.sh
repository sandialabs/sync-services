#!/bin/sh

if [ -z "$SECRET" ]; then
    echo Must set the SECRET variable""
    exit 1
fi

if [ -z "$WINDOW" ]; then
    WINDOW="#f"
fi

resolve_lisp_file() {
    filename="$1"

    if [ -n "$LISP_DIR" ]; then
        path="$LISP_DIR/$filename"
        if [ ! -f "$path" ]; then
            echo "Missing required Lisp file: $path"
            exit 1
        fi
        echo "$path"
        return 0
    fi

    if [ ! -f "$filename" ]; then
        echo "Missing required Lisp file: $filename"
        exit 1
    fi
    echo "$filename"
}

control=$( cat "$(resolve_lisp_file control.scm)" )
standard=$( cat "$(resolve_lisp_file standard.scm)" )
chain=$( cat "$(resolve_lisp_file log-chain.scm)" )
tree=$( cat "$(resolve_lisp_file tree.scm)" )
configuration=$( cat "$(resolve_lisp_file configuration.scm)" )
ledger=$( cat "$(resolve_lisp_file ledger.scm)" )

interface=$( cat interface.scm )

boot="($interface '$SECRET '$SECRET $WINDOW $control '$standard '$chain '$tree '$configuration '$ledger)"
step="((function *step*) (authentication $SECRET))"

RUST_LOG=$RUST_LOG ./journal-sdk -b "$boot" -p 80 -c $PERIOD -s "$step" -d database
