#!/bin/sh

if [ -z "$SECRET" ]; then
    echo Must set the SECRET variable""
    exit 1
fi

if [ -z "$WINDOW" ]; then
    WINDOW="#f"
fi

control=$( cat control.scm )
standard=$( cat standard.scm )
chain=$( cat log-chain.scm )
tree=$( cat tree.scm )
configuration=$( cat configuration.scm )
ledger=$( cat ledger.scm )

interface=$( cat interface.scm )

boot="($interface '$SECRET '$SECRET $WINDOW $control $standard '$chain '$tree '$configuration '$ledger)"
step="((function *step*) (arguments ()) (authentication $SECRET))"

RUST_LOG=$RUST_LOG ./journal-sdk -b "$boot" -p $PORT -c $PERIODICITY -s "$step" -d database
