#!/bin/sh

if [ -z "$SECRET" ]; then
    echo Must set the SECRET variable""
    exit 1
fi

if [ -z "$WINDOW" ]; then
    WINDOW="#f"
fi

record=$( cat record.scm )
control=$( cat control.scm )
ledger=$( cat ledger.scm )

RUST_LOG=INFO ./journal-sdk -b "($record \"$SECRET\" $control ($ledger \"$SECRET\" #f $WINDOW))" -s "(*step* \"$SECRET\")" -p 80 -c $PERIODICITY -d database
