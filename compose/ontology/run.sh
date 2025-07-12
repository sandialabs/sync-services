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
ontology=$( cat ontology.scm )

RUST_LOG=info ./journal-sdk -b "($record \"$SECRET\" $control ($ledger \"$SECRET\" #f $WINDOW) $ontology)" -s "(*step* \"$SECRET\")" -p 80 -c $PERIODICITY -d database
