#!/bin/sh

if [ -z "$SECRET" ]; then
    echo Must set the SECRET variable""
    exit 1
fi

record=$( cat record.scm )
control=$( cat control.scm )
ledger=$( cat ledger.scm )

./journal-sdk -b "($record \"$SECRET\" $control ($ledger #t #f))" -s "(*step* \"$SECRET\")" -p 80 -c $PERIODICITY
