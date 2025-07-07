#!/bin/sh

SECRET=password
CRYPTOGRAPHY=http://cryptography.docker

record=$( cat record.scm )
control=$( cat control.scm )
ledger=$( cat ledger.scm )
contracts=$( cat contracts.scm )

script="($record \"$SECRET\" $control $contracts)"
echo $script > test.txt
curl -X POST localhost/.interface --data-binary "$script"
 