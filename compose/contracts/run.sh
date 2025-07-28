#!/bin/sh

if [ -z "$SECRET" ]; then
    echo Must set the SECRET variable""
    exit 1
fi

record=$( cat record.scm )
control=$( cat control.scm )
ledger=$( cat ledger.scm )
contracts=$( cat contracts.scm )

check_status() {
    while true; do
        echo "Polling for cryptography service"
	    response=$( wget -qO - "$1" 2>/dev/null)
	    if [ -n "$response" ]; then
	        return 0
	    else
            sleep 1
	    fi
    done
}

check_status "${CRYPTOGRAPHY}/signature/key/deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"

./journal-sdk -b "($record \"$SECRET\" $control ($ledger \"$CRYPTOGRAPHY\" #t #f) ($contracts 40 2))" -s "(*step* \"$SECRET\")" -p 80 -c $PERIODICITY
