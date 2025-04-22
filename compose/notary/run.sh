#!/bin/sh

if [ -z "$SEED" ]; then
    echo Must set the secret SEED variable""
    exit 1
fi

check_status() {
    while true; do
	response=$( wget -qO - "$1" 2>/dev/null)
	if [ -n "$response" ]; then
	    return 0
	else
            sleep 1
	fi
    done
}

check_status "${CRYPTOGRAPHY}/signature/key/${SEED}"

routine() {
    check_status "localhost/"

    while true; do
        start_time=$(date +%s)
	response=$( wget -qO - localhost/interface --post-data "(step \"$SEED\")" )
	echo $response >> steps.log
        next_time=$((start_time + $(( 2 ** $PERIODICITY ))))
        current_time=$(date +%s)
        sleep_duration=$((next_time - current_time))
        if [ $sleep_duration -gt 0 ]; then
            sleep $sleep_duration
        fi
    done
}

routine &

./journal-sdk -p 80 --database db --boot "($( cat source.scm ) \"$SEED\" $PERIODICITY \"$CRYPTOGRAPHY\" $REST)"
