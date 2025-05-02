FROM ghcr.io/sandialabs/sync-journal/journal-sdk:1.0.1

RUN wget https://raw.githubusercontent.com/sandialabs/sync-records/refs/heads/main/lisp/record.scm
RUN wget https://raw.githubusercontent.com/sandialabs/sync-records/refs/heads/main/lisp/control.scm
RUN wget https://raw.githubusercontent.com/sandialabs/sync-records/refs/heads/main/lisp/ledger.scm

COPY run.sh .

CMD ./run.sh