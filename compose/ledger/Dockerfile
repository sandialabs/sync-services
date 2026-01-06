FROM ghcr.io/sandialabs/sync-journal/journal-sdk:1.0.4

ARG REPOSITORY=https://raw.githubusercontent.com/sandialabs/sync-records/a54c1cbee4d266a044430edf4b936bb770c60a69/lisp/

RUN wget $REPOSITORY/record.scm
RUN wget $REPOSITORY/control.scm
RUN wget $REPOSITORY/ledger.scm

COPY run.sh .

ENTRYPOINT ["./run.sh"]