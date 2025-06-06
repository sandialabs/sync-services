FROM ghcr.io/sandialabs/sync-journal/journal-sdk:1.0.1

ARG REPOSITORY=https://raw.githubusercontent.com/sandialabs/sync-records/6e8593d301d360a64413c3333e633dbb70863796/lisp/

RUN wget $REPOSITORY/record.scm
RUN wget $REPOSITORY/control.scm
RUN wget $REPOSITORY/ledger.scm

COPY run.sh .

CMD ./run.sh