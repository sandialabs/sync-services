FROM ghcr.io/sandialabs/sync-journal/journal-sdk:1.0.1

ARG REPOSITORY=https://raw.githubusercontent.com/sandialabs/sync-records/0cf4c834fa37ca84b837e5a4a594dd16f09771a8/lisp/

RUN wget $REPOSITORY/record.scm
RUN wget $REPOSITORY/control.scm
RUN wget $REPOSITORY/ledger.scm

COPY run.sh .

CMD ./run.sh