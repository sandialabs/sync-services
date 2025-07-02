FROM ghcr.io/sandialabs/sync-journal/journal-sdk:1.0.2

ARG REPOSITORY=https://raw.githubusercontent.com/sandialabs/sync-records/4f962860983af5529f56a11e60d21182697907e6/lisp/

RUN wget $REPOSITORY/record.scm
RUN wget $REPOSITORY/control.scm
RUN wget $REPOSITORY/ledger.scm
RUN wget $REPOSITORY/ontology.scm

COPY run.sh .

ENTRYPOINT ["./run.sh"]