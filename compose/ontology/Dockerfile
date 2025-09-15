FROM ghcr.io/sandialabs/sync-journal/journal-sdk:1.0.3

ARG REPOSITORY=https://raw.githubusercontent.com/sandialabs/sync-records/2ccedf806cbd75ef9e30b46c2c02138c77568dcb/lisp/

RUN wget $REPOSITORY/record.scm
RUN wget $REPOSITORY/control.scm
RUN wget $REPOSITORY/ledger.scm
RUN wget $REPOSITORY/ontology.scm

COPY run.sh .

ENTRYPOINT ["./run.sh"]