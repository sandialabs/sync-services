FROM ghcr.io/sandialabs/sync-journal/journal-sdk:1.0.2

ARG REPOSITORY=https://raw.githubusercontent.com/sandialabs/sync-records/c2f26c0087846043da045f0d55b6d10bd787e0bb/lisp/

RUN wget $REPOSITORY/record.scm
RUN wget $REPOSITORY/control.scm
RUN wget $REPOSITORY/ledger.scm
RUN wget $REPOSITORY/ontology.scm

COPY run.sh .

ENTRYPOINT ["./run.sh"]