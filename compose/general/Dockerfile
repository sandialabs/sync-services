FROM ghcr.io/sandialabs/sync-journal/journal-sdk:1.0.5

ARG REPOSITORY=https://raw.githubusercontent.com/sandialabs/sync-records/bb412a24b18ec2f287922b10e633f9853adbeab1/lisp/

RUN wget $REPOSITORY/control.scm
RUN wget $REPOSITORY/standard.scm
RUN wget $REPOSITORY/log-chain.scm
RUN wget $REPOSITORY/tree.scm
RUN wget $REPOSITORY/configuration.scm
RUN wget $REPOSITORY/ledger.scm

COPY run.sh .

COPY interface.scm .

ENTRYPOINT ["./run.sh"]