FROM ghcr.io/sandialabs/sync-journal/journal-sdk:1.0.4

ARG REPOSITORY=https://raw.githubusercontent.com/sandialabs/sync-records/b4c2cdc31f03b5a640ec9690b51c2b672d23c62d/lisp/

RUN wget --no-check-certificate $REPOSITORY/control.scm
RUN wget --no-check-certificate $REPOSITORY/standard.scm
RUN wget --no-check-certificate $REPOSITORY/log-chain.scm
RUN wget --no-check-certificate $REPOSITORY/tree.scm
RUN wget --no-check-certificate $REPOSITORY/configuration.scm
RUN wget --no-check-certificate $REPOSITORY/ledger.scm

COPY run.sh .

COPY interface.scm .

ENTRYPOINT ["./run.sh"]