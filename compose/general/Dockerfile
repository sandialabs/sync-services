FROM ghcr.io/sandialabs/sync-journal/journal-sdk:1.0.5

ARG REPOSITORY=https://raw.githubusercontent.com/sandialabs/sync-records/9e335b400a2d8c0c0ead220f6f3cccbbd9b09a3c/lisp/

RUN wget $REPOSITORY/control.scm
RUN wget $REPOSITORY/standard.scm
RUN wget $REPOSITORY/log-chain.scm
RUN wget $REPOSITORY/tree.scm
RUN wget $REPOSITORY/configuration.scm
RUN wget $REPOSITORY/ledger.scm

COPY run.sh .

COPY interface.scm .

ENTRYPOINT ["./run.sh"]