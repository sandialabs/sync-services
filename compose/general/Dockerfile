FROM ghcr.io/sandialabs/sync-journal/journal-sdk:1.0.5

ARG REPOSITORY=https://raw.githubusercontent.com/sandialabs/sync-records/2d846aa57a31af85c6f84efd26b2129f7681b05a/lisp/

RUN wget $REPOSITORY/control.scm
RUN wget $REPOSITORY/standard.scm
RUN wget $REPOSITORY/log-chain.scm
RUN wget $REPOSITORY/tree.scm
RUN wget $REPOSITORY/configuration.scm
RUN wget $REPOSITORY/ledger.scm

COPY run.sh .

COPY interface.scm .

ENTRYPOINT ["./run.sh"]