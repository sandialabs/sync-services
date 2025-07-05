FROM ghcr.io/sandialabs/sync-journal/journal-sdk:1.0.3

ARG REPOSITORY=https://raw.githubusercontent.com/sandialabs/sync-records/28818b41c1660db0eb81b05620972f7220b20b1b/lisp/

RUN wget $REPOSITORY/record.scm
RUN wget $REPOSITORY/control.scm
RUN wget $REPOSITORY/ledger.scm

COPY run.sh .

ENTRYPOINT ["./run.sh"]