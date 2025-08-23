FROM ghcr.io/sandialabs/sync-journal/journal-sdk:1.0.3

ARG REPOSITORY=https://raw.githubusercontent.com/sandialabs/sync-records/965c839ac6bed6db24541ba642089e8fa8063633/lisp/

RUN wget $REPOSITORY/record.scm
RUN wget $REPOSITORY/control.scm
RUN wget $REPOSITORY/ledger.scm

COPY run.sh file-system.scm .

COPY run.sh .

CMD ./run.sh