FROM ghcr.io/sandialabs/sync-journal/journal-sdk:1.0.1

ENV REPOSITORY=$

RUN wget $REPOSITORY/record.scm
RUN wget $REPOSITORY/control.scm
RUN wget $REPOSITORY/ledger.scm

COPY run.sh .

CMD ./run.sh