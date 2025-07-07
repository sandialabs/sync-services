FROM ghcr.io/sandialabs/sync-journal/journal-sdk:1.0.1

ARG REPOSITORY=https://raw.githubusercontent.com/sandialabs/sync-records/965c839ac6bed6db24541ba642089e8fa8063633/lisp/

# RUN wget $REPOSITORY/record.scm
# RUN wget $REPOSITORY/control.scm
# RUN wget $REPOSITORY/ledger.scm

COPY record.scm .
COPY control.scm .
COPY ledger.scm .
COPY contracts.scm .

COPY run.sh .

CMD ./run.sh