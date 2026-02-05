FROM ghcr.io/sandialabs/sync-journal/journal-sdk:1.0.4

ENV HTTP_PROXY=http://proxy.sandia.gov:80
ENV HTTPS_PROXY=http://proxy.sandia.gov:80

ARG REPOSITORY=https://raw.githubusercontent.com/sandialabs/sync-records/4192f8b055c3f2d4d8438cd7a577ed4e853a334e/lisp/

RUN wget --no-check-certificate $REPOSITORY/control.scm
RUN wget --no-check-certificate $REPOSITORY/standard.scm
RUN wget --no-check-certificate $REPOSITORY/log-chain.scm
RUN wget --no-check-certificate $REPOSITORY/tree.scm
RUN wget --no-check-certificate $REPOSITORY/configuration.scm
RUN wget --no-check-certificate $REPOSITORY/ledger.scm

COPY run.sh .

COPY interface.scm .

ENTRYPOINT ["./run.sh"]