FROM ghcr.io/sandialabs/sync-journal/journal-sdk:1.2.0

ARG LISP_REPOSITORY=https://raw.githubusercontent.com/sandialabs/sync-records/e40403783592c09ffa31a7af63d2838b72bfcc31/lisp/
ARG CUSTOM_SETUP=""

RUN set -eu; \
    if [ -n "$CUSTOM_SETUP" ]; then \
      echo "$CUSTOM_SETUP" | base64 -d > /tmp/custom-setup.sh; \
      chmod +x /tmp/custom-setup.sh; \
      /bin/sh -eu /tmp/custom-setup.sh; \
      rm -f /tmp/custom-setup.sh; \
    fi

RUN wget "${LISP_REPOSITORY}/root.scm"
RUN wget "${LISP_REPOSITORY}/standard.scm"
RUN wget "${LISP_REPOSITORY}/log-chain.scm"
RUN wget "${LISP_REPOSITORY}/linear-chain.scm"
RUN wget "${LISP_REPOSITORY}/tree.scm"
RUN wget "${LISP_REPOSITORY}/ledger.scm"
RUN wget "${LISP_REPOSITORY}/interface.scm"

COPY run.sh .

ENTRYPOINT ["./run.sh"]
