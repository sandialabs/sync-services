FROM ghcr.io/sandialabs/sync-journal/journal-sdk:1.1.1

ARG LISP_REPOSITORY=https://raw.githubusercontent.com/sandialabs/sync-records/8eba71118f6007798be725fc8417ed73d46d5388/lisp/
ARG CUSTOM_SETUP=""

RUN set -eu; \
    if [ -n "$CUSTOM_SETUP" ]; then \
      echo "$CUSTOM_SETUP" | base64 -d > /tmp/custom-setup.sh; \
      chmod +x /tmp/custom-setup.sh; \
      /bin/sh -eu /tmp/custom-setup.sh; \
      rm -f /tmp/custom-setup.sh; \
    fi

RUN wget "${LISP_REPOSITORY}/control.scm"
RUN wget "${LISP_REPOSITORY}/standard.scm"
RUN wget "${LISP_REPOSITORY}/log-chain.scm"
RUN wget "${LISP_REPOSITORY}/linear-chain.scm"
RUN wget "${LISP_REPOSITORY}/tree.scm"
RUN wget "${LISP_REPOSITORY}/configuration.scm"
RUN wget "${LISP_REPOSITORY}/ledger.scm"

COPY run.sh .

COPY interface.scm .

ENTRYPOINT ["./run.sh"]
