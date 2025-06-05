FROM python:3.11.9-alpine3.19

ARG pip_args

WORKDIR /srv

RUN apk update
RUN apk --no-cache add tini bash samba tzdata shadow fuse-dev
ENV FUSE_LIBRARY_PATH=/usr/lib/libfuse.so

RUN addgroup -S smb
RUN rm -f /etc/samba/smb.conf && rm -rf /tmp/* /var/cache/apk/*

COPY --chmod=755 run.sh /usr/bin/run.sh
COPY --chmod=664 smb.conf /etc/samba/smb.default

RUN mkdir data-front
VOLUME /srv/data-back
EXPOSE 139 445

ENV SAMBA_USERNAME="samba"
ENV SAMBA_SECRET="secret"
ENV SAMBA_SHARE="data"

ENV UID=1000
ENV GID=1000
ENV RW=true

HEALTHCHECK --interval=60s --timeout=15s CMD smbclient --configfile=/etc/samba.conf -L \\localhost -U % -m SMB3

COPY requirements.txt .
RUN pip install -r requirements.txt
COPY service.py .

RUN echo "user_allow_other" >> /etc/fuse.conf

ENTRYPOINT ["/sbin/tini", "--", "/usr/bin/run.sh"]