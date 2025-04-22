# --- Build ---

FROM alpine:latest AS builder

ENV CC=clang
ENV CXX=clang++

# Install OS dependencies
RUN apk update
RUN apk add cargo
RUN apk add clang

# Build SDK
WORKDIR /srv
COPY . .
RUN cargo build --release

# --- Deploy ---

FROM alpine:latest

WORKDIR /srv
COPY --from=builder /usr/lib/libgcc_s.so.1 /usr/lib/
COPY --from=builder /srv/target/release/cryptography-service .

CMD ./cryptography-service -p 80