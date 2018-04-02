FROM debian:9.3-slim AS build

  # Install build dependencies.
  RUN apt-get update && apt-get install --assume-yes gcc libgmp-dev make netbase wget xz-utils zlib1g-dev

  # Install Stack.
  WORKDIR /root/stack
  ARG STACK_VERSION=1.6.5
  RUN wget https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/stack-$STACK_VERSION-linux-x86_64.tar.gz
  RUN tar --extract --file stack-$STACK_VERSION-linux-x86_64.tar.gz
  RUN cp stack-$STACK_VERSION-linux-x86_64/stack /usr/local/bin/stack

  # Install GHC.
  WORKDIR /root/semabadge
  COPY stack.yaml .
  RUN stack setup

  # Install Haskell dependencies.
  COPY package.yaml .
  RUN stack build --only-dependencies

  # Build Semabadge.
  COPY . .
  RUN stack build --copy-bins

FROM debian:9.3-slim

  # Install runtime dependencies.
  RUN apt-get update && apt-get install --assume-yes ca-certificates libgmp-dev netbase

  # Install executable.
  COPY --from=build /root/.local/bin/semabadge /usr/local/bin/semabadge
  EXPOSE 80
  ENV PORT=80
  CMD semabadge
