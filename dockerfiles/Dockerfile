# Inherit from Heroku's stack
FROM heroku/heroku:18-build

ENV LANG en_US.UTF-8
# Stack stores binaries in /root/.local/bin
ENV PATH /root/.local/bin:$PATH

# Heroku assumes we'll put everything in /app/user
RUN mkdir -p /app/user
WORKDIR /app/user

ARG STACK_VERSION=1.9.3
RUN wget -qO- https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/stack-$STACK_VERSION-linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C /usr/local/bin '*/stack'

ADD . /app/user
RUN cd /app/user/haskell-api-test; stack build --dependencies-only;
