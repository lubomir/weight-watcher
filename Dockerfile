FROM heroku/cedar:14

ENV LANG C.UTF-8

RUN wget -q -O- https://s3.amazonaws.com/download.fpcomplete.com/ubuntu/fpco.key | apt-key add - && echo 'deb http://download.fpcomplete.com/ubuntu/utopic stable main'| tee /etc/apt/sources.list.d/fpco.list

RUN apt-get update && apt-get install stack -y

RUN useradd -d /app -m app
USER app
WORKDIR app

ENV HOME /app
ENV PORT 3000

RUN mkdir -p /app/heroku /app/src /app/.profile.d

WORKDIR /app/src

COPY stack.yaml stack.yaml
COPY LICENSE LICENSE

RUN stack setup

COPY weight-watcher.cabal weight-watcher.cabal

RUN stack build --only-dependencies

COPY src src
COPY js js

RUN stack build
