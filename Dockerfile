FROM openjdk:8u121-jdk-alpine as BUILD

#Used to download and install sbt
RUN apk add --no-cache curl tar bash

##
## Setup sbt
##

ARG SBT_VERSION=1.1.1
ARG SBT_BASEURL=https://github.com/sbt/sbt/releases/download/v${SBT_VERSION}

RUN mkdir -p /usr/share/sbt \
  && curl -fsSL -o /tmp/sbt.tgz ${SBT_BASEURL}/sbt-${SBT_VERSION}.tgz \
  && tar -xzf /tmp/sbt.tgz -C /usr/share/sbt \
  && rm -f /tmp/sbt.tgz \
  && ln -s /usr/share/sbt/sbt/bin/sbt /usr/bin/sbt

WORKDIR /usr/src
RUN sbt compile && sbt test
COPY . .
RUN sbt universal:packageZipTarball

#Launcher
FROM openjdk:8u121-jdk-alpine
RUN apk add --no-cache tar bash

WORKDIR /app
COPY --from=BUILD /usr/src/target/universal/liability_proof-0.0.1.tgz .
RUN tar -xzf liability_proof-0.0.1.tgz

# passes the host system java options to this image entrypoint
ENV JAVA_OPTS=

ENTRYPOINT ./liability_proof-0.0.1/bin/liability_proof
