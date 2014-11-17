FROM ubuntu:trusty

MAINTAINER Evangelos Pappas <epappas@evalonlabs.com>

RUN apt-get update && apt-get -y upgrade && apt-get -y install wget git openssl

RUN cd /tmp; wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb && \
    dpkg -i erlang-solutions_1.0_all.deb

RUN apt-get update && apt-get -y install erlang erlang-base-hipe build-essential

#Configuration
ADD . /app

RUN cd /app; make clean

RUN cd /app; make package

RUN cd /app/rest_store

EXPOSE 4421

CMD ["/app/bin/rest_store", "start"]
