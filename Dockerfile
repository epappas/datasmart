FROM ubuntu:trusty

MAINTAINER Evangelos Pappas <epappas@evalonlabs.com>

# ------------------------------------------
# Download Dependencies of the enviroment
# ------------------------------------------
RUN apt-get update && apt-get -y upgrade && \
    apt-get -y install wget git openssl

# ------------------------------------------
# Download Erlang
# ------------------------------------------
RUN cd /tmp; wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb && \
    dpkg -i erlang-solutions_1.0_all.deb

# ------------------------------------------
# Install Erlang
# ------------------------------------------
RUN apt-get update && apt-get -y install \
    erlang erlang-base-hipe build-essential

# ------------------------------------------
# Set Volume
# ------------------------------------------
ADD . /app

# ------------------------------------------
# Environment Variables
# ------------------------------------------
ENV ENV_VOLUME /app

# ------------------------------------------
# Set up the Environment
# ------------------------------------------
RUN cd /app; make clean; make all; make package

RUN cd /app; chmod +x /app/start.sh
RUN cd /app; /app/start.sh

# ------------------------------------------
# Expose Ports
# ------------------------------------------
EXPOSE 4421

# ------------------------------------------
# Run The Service
# ------------------------------------------
CMD ["/app/rest_store/bin/rest_store", "start"]
