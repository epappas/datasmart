#!/bin/sh

# ------------------------------------------
# Build the Dockerfile
# ------------------------------------------
cd /home/core/share/src/datasmart_erl
docker build -rm=true -t erlang .

# ------------------------------------------
# Enviroment Values
# ------------------------------------------
# ${ETCD_HOST}      # Set by Vagrant
# $1                # Name,     Set by User shell
# $2                # Version,  Set by User shell
# $3                # ip,       Set by User shell
# $4                # Host,     Set by User shell
# $5                # Port,     Set by User shell
# $6                # Volume,   Set by User shell
# $7                # AUTH,     Set by User shell
# $8                # CWD,      Set by User shell

# ------------------------------------------
# SET Service Values
# ------------------------------------------
: ${SERVICE_NAME=datasmart}
: ${SERVICE_VERSION=0.0.1}
: ${SERVICE_IP="172.17.0.5"} # 172.17.0.5
: ${SERVICE_HOST=datasmart} # datasmart
: ${SERVICE_PORT=4421}
: ${SERVICE_VOLUME=/app}
: ${SERVICE_AUTH_METHOD=Basic}

# ------------------------------------------
# Bootstrap Container
# ------------------------------------------
: ${IMAGE_NAME="erlang"}
: ${LINK_REDIS="--link redis-1:redis"}
: ${LINK_COUCHDB="--link couchdb-1:couch"}
: ${CONTAINER_NAME="--name datasmart_erl"}
: ${ETCD_HOST="--env ETCD_HOST=172.17.0.5"}
: ${ETCD_PORT="--env ETCD_PORT=4001"}
: ${ETCD_ADDR="--env ETCD_ADDR=http://172.17.0.5:4001"}
: ${NAME="--env NAME="${SERVICE_NAME}}
: ${VERSION="--env VERSION="${SERVICE_VERSION}}
: ${HOST="--env HOST="${SERVICE_IP}}
: ${ENV_PORT="--env ENV_PORT="${SERVICE_PORT}}
: ${ENV_AUTH_METHOD="--env ENV_AUTH_METHOD=TOKEN"}
: ${ERLANG_COOKIE="--env ERLANG_COOKIE=cbeba4f3eced9de05baa49f6726248946362f51b0da449fe1905c74a2710d78f"} #`openssl rand -hex 32`"}
: ${EXPORT_PORT="-p "${SERVICE_PORT}:${SERVICE_PORT}}

: ${SERVICE_CID=`docker run \
    ${LINK_REDIS} ${LINK_COUCHDB} ${ETCD_HOST} \
    ${ETCD_PORT} ${ETCD_ADDR} ${NAME} \
    ${VERSION} ${HOST} ${ENV_PORT} \
    ${ENV_AUTH_METHOD} ${ERLANG_COOKIE} \
    ${EXPORT_PORT} ${CONTAINER_NAME} \
    --rm -i -t ${IMAGE_NAME} /bin/bash`
}