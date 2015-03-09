#!/bin/bash

# Example
# ./bootstrap datasmart 0.0.1 172.17.0.5 datasmart 4421 /app Basic /home/core/share/src/datasmart_erl

# ------------------------------------------
# Build the Dockerfile
# ------------------------------------------
cd $8
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
: ${SERVICE_NAME=$1}
: ${SERVICE_VERSION=$2}
: ${SERVICE_IP=$3} # 172.17.0.5
: ${SERVICE_HOST=$4} # datasmart
: ${SERVICE_PORT=$5}
: ${SERVICE_VOLUME=$6}
: ${SERVICE_AUTH_METHOD=$7}

# ------------------------------------------
# Bootstrap Container
# ------------------------------------------
: ${IMAGE_NAME="erlang"}
#: ${LINK_REDIS="--link redis-1:redis"}
#: ${LINK_COUCHDB="--link couchdb-1:couch"}
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

# : ${TARGET_HOST="www....com"}
# : ${DOCKER_TARGET_HOST="-h "${TARGET_HOST}}
# : ${DNS="10.0.0.10"}
# : ${DOCKER_DNS="--dns="${DNS}}
# : ${DNS_SEARCH="--dns-search=dev.org"}
# : ${DOCKER_DNS_SEARCH="--dns-search="${DNS_SEARCH}}

docker run \
    ${LINK_REDIS} ${LINK_COUCHDB} ${ETCD_HOST} \
    ${ETCD_PORT} ${ETCD_ADDR} ${NAME} \
    ${VERSION} ${HOST} ${ENV_PORT} \
    ${ENV_AUTH_METHOD} ${ERLANG_COOKIE} \
    ${EXPORT_PORT} ${CONTAINER_NAME} ${IMAGE_NAME}

## ------------------------------------------
## Service JSON
## ------------------------------------------
#SERVICE_JSON=$(cat <<-}
#    {
#        "service": "${SERVICE_NAME}",
#        "version": "${SERVICE_VERSION}",
#        "ip": "${SERVICE_IP}",
#        "host": "${SERVICE_HOST}",
#        "port": "${SERVICE_PORT}",
#        "volume": "${SERVICE_VOLUME}",
#        "auth_method": "${SERVICE_AUTH_METHOD}",
#        "cid": "${SERVICE_CID}"
#    }
#)
#
## ------------------------------------------
## Register Service
## ------------------------------------------
#curl --silent -L -X PUT \
#    "http://${ETCD_HOST}:4001/v2/keys/services/${SERVICE_NAME}" \
#    -d value=$SERVICE_JSON
