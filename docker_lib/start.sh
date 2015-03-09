#!/bin/bash

# ------------------------------------------
# Enviroment Values
# ------------------------------------------
# ${ETCD_HOST}          # Set by bootstrap.sh, default: 172.17.0.5
# ${ETCD_PORT}          # Set by bootstrap.sh, default: 4001
# ${ETCD_ADDR}          # Set by bootstrap.sh, default: http://172.17.0.5:4001
# ${HOSTNAME}           # Set by Docker
# ${NAME}               # Set by bootstrap.sh
# ${VERSION}            # Set by bootstrap.sh
# ${HOST}               # Set by bootstrap.sh, default: 172.17.0.5
# ${ENV_PORT}           # Set by bootstrap.sh
# ${ENV_VOLUME}         # Set by Dockerfile, default: "/app"
# ${ENV_AUTH_METHOD}    # Set by bootstrap.sh, default: ""
# ${ERLANG_COOKIE}      # Set by bootstrap.sh, default: `openssl rand -hex 32`

# ------------------------------------------
# Enviroment Dependencies
# ------------------------------------------
# --link redis-1:redis
# ${REDIS_NAME}                 # Set by docker link
# ${REDIS_PORT}                 # Set by docker link
# ${REDIS_PORT_6379_TCP}        # Set by docker link
# ${REDIS_PORT_6379_TCP_ADDR}   # Set by docker link
# ${REDIS_PORT_6379_TCP_PORT}   # Set by docker link
# ${REDIS_PORT_6379_TCP_PROTO}  # Set by docker link

: ${REDIS_ADDR=${REDIS_PORT_6379_TCP_ADDR}:${REDIS_PORT_6379_TCP_PORT}}
: ${REDIS_TCP_PORT=${REDIS_PORT_6379_TCP_PORT}}

# --link couchdb-1:couch
# ${COUCH_NAME}                 # Set by docker link
# ${COUCH_PORT}                 # Set by docker link
# ${COUCH_PORT_5984_TCP}        # Set by docker link
# ${COUCH_PORT_5984_TCP_ADDR}   # Set by docker link
# ${COUCH_PORT_5984_TCP_PORT}   # Set by docker link
# ${COUCH_PORT_5984_TCP_PROTO}  # Set by docker link

: ${COUCH_ADDR=${COUCH_PORT_5984_TCP_ADDR}:${COUCH_PORT_5984_TCP_PORT}}
: ${COUCH_URL="http://"${COUCH_ADDR}}
: ${COUCH_TCP_PORT=${COUCH_PORT_5984_TCP_PORT}}

# ------------------------------------------
# SET Service Values
# ------------------------------------------
: ${SERVICE_NAME=${NAME}}
: ${SERVICE_VERSION=${VERSION}}
: ${SERVICE_HOST=${HOST}}
: ${SERVICE_PORT=${ENV_PORT}}
: ${SERVICE_VOLUME=${ENV_VOLUME}}
: ${SERVICE_AUTH_METHOD=${ENV_AUTH_METHOD}}
: ${SERVICE_ERLANG_COOKIE=${ERLANG_COOKIE}}
: ${SERVICE_CID=${HOSTNAME}}

# ------------------------------------------
# EXPORTS
# ------------------------------------------
export SERVICE_NAME
export SERVICE_VERSION
export SERVICE_HOST
export SERVICE_PORT
export SERVICE_VOLUME
export SERVICE_AUTH_METHOD
export SERVICE_ERLANG_COOKIE
export SERVICE_CID

export COUCH_ADDR
export COUCH_URL
export COUCH_TCP_PORT

export REDIS_ADDR
export REDIS_TCP_PORT

# ------------------------------------------
# Service JSON
# ------------------------------------------
#SERVICE_JSON=$(cat <<-}
#    {
#        "service": "${SERVICE_NAME}",
#        "version": "${SERVICE_VERSION}",
#        "ip": "${SERVICE_IP}",
#        "host": "${SERVICE_HOST}",
#        "port": "${SERVICE_PORT}",
#        "volume": "${SERVICE_VOLUME}",
#        "auth_method": "${SERVICE_AUTH_METHOD}",
#        "cookie": "${SERVICE_ERLANG_COOKIE}",
#        "cid": "${SERVICE_CID}"
#    }
#)

# ------------------------------------------
# Bootstrap
# ------------------------------------------
# cd rest_store
# bin/rest_store start
# bin/rest_store getpid # TODO
