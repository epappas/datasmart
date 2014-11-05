EVAL ?= "application:start(rest_store)."
START ?= rest_store
NODE ?= datastore
HOSTNAME ?= `uname -n`
REBAR ?= "./rebar"
CONFIG ?= "datasmart.config"
RUN := erl -pa lib/*/ebin -pa deps/*/ebin -smp enable -boot start_sasl -config ${CONFIG} ${ERL_ARGS}

EVAL_IGNITION = -eval ${EVAL};
START_IGNITION = -s ${START}
IGNITION = ${START_IGNITION}

all:
	${REBAR} get-deps compile

quick:
	${REBAR} skip_deps=true compile

clean:
	${REBAR} clean

quick_clean:
	${REBAR} skip_deps=true clean

package:
	${REBAR} get-deps compile generate

run: quick
	if [ -n "${NODE}" ]; then ${RUN} -name ${NODE}@${HOSTNAME} ${IGNITION}; \
	else ${RUN} ${IGNITION}; \
	fi

