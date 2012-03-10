ERL  		= ERL_LIBS=deps erl +A 4 +K true -pa ebin
NAME 	 	= proxylocal
RUN_OPTS	= -boot start_sasl -s proxylocal -config priv/app.config -sname ${NAME}
CTL_OPTS	= ${ERL} -sname ctl-${NAME} -noinput -hidden -s proxylocal ctl -extra ${NAME}@`hostname -s`

all: compile

compile:
	./rebar compile

eunit:
	./rebar skip_deps=true eunit

clean:
	./rebar clean

distclean: clean
	rm -rf deps/*

run: compile
	${ERL} ${RUN_OPTS}

runit:
	${ERL} -noinput ${RUN_OPTS}

stop:
	${ERL} ${CTL_OPTS} stop

reload:
	${ERL} ${CTL_OPTS} reload

status:
	${ERL} ${CTL_OPTS} status

shell:
	${ERL} -sname shell-${NAME} -hidden -remsh ${NAME}@`hostname -s`
