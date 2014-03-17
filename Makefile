REBAR = $(shell pwd)/rebar
GIT = git
DEPS = deps

.PHONY: deps

all: deps compile

compile:
	$(REBAR) compile

deps:
	@(cp -rf lib/amqp_client $(DEPS))	
	@(cp -rf lib/rabbit_common $(DEPS))
	$(REBAR) get-deps

#rabbit_deps:
#	cd lib && wget -c https://github.com/rabbitmq/rabbitmq-public-umbrella/archive/rabbitmq_v3_2_4.zip && \
#	mv -f rabbitmq_v3_2_4 rabbitmq_v3_2_4.zip && \
#	unzip -qo rabbitmq_v3_2_4.zip && \
#        cd rabbitmq-public-umbrella-rabbitmq_v3_2_4  && make release && \
#	cp -rf rabbitmq-erlang-client/dist/* ..
#	$(REBAR) get-deps

clean:
	$(REBAR) clean

distclean: clean devclean relclean
	$(REBAR) delete-deps

rel: all
	$(REBAR) generate

genrel: 
	$(REBAR) generate

relclean:
	rm -rf rel/sqor_msg_consumer

xref: all
	$(REBAR) skip_deps=true xref

stage : rel
	$(foreach dep,$(wildcard deps/*), rm -rf rel/sqor_msg_consumer/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) rel/sqor_msg_consumer/lib;)
	$(foreach app,$(wildcard apps/*), rm -rf rel/sqor_msg_consumer/lib/$(shell basename $(app))-* && ln -sf $(abspath $(app)) rel/sqor_msg_consumer/lib;)


##
## Developer targets
##
##  devN - Make a dev build for node N
##  stagedevN - Make a stage dev build for node N (symlink libraries)
##  devrel - Make a dev build for 1..$DEVNODES
##  stagedevrel Make a stagedev build for 1..$DEVNODES
##
##  Example, make a 68 node devrel cluster
##    make stagedevrel DEVNODES=68

.PHONY : stagedevrel devrel
DEVNODES ?= 4

# 'seq' is not available on all *BSD, so using an alternate in awk
SEQ = $(shell awk 'BEGIN { for (i = 1; i < '$(DEVNODES)'; i++) printf("%i ", i); print i ;exit(0);}')

$(eval stagedevrel : $(foreach n,$(SEQ),stagedev$(n)))
$(eval devrel : $(foreach n,$(SEQ),dev$(n)))

dev% : all
	mkdir -p dev
	rel/gen_dev $@ rel/vars/dev_vars.config.src rel/vars/$@_vars.config
	(cd rel && $(REBAR) generate target_dir=../dev/$@ overlay_vars=vars/$@_vars.config)

stagedev% : dev%
	  $(foreach dep,$(wildcard deps/*), rm -rf dev/$^/lib/$(shell basename $(dep))* && ln -sf $(abspath $(dep)) dev/$^/lib;)
	  $(foreach app,$(wildcard apps/*), rm -rf dev/$^/lib/$(shell basename $(app))* && ln -sf $(abspath $(app)) dev/$^/lib;)

devclean: clean
	rm -rf dev
