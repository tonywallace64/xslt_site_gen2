.PHONY: compile clean

compile: ebin/static_world_app.beam ebin/static_world_sup.beam ebin/static_world.app
ebin/static_world.app: src/static_world.app.src
	cp src/static_world.app.src ebin/static_world.app
ebin/%.beam: src/%.erl
	erlc -o ebin $<

build: static_world_app
	echo "build called"
deploy: static_world_app
	cp static_world_app ../../static_world_app 
static_world_app: compile
	./mad release script static_world_app
clean:
	-rm -f ebin/*
	-rm static_world_app
