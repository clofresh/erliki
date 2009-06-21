all: deps erliki

deps: beepbeep

beepbeep:
	(cd deps/beepbeep;$(MAKE))

erliki:
	(cd src;$(MAKE))

docs:
	erl -pa `pwd`/ebin \
	-noshell \
	-run edoc_run application "'Erliki'" '"."' '[no_packages]'

clean:
	(cd src;$(MAKE) clean)
