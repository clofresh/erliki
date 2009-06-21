all: deps erliki

deps: beepbeep

beepbeep:
	(cd deps/beepbeep;$(MAKE))

erliki:
	(cd src;$(MAKE))

clean:
	(cd src;$(MAKE) clean)
