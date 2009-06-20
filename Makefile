all:
	(cd deps/beepbeep;$(MAKE))
	(cd src;$(MAKE))

clean:
	(cd src;$(MAKE) clean)
