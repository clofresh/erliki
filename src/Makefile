include ../support/include.mk

all: $(EBIN_FILES_NO_DOCS) yecc

yecc:
	erl -noshell -pa ../ebin -s wiki_tokenizer generate_parser -s init stop
	erlc -W +debug_info -o ../ebin wiki_parser.erl

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES_NO_DOCS)
	rm -f wiki_parser.erl



