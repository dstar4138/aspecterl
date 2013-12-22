.PHONY: async call_logger erlaop pooler distclean

all: async call_logger erlaop pooler

async:
	cd async; make

call_logger:
	cd call_logger; make

erlaop:
	cd erlaop; make

pooler: 
	cd pooler; make

distclean:
	cd async; make distclean
	cd call_logger; make distclean
	cd erlaop; make distclean
	cd pooler; make distclean

