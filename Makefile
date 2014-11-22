RELEASE=ostracon/_rel/ostracon_release/bin/ostracon_release

all: build interpreter 

build:
	cd ostracon && make

interpreter:
	$(RELEASE) console

start:
	$(RELEASE) start 
