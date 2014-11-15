all: build interpreter 

build:
	cd ostracon && make

interpreter:
	ostracon/_rel/ostracon_release/bin/ostracon_release console

start:
	ostracon/_rel/ostracon_release/bin/ostracon_release start 
