all: build run-i

build:
	cd ostracon && make

run-i:
	ostracon/_rel/ostracon_release/bin/ostracon_release console

run:
	ostracon/_rel/ostracon_release/bin/ostracon_release run 
