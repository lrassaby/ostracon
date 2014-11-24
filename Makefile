PROFESSORS=professors/_rel/ostracon_release/bin/ostracon_release

all: build_professors run_professors

build_professors:
	cd professors && make

run_professors:
	$(PROFESSORS) console
