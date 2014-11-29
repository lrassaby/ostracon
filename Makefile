INTRO=introdemo/_rel/ostracon_release/bin/ostracon_release
PROFESSORS=professors/_rel/ostracon_release/bin/ostracon_release

professors: build_professors run_professors
intro: build_intro run_intro

build_professors:
	cd professors && make

run_professors:
	$(PROFESSORS) console

build_intro:
	cd introdemo && make

run_intro:
	$(INTRO) console
