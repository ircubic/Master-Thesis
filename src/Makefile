default: compile

spec: npc.sml specscrubber.sed
	sed -f specscrubber.sed npc.sml > npc.spec

compile: npc.sml
	mlton npc.sml

unittest: npc.sml unittests.sml
	cat npc.sml unittests.sml > test.sml
	mlton test.sml
	./test
