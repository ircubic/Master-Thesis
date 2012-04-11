default: compile

spec: npc.sml specscrubber.sed
	sed -f specscrubber.sed npc.sml > npc.spec
	makespec npc.spec
	cat main1.sml npc.spec.sml main2.sml
	mlt
	echo "Your binary is done and can be run with ./main"

compile: npc.sml
	mlton npc.sml

unittest: npc.sml unittests.sml
	cat npc.sml unittests.sml > test.sml
	mlton test.sml
	./test
