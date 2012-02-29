default: compile

spec:
	sed '/(\*SPECSTART\|SPECEND\*)/d;s/(\*%%\*)/%%/' npc.sml > npc.spec

compile:
	mlton npc.sml
