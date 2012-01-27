default: library


library:
	mlton -default-ann 'allowFFI true' -format library -output sim.so simulation/test.sml

bin:
	mlton -default-ann 'allowFFI true' -output test simulation/test.sml
