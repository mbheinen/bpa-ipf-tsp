.. _lineflow:

``lineflow``
============

``lineflow`` is a command line program that generates a table of values showing the requested branch 
quantities  for multiple base cases. Selects lines by branch list, bus, kV, owner, zone, loading 
level, or matches to 'wild card' input. Sorts alphabetically, or by owner, zone, kV, loading (in percent),
or according to input order of branches in a list. Generates a control script that allows repetitive 
similar studies to be performed automatically. Reports the following quantities: loading in Amps or 
MVA and percent of critical rating; or, power in, power out, and losses in MW. Data fields in the output
report table are character delimited to ease importing to Microsoft Excel or DECwrite.