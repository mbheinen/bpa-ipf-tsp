.. _findout:

***********
``findout``
***********
Command line interface that Generates a table of outages and corresponding branch overloads or bus voltage violations from power flow output (``.pfo``) files. Works with the ``.pfo`` output files of ``bpf`` runs that contain an ``/OUTAGE_SIMULATION`` command. Runs as a post-processor to filter and sort the results and present them in             tabular form. Tables of 'Outages and Overloads' or 'Outages and Bus Violations' can be produced.  Entries in these tables can be filtered according to Zone, Owner, Base kV, Loading and Bus Voltage.

Tables can be sorted by Zone, Owner, Base kV, or alphabetically. The idea is to allow the user to automate the creation of a report detailing the results of outages instead of having to do manually which generally includes cut and paste operation with a text editor. Data fields in the output report table are character delimited to ease importing to Microsoft Excel or DECwrite.
