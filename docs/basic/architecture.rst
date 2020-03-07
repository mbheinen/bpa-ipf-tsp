************
Architecture
************
The core powerflow program can be ran on separate machine from the graphical user interface. These two halves of the IPF program are joined by interprocess communication (IPC) routines that shuttle data and instruction messages back and forth between the GUI and powerflow program. The powerflow program serves primarily as a solution and database engine that sends and receives data when requested by the user through the GUI. Note that all of this commuication is basic socket communication so it likely doesn't work if GUI runs on one platform (Windows) and powerflow programs run on another (Linux).

Executables
===========
IPF can be thought of as a family of programs. ``bpf`` is the batch form of the powerflow program. When the editing and displaying of buses and branches is being handled by GUI, the work of calculating solution voltages for a given power system network is done by ``ipfsrv``, which is just the batch program in a different guise. Auxiliary programs allow you to do plots in batch mode, do a save of network data in batch mode, perform a “cut” of a solved base case, etc. These command line programs are briefly described here. 
 
 * ``gui`` Launches the X-based (X Window System) push button and menu-driven Graphical User Interface program that works in conjunction with the power flow server, ``ipfsrv``.

 * ``ipfsrv`` The power flow server to the GUI. It executes power flow commands through Power Flow Control Language (PCL) scripts dispatched from the GUI.

``ipfbat`` The batch version of IPFSRV. It accepts a “new style” Power Flow Control Language (PCL) script file. Plotting can be done with a control file; however, for most plots ``ipfplot`` is easier to use. Example of use: ``ipfbat test.pcl``. The “new style” PCL commands used with IPFSRV and IPFBAT (pseudo standard of .pcl) are described in ??.

``bpf`` The updated version of the old BPA batch Power Flow program. It executes using the commands from an “old style” Power Flow Control (PFC) script file. Example of use: ``bpf test.pfc.`` The PFC commands (.pfc) used with ``bpf`` are scripts for a complete power flow run. ?? describes the commands available.

``ipfplot`` Batch plotting program to produce printed maps. The program accepts a coordinate file and a base case file on the command line, as well as an optional second base case file. When the second base case file is specified, a difference plot is produced. You can also use ``ipfplot`` to produce bubble diagrams. The same coordinate files are used for both ``gui`` and ``ipfplot``, but not all capabilities are available in GUI. Documentation is in ??.

``ipfnet`` The batch version of the “save netdata file” function built into the ``gui``/``ipfsrv``. This program generates a WSCC-formatted network data file in any of the following dialects: BPA, WSCC, or PTI. “Dialects” means that the file is still WSCC, but the data is generated with special processing or restrictions and is destined for use with other programs. In the case of the PTI dialect, that data is preprocessed by the PTI-proprietary conversion program WSCFOR. Documentation is in ??.

``ipfcut`` The stand-alone program that cuts out a subsystem from a solved base case file. Flows at the cut branches are converted into equivalent generation or load on specially formatted +A continuation bus records. An ensuing power flow run should solve with internal branch flows and bus voltages which are identical to those quantities in the original base case. Documentation is in ??. Several methods are available to define the cut system: bus names, zones, base kVs, and individual branches.A pi-back feature replaces selected buses with a passive-node sequence (lines consisting of sections) with the original loads pi-backed in proportion to the line admittances.

``ips2ipf`` The program that converts a network data file from IPS to IPF. Duplicate buses are renamed; LTC steps are converted to taps, shunt susceptance on slack and BQ buses are transferred to +A records; sectionalized lines containing a section 0 are renumbered 1, 2, . . . ; BX, X, and remote controlled bus data are converted to IPF format, etc. Documentation is in ??