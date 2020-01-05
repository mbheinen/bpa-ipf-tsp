# IPF Batch Powerflow -- bpf

Usage:  `bpf <controlfile.pfc> <cr>`

	or

	bpf <cr>
	BPA POWER FLOW PROGRAM VERSION:IPF  327                     
 	Enter Power Flow Control (PFC) file name > <controlfile.pfc> <cr>

Output:  Powerflow Output file <casename.pfo>

The updated version of the old BPA batch Power Flow program. It executes using the 
commands from an old style Batch Power Flow Control (PFC) script file. 

Example of use: bpf test.pfc. 

The PFC commands (standard filename extension of .pfc) used with BPF are
scripts for a complete power flow run. Chapter 4 of the IPF Batch Users Guide
(ipfbat.pdf) describes the commands available.

The BPF Approach: 
When you use BPF, you must first create a PFC file with the appropriate commands 
to accomplish the solution task at hand. At runtime these commands are
accepted by BPF and executed according to a logical processing order
determined by the program. Hence you need not be concerned with the ordering
of commands in your PFC file. Input commands will be processed first, and a
solution done automatically before any output is produced. Finally, a new base
file will be created, if you have requested one. See the IPF Batch Users
Guide (ipfbat.pdf) for information on this approach.

# IPF Advanced Powerflow Analysis -- cflow

Usage:  <cflow routine name> <cr>

	or

	<include these routines in .PCL control files for use with IPFBAT>

Output:  <depends on usage>

Pre-Programmed CFLOW routines included with IPF-3.27 include the following:
(see specific CFLOW routine HowTos for further information)

pvcurve
post_pvcurve
qvcurve
findout
mimic
puf


The CFLOW Approach: 
Many system planning studies entail a large number of similar runs. IPS users
have encoded these standard operations in the COPE language; to do the same
sort of thing with IPF, you will use the CFLOW approach. Unlike COPE, CFLOW is
not a complete language which is interpreted by the IPF program itself.
Instead, CFLOW consists of a library of C language functions, callable from
either C or Fortran. 

To create a CFLOW program or routine, you write your routine using the C
programming language (at least the main must be in C), including the header
file cflowlib.h, which defines all the structures and unions which allow
access to the powerflow input and solution values. To retrieve these values,
you call various CFLOW routines. You can also pass modifications to IPFSRV,
ask for a new solution, etc. See the IPF CFLOW Users Guide (cflow.pdf) for
information on writing these programs.

See section 3.4 of the IPF Advanced Users Guide (ipfadv.pdf) for information on
including these routines in .PCL control files, along with other processes.


# IPF Cutting Program -- cutting

Usage:  cutting <controlfile.pfc> <cr>

	or

	cutting <cr>
	BPA POWER FLOW PROGRAM VERSION:IPF  327                     
 	Enter Power Flow Control (PFC) file name > <controlfile.pfc> <cr>

Output:  Powerflow Network Data file <cutcasename.bse>

Cuts out a section of the entire system model, and prepares it to be set up for
running with its own slack bus.

This is a stand-alone program that cuts out a subsystem from a solved base
case (.BSE) file.  Flows at the cut branches are converted into equivalent
generation or load on specially formatted +A continuation bus records. An
ensuing power flow run should solve with internal branch flows and bus
voltages which are identical to those quantities in the original base case. 

In almost all cases, you will have to convert one of the buses in the cut
subsystem into a slack bus, to replace the original system slack bus (COULEE).

Several methods are available to define the cut system: bus names, zones,
base kVs, and individual branches.  A pi-back feature replaces selected buses
with a passive-node sequence (lines consisting of sections) with the original
loads pi-backed in proportion to the line admittances.

Documentation is in Appendix F of the IPF Batch Users Guide (ipfbat.pdf). 

SAMPLE CONTROL FILE: EWEBCUT1.PFC

( CUTTING, PROJECT=EWEB, CASEID=EWEBCUT1 )
>DEBUG<
.>EXCLUDE_BUSES<
.>INCLUDE_BUSES<
.>PI_BACK_BUSES<
.>SAVE_BUSES...<
.>CUT_BRANCHES<
>SAVE_ZONES NC,SAVE_BASES 500,230,115,69<
/ DEBUG, TX = ON, DC_MODEL = ON
(STOP)

# dos-linux
Utility Program to remove carriage returns from the ends of lines for 
data files imported to Linux from DOS/MS-Windows.  Also changes file 
names from Capital Letters to Lower Case.
