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

Output:  Powerflow Network Data file `<cutcasename.bse>`

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
# IPF Batch Analysis Tool -- ipfbat

Usage:  `ipfbat <controlfile.pcl> <cr>`

The batch version of IPFSRV. It accepts a  new style  Power Flow Control Language (PCL) 
script file.  

Example of use: `ipfbat test.pcl`. 

The new style PCL commands used with IPFSRV and IPFBAT (standard filename
extension of .pcl) are described in the IPF Advanced Users Guide
(ipfadv.pdf) and in Appendix A of the CFLOW Users Guide (cflow.pdf). Many of
the BPF commands from chapter 4 of this manual are supported, but not all, and
there are many additional new commands.


The IPFBAT Approach: 
IPFBAT allows you fine control over the database and solution engine (IPFSRV). 
When you use the PCL approach, you first create a PCL file with the
appropriate commands to accomplish the solution task at hand. At runtime these
commands are interpreted by IPFBAT. The PCL file commands are processed
sequentially. Additional PCL command files may be specified by name, so that a
chain  of PCL files may be processed in one run. See the IPF Advanced User s
Guide for details (ipfadv.pdf).

IPFBAT Command Summary:

/INITIALIZE

       processed by  p_pfinit_ 

/NETWORK_DATA, FILE = <filename>

       processed by p_gtnetdat_ 

/OLD_BASE, FILE = <* | filename> [, CASE = <casename>] 
                                 [, REBUILD = < on | off> ]
       processed by cmd_parse.c
                    p_gtbase.f
                    ctlpow.f

/CHANGES, FILE = <* | filename>

       processed by cmd_parse.c
                    p_change.f
                    ctlpow.f

/SOLUTION
> BASE_SOLUTION
> DEBUG, BUS = ON,(page 3-51 to 3-56) 

       processed by cmd_parse.c
                    p_solton.f
                    ctlpow.f

/GET_DATA, TYPE = INPUT 
A     <areaname>  to be added 22 July by wlp
I     <area1 area2> to be added 22 July by wlp
B     <busname, etc> returns all data associatated with bus
+     <busname, etc> returns all data if id fields have wild cards
                     (type - column 2, owner, columns 3-5, and code-year 
                      columns 20-21)
X     <busname, etc>
L     <bus1 bus2, etc> returns all paralles if id is wild card (\*)
                       returns all sections if section is 0
T     <bus1 bus2, etc>
R     <bus1 bus2, etc>
E     <bus1 bus2, etc>

       processed by cmd_parse.c
                    p_gtdata.f 
                    gtinput.f

/GET_DATA, TYPE = BUS_VOLTAGES

       processed by cmd_parse.c
                    p_gtdata.f 
                    bus_voltages.f

/GET_DATA, TYPE = BUS_LIST [ FROM BUS_DATA ]
                  WHERE AREAS = <area1>, <area2>, etc AND 
                         ZONES = <zone1>, <zone2>, etc AND
                         OWNERS = <own1>, <own2>, etc AND
                         BASEKV = base1 
                                  < base  ( example < 115.0 means all base
                                            kv's less than or equal to 115.0)
                                  > base  ( example > 115.0 means all base
                                            kv's greater than or equal to 115.0)
                                  base1 < base2  (all bases between base1 and
                                                  base 2)
                                  base2 > base1  (same as above)
                         TYPE = B , BE, BS, BC, BD ,BV ,BQ ,BG ,BO ,BT ,BX ,
                                BM ,L  ,LD ,LM , ,E  ,T  ,TP ,R 
                         BUS = "<busname>" (quotes are necesary)
                         AFTER_BUS = "<busname>" ( to do 22 July by wlp)
                         ALL
                         LOADING = (<min> <max>)

       processed by cmd_parse.c
                    p_gtdata.f 
                    bus_list.f
                    gtfltr.f

/GET_DATA, TYPE = A_DATA

       processed by cmd_parse.c
                    p_gtdata.f 
                    a_data.f

/GET_DATA, TYPE = I_DATA

       processed by cmd_parse.c
                    p_gtdata.f 
                    i_data.f

/GET_DATA, TYPE = BSEKV_LIST

       processed by cmd_parse.c
                    p_gtdata.f 
                    bsekvlst.f

/GET_DATA, TYPE = AREA_LIST

       processed by cmd_parse.c
                    p_gtdata.f 
                    area_list.f

/GET_DATA, TYPE = OWNER_LIST

       processed by cmd_parse.c
                    p_gtdata.f 
                    owner_list.f

/GET_DATA, TYPE = BUS_EXISTS, BUS = "bus_name" 

       processed by cmd_parse.c
                    p_gtdata.f 
                    ex_bus.f
       return status: status = 0 : bus does not exist
                               1 : bus exists

/GET_DATA, TYPE = FILE_EXISTS, FILE = <file_name>

       processed by cmd_parse.c
                    p_gtdata.f 
                    ex_file.f
       return status: status = 0 : file does not exist
                               1 : file exists
/GET_DATA, TYPE = CONNECTION
B     <busname, etc> returns all connection data associatated with bus

       processed by cmd_parse.c
                    p_gtdata.f 
                    ex_file.f
       return status: status = 0 : file does not exist
                               1 : file exists

/GET_DATA, TYPE = ZONE_LIST

       processed by cmd_parse.c
                    p_gtdata.f 
                    zone_list.f

/GET_DATA, TYPE = OUTAGES

       processed by cmd_parse.c
                    p_gtdata.f 
                    gtoutage.f

/GET_DATA, TYPE = LINE_IMPEDANCE_CALCULATION
           UNITS = < ENGLISH | METRIC >, 
           DISTANCE = < miles | km >
           BASEKV = <basekv>, 
           BASEMVA = <basemva>, 
           FREQUENCY = <freq>
 CONDUCTOR = 1 .3636 .05215  1.602  -20.75   50.     50.   0.0 0.0 0
 CONDUCTOR = 1 .3636 .05215  1.602  -19.25   50.     50.   0.0 0.0 0  
 CONDUCTOR = 2 .3636 .05215  1.602   -0.75   77.5    77.5  0.0 0.0 0
 CONDUCTOR = 2 .3636 .05215  1.602    0.75   77.5    77.5  0.0 0.0 0
 CONDUCTOR = 3 .3636 .05215  1.602   19.25   50.     50.   0.0 0.0 0
 CONDUCTOR = 3 .3636 .05215  1.602   20.75   50.     50.   0.0 0.0 0
 CONDUCTOR = 0 .5    2.61    0.386  -12.9    98.5    98.5  0.0 0.0 0
 CONDUCTOR = 0 .5    2.61    0.386   12.9    98.5    98.5  0.0 0.0 0

       processed by cmd_parse.c
                    p_gtdata.f 
                    p_lic.f
                    linimp.f

/GET_DATA, TYPE = OUTPUT
A     <areaname>  to be added 22 July by wlp
I     <area1 area2> to be added 22 July by wlp
B     <busname, etc> returns all data associatated with bus
+     <busname, etc> returns all data if id fields have wild cards
                     (type - column 2, owner, columns 3-5, and code-year 
                      columns 20-21)
X     <busname, etc>
L     <bus1 bus2, etc> returns all paralles if id is wild card (*)
                       returns all sections if section is wild card
T     <bus1 bus2, etc>
R     <bus1 bus2, etc>
E     <bus1 bus2, etc>

       processed by cmd_parse.c
                    p_gtdata.f 
                    gtoutput.f

/GET_DATA, TYPE = INITIALIZE_DEF

       processed by cmd_parse.c 
                   p_gtdata.f
                   p_initdef.f

/GET_DATA, TYPE = LOAD_DEFINE
> DEFINE ...
> DEFINE ...
> DEFINE ...
C ...
C ...
C ...

       processed by cmd_parse.c 
                   p_gtdata.f
                   p_loaddef.f

/GET_DATA, TYPE = SUB_DEFINE, SOURCE = BASE
                                       ALTERNATE_BASE
       processed by cmd_parse.c 
                   p_gtdata.f
                   p_subdef.f

/GET_DATA, TYPE = LOAD_AREA

       processed by cmd_parse.c
                    p_gtdata.f 
                    p_ldardata.f
       return status: status = 0 : success
                               1 : errors

/GET_DATA, TYPE = AREA_DATA
A  <areaname>

       processed by cmd_parse.c
                    p_gtdata.f 
                    gtardata
       return status: status = 0 : success
                               1 : errors

/REPORTS, SELECT BUS_BR_INPUT [ FROM BUS_DATA ]
                 [ OUTPUT = <filename> ]
                 WHERE (repeat filter from BUS_LIST) 

       processed by cmd_parse.c 
                   p_report.f
                   busbrinrpt.f
                   gtfltr.f 

/REPORTS, SELECT BUS_BR_OUTPUT [ FROM BUS_DATA ]
                 [ OUTPUT = <filename> ]
                 WHERE (repeat filter from BUS_LIST) 

       processed by cmd_parse.c 
                   p_report.f
                   busbrotrpt.f
                   gtfltr.f 

/REPORTS, SELECT OVERLOADED_LINES [ FROM BUS_DATA ]
                 [ OUTPUT = <filename> ]
                 WHERE (repeat filter from BUS_LIST) 

       processed by cmd_parse.c 
                   p_report.f
                   ovldlnsrpt.f
                   gtfltr.f 

/REPORTS, SELECT OVERLOADED_TXS [ FROM BUS_DATA ]
                 [ OUTPUT = <filename> ]
                 WHERE (repeat filter from BUS_LIST) 

       processed by cmd_parse.c 
                   p_report.f
                   ovldtxsrpt.f
                   gtfltr.f 

/REPORTS, SELECT BUS_UVOV [ FROM BUS_DATA ]
                 [ OUTPUT = <filename> ]
                 WHERE (repeat filter from BUS_LIST) 

       processed by cmd_parse.c 
                   p_report.f
                   busuvovrpt.f
                   gtfltr.f 

/REPORTS, SELECT VOLTAGE_COMPARISON
                 [ OUTPUT = <filename> ]
                 FILE = <filename>
                 WHERE (repeat filter from BUS_LIST) 

/REPORTS, SELECT LINE_COMPARISON 
                 [ OUTPUT = <filename> ]
                 FILE = <filename>
                 WHERE (repeat filter from BUS_LIST) 

/REPORTS, SELECT AI_SUMMARY 
                 [ OUTPUT = <filename> ]

/REPORTS, SELECT TIE_LINE_SUMMARY
                 [ OUTPUT = <filename> ]
                 WHERE (repeat filter from BUS_LIST) 

/REPORTS, SELECT DC_INPUT
                 [ OUTPUT = <filename> ]

/REPORTS, SELECT DC_OUTPUT
                 [ OUTPUT = <filename> ]
 
/REPORTS, SELECT NETWORK_CHANGES
                 [ OUTPUT = <filename> ]

/REPORTS, SELECT NETWORK_DELETIONS
                 [ OUTPUT = <filename> ]

/REPORTS, SELECT PHASE_SHIFTER
                 [ OUTPUT = <filename> ]

/REPORTS, SELECT 
          (GUI-customized reports, but not completed!)
       processed by cmd_parse.c 
                   p_report.f
                   outputrpt.f
                   gtfltr.f 

/NEW_BASE, FILE = <filename>, CASE = <casename>
C  < case comments - three records maximum >
C  < case comments - three records maximum >
C  < case comments - three records maximum >
       processed by cmd_parse.c
                    p_newbse.f
                    ctlpow.f

/SAVE_FILE, TYPE = SYSTEM_CHANGES, FILE = <filename>

/SAVE_FILE, TYPE = NETWORK_DATA, FILE = <filename>, OPTIONS = BPA,
                                                              WSCC,
                                                              WSCC1,
                                                              PTI,
                                                              PECO,
                                                              PECO1,
                   SIZE = 80,   RATINGS = EXTENDED
                          120,            NOMINAL
                                          MIN_EXTENDED

/SAVE_FILE, TYPE = WSCC_BINARY_STABILITY, FILE = <filename>
                   WSCC_ASCII_STABILITY

/QUIT

        processed by p_pfexit_ 

/EXIT

        processed by p_pfexit_ 

/CFLOW_GUI

        processed by p_cflow_gui 

/QUIT

        processed by p_pfexit_ 

/EXIT

        processed by p_pfexit_ 

/CFLOW_GUI

        processed by p_cflow_gui 

/INITDEF

        processed by p_initdef_ 

/LOADDEF

        processed by p_loaddef_ 

/SUBDEF

       processed by p_subdef_


