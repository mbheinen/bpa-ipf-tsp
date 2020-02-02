# Interactive Power Flow & Transient Stability Program
This is a fork of Bonneville Power Administration's Interactive Power Flow (IPF) and Transient Stability Program (TSP). IPF and TSP were made public domain by Bonneville Power Administration (BPA) back in 1994. The original source code for this repo was obtained from ftp://ftp.bpa.gov, which no longer seems to be available. 

Interactive Power Flow (IPF) was developed by BPA and its contractors with about 20% of the cost supported by the Electric Power Research Institute (EPRI). By mutual agreement, as described in EPRI Agreement RP2746-03 entitled Graphical User Interface for Powerflow, March, 1992, all results of this project--including the computer program and its documentation--are to be in the public domain. In a separate Memorandum of Understanding with the Western Systems Coordinating Council (WSCC), BPA agreed in March, 1992, to keep WSCC informed of progress, to make its best effort to develop the program according to the Guidelines adopted by the WSCC Computer Program Management Subcommittee, and to make the final results available for possible further development by WSCC. 

The goal is to breathe life into this codebase again. The initial step is to get the command line tools working for benchmarking future projects related to power flow or transient stability analysis. Note that the original programs had a GUI component built with [Motif X Window], but given how dated it is, it is very unlikely it will ever run without significant effort. Once the power flow and transient programs are operational, it wil be easier to see if original GUI components can be made to work again.

# Building
The majority of this codebase is Fortran with some C. Both Fortran and C compiler are needed in order to compile it. Also, note that to this point it has only been test compiled on CentOS. To get the compilers:

    $ yum install gcc
    $ yum install gcc-gfortran

If you will be building the GUI as well, you will also need to install [Motif X Window]

    $ yum install motif
    $ yum install motif-devel

This project uses CMake. CMake is a multi-platform build tool that can generate build files for many different target platforms. See more info at http://www.cmake.org. CMake recommends doing "out of source" builds, that is, the build files are separated from your sources. This is convenient when doing development because there is no need to clean out compiled stuff (e.g. object files and executables) from the source tree. To do this, you create a `build/` directory at the top level of the project and everything gets built there. This allows you to just delete the `build/` directory when you're done. Doing a checkout and compile of this repository is done as follows:

    $ git clone https://github.com/mheinen/bpa-ipf-tsp
    $ cd bpa-ipf-tsp
    $ mkdir build
    $ cd build
    $ cmake ..
    $ make
    $ sudo make install
    
After building, you will see the library binaries in `lib/` and the executables in `bin/`.  

# Sample Cases
There are a variety of sample cases in the [data](https://github.com/mbheinen/bpa-ipf-tsp/tree/master/data) directory of this repo. Some of them came from original IPF codebase others came from publically available cases like Texas A&M's set of synthetic cases found [here](https://electricgrids.engr.tamu.edu/electric-grid-test-cases/). None of the data is from real power system networks since such information is generally considered confidential by Transmission Owners.

# Documentation
The sections below describe the various executables and libraries in this project and how to use them. The best place for a thorough description of the original IPF and TSP applications is the [manuals](https://github.com/mbheinen/bpa-ipf-tsp/tree/master/manuals). Additionally, check out [John Schaad's website](http://members.efn.org/~jschaad/ipf-1.html) for some history on the original BPA project.

## Batch Power Flow -- bpf
Batch Power Flow (`bpf`) executes using commands from a Power Flow Control (`.pfc`) file. Users write commands in the `.pfc` to do power flow runs. Chapter 4 of the [IPF Batch Users Guide] describes the commands available. You can try it out using test cases found in the [data](https://github.com/mbheinen/bpa-ipf-tsp/tree/master/data) directory. To run a case just run `bpf <controlfile.pfc>` or `bpf` and follow the prompts. For example,

    $ bpf bench.pfc

The output is a Power Flow Output file `<casename.pfo>`. When you use `bpf`, you must first create a PFC file with the appropriate commands to accomplish the solution task at hand. At runtime these commands are accepted by `bpf` and executed according to a logical processing order determined by the program. Hence you need not be concerned with the ordering of commands in your PFC file. Input commands will be processed first, and a solution done automatically before any output is produced. Finally, a new base file will be created, if you have requested one. See the [IPF Batch Users Guide] for more information.

## Cutting Program -- ipfcut
Cuts out a section of the entire system model and prepares it to be set up for running with its own slack bus.

Usage:  `ipfcut <controlfile.pfc>` or `ipfcut`

Output:  Powerflow Network Data file `<cutcasename.bse>`

This is a stand-alone program that cuts out a subsystem from a solved base case (.bse) file. Flows at the cut branches are converted into equivalent generation or load on specially formatted +A continuation bus records. An ensuing power flow run should solve with internal branch flows and bus voltages which are identical to those quantities in the original base case. 

In almost all cases, you will have to convert one of the buses in the cut
subsystem into a slack bus, to replace the original system slack bus.

Several methods are available to define the cut system: bus names, zones,
base kVs, and individual branches. A pi-back feature replaces selected buses
with a passive-node sequence (lines consisting of sections) with the original
loads pi-backed in proportion to the line admittances.

Documentation is in Appendix F of the [IPF Batch Users Guide]. 

Sample control file:

```
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
```

## Batch Analysis Tool -- ipfbat
The batch version of `ipfsrv`. It accepts a Power Flow Control Language (PCL) file. This was considered a "new" style of commands when BPA first wrote these programs.

Usage:  `ipfbat <controlfile.pcl>`

Example of use: `ipfbat test.pcl`. 

The new style PCL commands used with `ipfsrv` and `ipfbat` (standard filename extension of `.pcl`) are described in the [IPF Advanced Users Guide] and in Appendix A of the [IPF CFLOW Users Guide]. Many of the `bpf` commands from Chapter 4 of this manual are supported, but not all, and there are many additional new commands.

`ipfbat` allows you fine control over the case and solution engine (`ipfsrv`). 
When you use the PCL approach, you first create a PCL file with the
appropriate commands to accomplish the solution task at hand. At runtime these
commands are interpreted by IPFBAT. The PCL file commands are processed
sequentially. Additional PCL command files may be specified by name, so that a
chain  of PCL files may be processed in one run. See the [IPF Advanced Users
Guide] for details.

`ipfbat` Command Summary:

```
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
L     <bus1 bus2, etc> returns all paralles if id is wild card (*)
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

/INITDEF

        processed by p_initdef_ 

/LOADDEF

        processed by p_loaddef_ 

/SUBDEF

       processed by p_subdef_
```

## IPF Graphic User Interface -- gui

Usage:  `gui`

Screen/Mouse-driven, Point-and-Click control of IPF powerflow program.

The X-based (X Window System using the Motif Window Manager) push-button and
menu-driven Graphical User Interface (`gui`) program that works in conjunction
with the power flow server, IPFSRV.  Documentation is in the [IPF Basic/GUI
Users Guide].

When you use the GUI, you use the dialog boxes, menus, windows,
etc. This makes data input, output, and manipulation easy. In
addition to allowing basic case solution tasks to be accomplished, certain
specialized tasks such as creating coordinate files, taking line outages
graphically, printing plots, tapping lines, viewing summary reports, and line
impedance calculations, are available. 

For information about how to work with the GUI dialog
boxes, menus, windows, etc., see the [IPF Basic/GUI Users Guide]. This guide
also has a tutorial to show you how to solve straightforward power system
cases.  However,for some more involved analysis tasks, you need to use the BPF, 
IPFBAT, and CFLOW  approaches.  

## IPF Batch Plotting Program -- ipfplot

Usage: `ipfplot <coordinatefile.cor> <basefile1.bse> <basefile2.bse(opt)>`

Output:  PostScript Plot file `<plot.ps>`

Batch plotting program to produce printed map files in PostScript (.ps)
format. The program accepts a coordinate file (.cor) and a base case file
(.bse) on the command line, as well as an optional second base case file. When
the second base case file is specified, a difference plot is produced. You can
also use `ipfplot` to produce bubble diagrams. 

The same coordinate files (.cor) are used by both `gui` and `ipfplot`, but not
all capabilities are  available in the `gui`. Documentation is in the [IPF
Advanced Users Guide].

## IPF Reports -- ipf_reports
Creates customized output reports and summaries.

Usage: `ipf_reports <basefile1.bse>` or `ipf_reports`

## IPF C Library -- libcflow
CFLOW is a C library (`libcflow`) for IPF. This repo contains several programs that have been created with CFLOW:

* pvcurve
* post_pvcurve
* qvcurve
* findout
* mimic
* puf

To create a CFLOW program or routine, you write your routine using the C
programming language (at least the main must be in C), including the header
file `cflowlib.h`, which defines all the structures and unions which allow
access to the powerflow input and solution values. To retrieve these values,
you call various CFLOW routines. You can also pass modifications to `ipfsrv`,
ask for a new solution, etc. See the [IPF CFLOW Users Guide] for information 
on writing these programs.

See section 3.4 of the [IPF Advanced Users Guide] for information on including 
these routines in .PCL control files, along with other processes.

## fastout (a CFLOW program)
Powerflow (`ipfbat`) output report post-processor. Generates a table of the 
loading of overloaded branches for a list of outages with data from one or 
more `.pfo` files. Uses CFLOW library.

Original author:  William D. Rogers, BPA

Generate a table of values showing the loading of various branches 
under the condition of a specified outage for various cases.  For 
each case, the "Summary of bus and line problems for each outage" 
report in the corresponding .pfo file is scanned for the loading, 
percent of rating, and rating type for each branch-outage pair.  One
input file contains a list of .pfo file path names.  A second input
file contains a list of outages and branch-outage pairs.  Where 
branch-outage pair is specified, data is collected from each .pfo
file for that pair.  Where only an outage is specified, all of the
overloaded branches for that outage are reported.  Data fields in 
the output report table are character delimited to ease importing to
MS Excel or DECwrite.

### Input requirements:
    Powerflow (`ipfbat`) output report (`.pfo`) files.
    Text file containing a list of *.pfo file path names, 'pfo file list'.
    Text file containing overloaded and outaged branchs, 'outages  list'.
    Prompts user for file names and ouput options.  Defaults in brackets [].
        > Enter 'pfo file list' file spec [pfolist.dat]:
        > Enter 'outages  list' file spec [outages.dat]:
        > Enter 'output report' file spec [fastout.rpt]:
        > Enter 'output report' page width....... [132]:

### Output files
Text file containing a table of reported values, 'output report'.
stdout messages.

INPUT FILE FORMATS:

    *.pfo files - Output reports generated by Powerflow (BPF) that contain the
        "Summary of bus and line problems for each outage" report.

    'pfo file list' - a text file containing a list of powerflow (BPF) output
        files (*.pfo) with one file specification per line.  The symbols !,*, or
        . in the first column indicate a comment line.  The default filename is
        pfolist.dat.

For example:

! Any of the three symbols {!*.} in column 1 indicate a commment line.
. Names of powerflow output files listed one per line.
disk15:[cflow.reports]j01cy94rfo.pfo
j01cy93rfo.pfo

    'outages  list' - a text file containing a list of either monitored-branch 
        and outaged-branch pairs or outaged branches.  If only a single branch
        name is listed on a line, it is taken as the name of an outage and all
        branches overloaded by that outage will be reported in the output file.
        If a pair of branches is listed on a single line, the first is taken to
        be an overload caused by the outage of the second listed branch.  The
        loading of that overloaded branch will be listed in the output file, but
        the other overloaded branches for that outage will not be reported
        unless specified explicitly.

        Branch names are listed in Powerflow netdata format.  An optional
        vertical bar '|' can separate the overloaded from outaged branches when
        listed together on a single line.  Spaces before and after the vertical
        bar are ignored.  Branch id starts with the first character in the type
        field.  Branch types L, E, T, and TP are allowed.  The symbols !, *, or
        . in the first column indicate a comment line.  The default file name is
        outages.dat. 

For example:
```
. Any of the five symbols {!*.} in column 1 indicate a commment line
. This same file may be used by LINEFLOW in which case the Outaged Branch data
. is ignored.
.     Monitored Branches          |       Outaged Branches
.Y OWN[ BUS1 ](KV)M[ BUS2 ](KV)CS |TY    [ BUS1 ](KV) [ BUS2 ](KV)CS
L     BEAVER   115 TILLAMOK 115   |L     BEAVER   115 TILLAMOK 11500
L     MASS     115 UNION T  115 0  TP    BROAD ST 115 EASTPINE 115
.     Outaged Branches - all overloaded branches caused by these outages will be
.        included in the output report table
.Y    [ BUS1 ](KV) [ BUS2 ](KV)CS
L     BEAVER   115 TILLAMOK 11500
TP    BROAD ST 115 EASTPINE 115
```

OUTPUT REPORT OPTIONS:

    'output report' name - any valid file specification.  The default is
        fastout.rpt.

    Page width - the width in columns of the 'output report'.  Data for three
        cases of delimited data fields fit in 132 columns and 1 in 96 which the
        program defaults to if fewer columns are specified.  A large page width
        can be specified to cause all of the cases be printed in one table.  If 
        more cases are included than will fit across the page, additional tables
        are generated, following consecutively.  The default is 132 columns.

OUTPUT REPORT FORMAT:
    Data delimiter - an vertical bar '|' character is inserted between each data
        field in the 'output report'.  The vertical bar is used as a delimiter
        so that the tables can be imported into Microsoft Excel of DECwrite. 


A table of values is generated.  For each overloaded-branch/outaged-branch pair,
the critical rating is given and for each case, each branch's load and percent
of rating is given.  Critical ratings are based on the first case that is read, 
which is assumed to be the base case.  Flags are placed in the output report in
the |F| columns to indicate the type of rating found in the .pfo file.

The table is then printed to a text file.  For example (fictionalized data):

Data fields delimited with |.  Note: A full report of even one case requires 100
columns.  Shown here wrapped around.  Actual output is 132 columns wide.

```
.  |   |       CASE NAME:        | | |   |   |                         | 
.  |   |                         | | |   |   |                         | 
.  |   |  MONITORED BRANCH       | | |   |   |  OUTAGED BRANCH         | 
.Yc|OWN|< BUS1 ><V1> < BUS2 ><V2>|C|S|TYc|OWN|< BUS1 ><V1> < BUS2 ><V2>|C
L* |BPA|BEVERLY  115-SNOHOMSH 115|2|0|L  |BPA|BEVERLY  115-SNOHOMSH 115|1
L* |BPA|BEVERLY  115-SNOHOMSH 115|1|0|L  |BPA|BEVERLY  115-SNOHOMSH 115|2
L* |PSP|BOTHELL  230-SAMMAMSH 230| |2|T  |BPA|CHIEF JO 230-CHIEF JO 500| 
L* |PSP|BEVERLY  115-N.BOTHEL 115|1|0|L  |BPA|ECHOLAKE 500-MAPLVLTP 500| 
L* |PSP|BOTHELL  230-SAMMAMSH 230| |2|L  |BPA|ECHOLAKE 500-MAPLVLTP 500| 

 |       | | H97188F_MOD ||| H97188F_MOD ||| A97188F     |||
 | CRITCL| | [          ]||| [          ]||| [          ]|||
 | RATING| |  LOAD | LOAD| |  LOAD | LOAD| |  LOAD | LOAD| |
 | (UNIT)|F| (UNIT)| ( %)|F| (UNIT)| ( %)|F| (UNIT)| ( %)|F|
 |    490|T|  980.0|  200|T|  980.0|  200|T|  517.8|  106|T|
 |    490|T|  980.0|  200|T|  980.0|  200|T|  515.3|  105|T|
 |    555|T|    0.0|    0|T|    0.0|    0|-|  649.5|  117|T|
 |    306|B|  417.4|  136|B|  417.4|  136|B|  417.4|  136|T|
 |    556|E|  688.0|  124|E|  688.0|  124|E|  688.0|  124|t|

.UNIT is Amps for lines and MVA for transformers.
.CRITCL RATING taken from the first non-zero case.

.F Rating flags indicate type of critical rating.
.Lowercase flag indicates rating differs from listed value.
.T Thermal, N Nominal, B Bottleneck, E Emergency (Loss of Life)
.@ Outage found, but branch is not overloaded for this case.
.- Outage not found for this case.
```

USAGE:

Follow the prompts.  Just press return to use the default values which are given
in brackets [].

## findout (a CFLOW program)
Powerflow (`ipfbat`) output report post-processor. Generates a table of outages 
and corresponding branch overloads or bus voltage violations from multiple `.pfo` 
files. Sorts and screens. Uses CFLOW library.

DEVELOPER:  William D. Rogers, BPA, TEOS (detail), 230-3806, wdrogers@bpa.gov

Works with the `.pfo` output files of /OUTAGE_SIMULATION runs as a 
post-processor to screen and sort the results and present them in 
tabular form. Tables of 'Outages and Overloads' or 'Outages and 
Bus Violations' can be produced.  Entries in these tables can be
screened according to Zone, Owner, Base kV, Loading and Bus Voltage.
Tables can be sorted by Zone, Owner, Base kV, or alphabetically. The
idea is to allow the user to automate the creation of a report 
detailing the results of outages--saving time and reducing errors 
from the current method of doing so which generally includes cut and
paste operation with an editor.

Data fields in the output report table are character delimited to 
ease importing to MS Excel or DECwrite.

```
INPUT REQUIREMENTS:
    Powerflow output report (*.pfo) files generated by /OUTAGE_SIMULATION.
    Optionally, text files containing data for FINDOUT to work with.

PROMPTS:
    /PFO or /INCLUDE
     > Enter list of .PFO files (and/or data files):

    /REPORT
     > Enter report name,      NAME = [FINDOUT.RPT]:

     Specify the type of output report (by number)
       1. OUTAGEs  causing  OVERLOADs    2. OUTAGEs  causing  BUS Violations
       3. OVERLOADs caused by OUTAGEs    4. BUS Violations caused by OUTAGES
     > Enter report choice, TYPE =[OUTAGE-OVERLOAD]:
     > Redundant outages:   REMOVE_REDUNDANT = [NO]:
     > Do a difference report?     DO_DELTA = [YES]:

     Specify sort priority (by number)
       1. ALPHA-base_kv   3. OWNER-alpha-base_kv    5. INPUT order
       2. BASE_KV-alpha   4.  ZONE-alpha-base_kv    6. SOLUTION
     > Enter sort choice,            SORT = [ALPHA]:

     Specify output report format (by number)
       1. DATA-FULL (Excel) 2. DATA-BRIEF (Excel) 3. PAGE-WIDE 4. PAGE-COMPACT
     > Enter format choice,    FORMAT = [DATA-FULL]:

     Specify the maximum number of cases per table (6 fit in 132 columns).
     > Enter width,   CASES_PER_TABLE = [3]:

    /INCLUDE
     > Enter list of files with branch or bus lists:

    /OUTAGE
     > Enter outaged branches list       :
       > Tycown< BUS1 ><V1> < BUS2 ><V2>cs
       >
    /COMMON_MODE
     > Enter common-mode outages list    :
       >

    /OUTG_OWNER, /OUTG_ZONE, /OUTG_BASE_KV
     Specify selection of branch OUTAGEs and COMMON_MODE outages
     > Enter owners of interest..[all owners='***']:
     > Enter zones of interest.....[all zones='**']:
     > Enter base_kv of interest.[all base kV='>0']:

    /OVERLOAD
     > Enter overloaded branches list    :
       > Tycown< BUS1 ><V1> < BUS2 ><V2>cs
       >

    /PROB_OWNER, /PROB_ZONE, /PROB_BASE_KV
     Specify selection of OVERLOADed branches
     > Enter owners of interest..[all owners='***']:
     > Enter zones of interest.....[all zones='**']:
     > Enter base_kv of interest.[all base kV='>0']:

    /LIMITS
     > Enter max change in %, MAX_DELTA_PCT = [ 0%]:
     > Enter max loading in %, MAX_LOAD_PCT = [80%]:

     > Include solution problems? INC_SOLN_PROB=[Y]:

     Trace  report written to FINDOUT.TRC
     > Do you want to continue this run?        [Y]:

OUTPUT FILES:
    *.rpt - Report file with a formatted table of values.
    *.trc - Trace file containing a script of the session that can be repeated.
    *.log - Log/error file containing error messages, if any.
    Stdout messages.

INPUT AND TRACE DATA FORMATS:
    Forward-slash / tags alert the program to the type of data to follow.  Data
    should follow on subsequent lines, but not on the line with the / card.
    / tags and data are recorded in a trace (.TRC) file after all prompts have
    been answered.  The trace file is a record of input and can be user-modified
    and input into the program on subsequent runs.

    All characters after a \ (back slash) are ignored.
    A line with a . in column one is ignored.

    /INCLUDE 
        when this tag occurs in a trace file, a list of text file names will be
        read from the following lines.  Each file will be opened and scanned for
        other / cards containing basecase, branch, or script data.  The data 
        files (usually .DAT or .LIS) can also be specified at the /PFO or 
        /INCLUDE prompts when running interactively.

    /PFO
        a list of Powerflow analysis files (*.PFO). Multiple files may occur on 
        each line.  To include a text file containing a list of pfo files, 
        specify it after a /INCLUDE card.  The needed .PFO files are generated
        by using IPF's fast outage capability through the /OUTAGE_SIMULATION
        command.

        The following "Summaries" are searched for in the *.PFO files.
            Summary of bus and line problems for each outage
            Summary of System Separations and Divergences
            Summary of Common Mode Outages


        note: at the "/PFO or /INCLUDE" prompt, *.pfo and include (data) files
        may both be specified in a list form.  For example:

        /PFO or /INCLUDE
         > Enter list of .PFO files (and/or data files): J.PFO, A.PFO, DIFF.TRC

            where, J.PFO and A.PFO could be winter and summer cases and DIFF.TRC
            could be a trace file requesting a list of branches loaded above 85%
            and with overloads differing by 5% or more, belonging to BPA, in
            zone NB, at 230kV, and formatted for export to Excel and sorted by
            percent over-loading.

    /OUTAGE, /COMMON_MODE, /OVERLOAD, /BUS
        Branch and bus data is specified in Powerflow card format.  Specify one
        branch or bus per line.  Only the key id items are required: T, L or
        B for type, bus name(s) and kv(s).  If section or circuit id are blank,
        they are treated as wild cards and all branches between the specified
        buses will be found.  Note, this works much faster than using ?? for the
        circuit and section.

	Note: all branches will be sorted low alpha to high alpha and high kv to
        to low kv in order to conform to the fastoutage data in the PFO file.

        For wildcard searches, place at least one question mark, ?, in the data
        and all non-blank characters will be matched.  For example:

        > Tycown< BUS1 ><V1> < BUS2 ><V2>cs (Lowercase fields optional)
        > ?  B??MON?????

        will match any branch (transformer or line) whose owner starts with 'B'
        and has a bus starting with 'MON'.  Note:  This function can be very
        slow and base kv, if specified, must be in the form: [500.] instead of
        [ 500] in its 4 digit field.  Leave ckt and section blank if wild card
        search is desired.

    /OUTG_OWNER, /OUTG_ZONE, /OUTG_BASE_KV - outage and common_mode outages
    /PROB_OWNER, /PROB_ZONE, /PROB_BASE_KV - overloaded branches, u/o voltages
        Enter a list of owner, zones, or base kv after each appropriate tag.
        The wildcard * may be used in owner or zone names.  For example,
            N* as a zone matches NA, NB, NC,...,NZ.
        The bang ! symbol is used to exclude zones or owners.  For example,
            !NA, !NB  will retrieve data on all zones except NA and NB.
        To specify ALL ZONES or ALL OWNERS (these are the defaults) use ** and
            ***, respectively.
        Base kv is specified explicitly or by range.  For example,
            69, 115, >230, <69, 115-500, are all valid.  >115 gets all base kv
            strictly greater than 115 kV, not including 115.  115-230 gets all
            base kV between 115 and 230 kV including 115 and 230 kV.  Use >0 to
            specify ALL BASE KV; this is the default.

    /LIMIT
        This tag has up to seven valid data items:
         MAX_DELTA_PCT = 5  (any valid percentage)
         MAX_LOAD_PCT = 80  (any valid percentage)
         MAX_500_PU = 1.100 (for 500 kV busses)
         MIN_500_PU = 1.000
         MAX_BUS_PU = 1.052 (for busses with kV less than 500 kV)
         MIN_BUS_PU = 0.950
         INC_SOLN_PROB = YES

    note: for a difference report, both the MAX_DELTA_PCT and MAX_LOAD_PCT
          must be satisfied or the MAX_DELTA_PCT and voltage limits, depending
          on the type of report requested.

    /REPORT
        This tag contains all other program control data.
            NAME = LINEFLOWX.RPT       (specifies the name of the report)
            TYPE = OUTAGE-OVERLOAD     (or, OVERLOAD-OUTAGE, OUTAGE-BUS_V,
                                         BUS_V-OUTAGE)
            SORT = SOLUTION            (or, ALPHA, BASE_KV, ZONE, OWNER, INPUT)
            FORMAT = DATA_FULL         (or, DATA_BRIEF, PAGE_WIDE, PAGE_COMPACT)
            REMOVE_REDUNDANT = NO      (or, YES; used with common-mode outages)
            TRACE = ON                 (or, OFF, stops generation of trace)
            DELTA = ON                 (or, OFF; differences between PFO files)
            CASES_PER_TABLE = 6        (width depends upon FORMAT and TYPE)

        A note on the REMOVE_REDUNDANT option:
            A value of 'NO' causes FINDOUT to report all outages not filtered
            out by other means, such as owner, zone, etc.  This is the default.

            A value of 'YES' causes FINDOUT to not report problems caused by 
            outages that have previously been taken as part of a common-mode
            outage.  This is an additional screening tool.  For example, if
            a bus delete common-mode outage resulted in a branch outage, that
            branch would be taken out a second time by itself by the IPF 
            Fastoutage facility.  This option allow the user to automatically
            screen out the problems caused by the second occurance of that
            branch outage.

OUTPUT REPORT FORMAT:
    Data delimiter - a vertical bar '|' character is inserted between each data
        field in the output report.  The vertical bar is used as a delimiter so
        that the tables can be imported into Microsoft Excel of DECwrite. 
    
    A table of values is generated.  For each branch, the critical-rating is
    given and for each case, each branch's load and percent of rating is given.
    Critical ratings are based on the first case that is read.  Flags are placed
    in the output report in the |F| columns to indicate the rating type of 
    subsequent cases.

The table is then printed to a text file. For example (FORMAT = PAGE_COMPACT):
--------------------------------------------------------------------------------
.  |   |OUTAGES CAUSING OVERLOADS| | |  |  |    | |     A.PFO|||
.  |   | Outage followed by...   | | |  |  |    | |[        ]|||
.  |   |    ...List of Overloads | | |  |  |CRTC| | LOAD |LOAD||
.Yc|OWN|< BUS1 ><V1> < BUS2 ><V2>|C|S|Z1|Z2|RATG|F|ACTUAL|(%)|F|
  
L  |BPA|ABERD TP115. COSMOPLS115.| | |NA|NA|                        <-- outage
L  |GH |ABERDEEN115. WEYERHSR115.| |0|NA|NA| 478|T| 395.8| 83|T|    <-- overload

L  |BPA|ACTON   115. BONNVILE115.| | |NB|NB|                        <-- outage
L  |PPL|HOOD RVR69.0 POWERDLE69.0| |0|NB|NB| 300|N| 287.2| 96|N|    <-- overload
L  |PPL|POWERDLE69.0 SDS TAP 69.0| |0|NB|NB| 310|N| 460.6|149|N|    <-- overload
```

Usage:

    $ findout 

Optionally follow with a list of .pfo, trace, or list files

Follow the prompts.  Just press return to use the default values which are given
in brackets [].

# Coordinate File Modification for BPA Generation Studies
For Generation Integration Studies, the use of Powerflow Plots showing proposed new generation facilities and 
associated system improvements is essential. Generally the diagrams available are the Standard BPA 
Powerflow Plots (.cor files) developed for the Budget Cases. Creation of new plots or modification of these 
existing .COR files is necessary to show the additional facilities. Manual modification using a text editor is 
very time consuming and is not intuitive. Use of the IPF GUI is a very effective way to update the poweflow 
plots with the new system information, or to create new plots from scratch.

The following step-by-step procedure is a guide to the steps necessary to create (1) an updated .COR plot file 
based on an existing Budget Case Powerflow Plot .COR file, or (2) to create a new .COR plot file from scratch 
using the IPF GUI.

Procedure

1) Modify Existing Budget Case .COR File:

Run the IPF GUI and click the appropriate "select" and "apply", and "load selections" buttons 
to load the base case file (.bse) already containing the added Generation and associated 
system improvements and the Budget Case coordinate file (.cor) for the part of the system 
you are going to show. Then click the  "load selections" buttons to load them both.  Then 
"File, Save-As" to save the .COR file with a name of your choice in your user directory.  After 
that, activate the "Explode" function by clicking the 2nd Button in the 2nd Column, and click 
on the busses with the new generation and facilities you have added to the system model in the 
.BSE file.  Then, activate the "Symbol Move" function by clicking the 5th Button in the 2nd 
Column, and use the mouse to rearrange the symbols on the diagram representing the new 
generation and facilities.  When you are done, use "File, Save-As" to save your new .cor 
file.

* Click  "Select" for Base Case File directory list
* Click the .BSE file you want to use
* Click "Apply"
* Click "Select" for Coordinate File directory list
* Click the .cor file you want to use
* Click "Apply"
* Click "Load Selections"
* After the .BSE and .COR files are loaded, click "View, Solution Data On"
* Click "File, Save-As".
For "Coordinate", click "Select" and enter your new name for this coordinate file.
Click "OK" , then click "Save" to save the .COR file; then click "Close".

Procedure

2) Create New .COR File from scratch:

Run the IPF GUI and click the appropriate "select" and "apply", and "load selections" buttons 
to load the base case file (.bse) already containing the added Generation and associated 
system improvements and the Blank Template coordinate file (L85X11.COR or 
P85X11.COR) to show the part of the system you are going to show. Then click the  "load 
selections" buttons to load them both.  Then "File, Save-As" to save the .cor file with a 
name of your choice in your user directory.  

Activate the "Bus Input Data" function by clicking the 6th Button in the 1st Column.  The 
cursor will change from an arrow to an open square.  Close the "Bus Input Data" window.  
Click "Edit, Alpha List"  to bring up a list of all the busses in the .bse case.  Now select the 
bus you will use to start the diagram.  Click in the "Search Bus Name kV" window and with 
"CapsLock" on, type in the 1st letter of the bus name, then the 2nd, etc. until the bus name 
appears in the list.  Select the desired bus and click "Close".  Then, with the mouse, locate the 
1st bus at the appropriate location on the blank diagram, and click to place it there.

After that, activate the "Explode" function by clicking the 2nd Button in the 2nd Column, and 
click on the busses with the new generation and facilities you have added to the system model 
in the .BSE file.  Then, activate the "Symbol Move" function by clicking the 5th Button in the 
2nd Column, and use the mouse to rearrange the symbols on the diagram representing the new 
generation and facilities.  When you are done, use "File, Save-As" to save your new .COR 
file.

* Click "Select" for Base Case File directory list
* Click the .BSE file you want to use
* Click "Apply"
* Click "Select" for Coordinate File directory list
* Click the .COR file you want to use
* Click "Apply"
* Click "Load Selections"
* After the .BSE and .COR files are loaded, click "View, Solution Data On"
* Click "File, Save-As".  
* For "Coordinate", click "Select" and enter your new name for this coordinate file.
* Click "OK", then click "Save" to save the .COR file; then click "Close".

```
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
L     <bus1 bus2, etc> returns all paralles if id is wild card (*)
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

/INITDEF

        processed by p_initdef_ 

/LOADDEF

        processed by p_loaddef_ 

/SUBDEF

       processed by p_subdef_
```

## mimic (CFLOW program)
Powerflow (`ipfsrv`) case generation and screening.
Generate new cases and check for loading and voltage violations.
Original author:  William D. Rogers, BPA, TEOS (detail), 230-3806
Automates solving and screening basecases, applying changes to basecases, basecase comparisons, and plot generation.

INPUT REQUIREMENTS: (some or all of the following depending on user selections.)
    Powerflow basecases for IPFSRV to load, change, solve and return data from.
    Change files to apply to base cases.
    Text files containing a list of base case file names.
    Text files containing a list of change file names.
    Trace file specifying all or some of the desired inputs.
    Coordinate files for plot generation.
    Outage simulation (fastoutage) control file.

MODES OF OPERATION:
    MIMIC uses built in intelligence to prompt the user based on the information
    already provided.  This serves to reduce the number of queries the user
    must answer to run the program.

    MIMIC automates these operations:
        1)  Applying multiple changes to multiple base cases
        2)  Solving and screening multiple base cases
        3)  Comparing two or more base cases
        4)  Generating plots (including difference plots)
        5)  Building Excel-ready report tables
        6)  Logging all user input into a "TRACE" file
        7)  Performing multiple similar studies
        8)  Re-running studies with updated base cases
        9)  Automating /Outage_simulation studies
       10)  Solving basecases with /SOLUTION options

    MIMIC screens data using these filters:
        1)  Line and transformer loading
            a.  maximum allowable loading
            b.  changes in loading
            c.  changes in loading above a threshold
        2)  Bus voltage
            a.  separate limits for 500 kV buses and sub-500 kV buses
            b.  maximum and minimum voltage
            c.  heavy winter conditions
            d.  changes in voltage
            e.  changes in voltage above a threshold
        3)  Owner    (include, exclude, all, and/or wildcard; NA, !PR, A*, !T*)
        4)  Zone     (include, exclude, all, and/or wildcard; NA, !PR, A*, !T*)
        5)  Base kV  (list and/or ranges; 230, >350, <35, 69-115)

    MIMIC reports these quantities:
        1)  Branch loadings
        2)  Bus voltages
        3)  Transformer tap settings
        4)  Difference in value between two base cases
        5)  /OUTAGE_SIMULATION reports go to their own file

    MIMIC generates these reports:
        1)  Summary of Voltage Violations
        2)  Summary of Voltage Violations and Voltage Changes
        3)  Summary of Overloaded  Transformers
        4)  Summary of Overloaded Transformers and Loading Changes
        5)  Summary of Overloaded Lines
        6)  Summary of Overloaded Lines and Loading Changes
        7)  Summary of Tap Positions
        8)  Summary of Tap Positions and Tap Changes

PROMPTS:
    /BASECASE, /TRACE, or /INCLUDE
     > Enter list of Basecases (and/or data files) :

    /PRI_CHANGE
     > Enter list of Primary-Change(or data files) :

    /AUX_CHANGE
     > Enter list of Auxillary-Chng(or data files) :

    /NEW_BASE
     > Enter list of New Base names(or data files) :

    /REF_BASE
     > Enter list of Ref base cases(or data files) :

    /COORDINATE
     > Enter list of Coordinate files or data files:

    /REPORT
     > Enter report name,        NAME = [MIMIC.RPT]:

     Plot Options: P -print plot file. S -save plot file. D -difference plots.
     > Enter P, S, and/or D,      PLOT = [PS]:

     Enter a list of the types of checks that you want performed:
       0.  No checks - just apply changes - NONE.
       1.  Check Bus Voltages             - CHECK_VOLTAGE.
       2.  Check Branch Loading           - CHECK_LOADING.
       3.  Check Transformer Tap Settings - CHECK_TAPS.
       4.  Check and Compare Bus Voltages to Reference   - COMPARE_VOLTAGE.
       5.  Check and Compare Branch Loading to Reference - COMPARE_LOADING.
       6.  Check Transformer Tap Changes from Reference  - COMPARE_TAPS.
       7.  Perform Outage Simulation (fast outage) - OUTAGE_SIMULATION.
     > Enter check list, CHECK = : 1, 2  3 4

    /LIMITS
     Check Voltage: Enter limits:
     > Max 500kV-bus voltage PU, MAX_500_PU=[1.100]:
     > Min 500kV-bus voltage PU, MIN_500_PU=[1.000]:
     > Max sub-500kV voltage PU, MAX_BUS_PU=[1.052]:
     > Min sub-500kV voltage PU, MIN_BUS_PU=[0.950]:
     > Min EH Winter, 500kV-bus, MIN_EH5_PU=[1.050]:
     > Min EH Winter, sub-500kV, MIN_EHV_PU=[1.000]:

     Check Delta Voltage: Enter limits:
     > Max 500kV-bus voltage PU, DV_MAX_500_PU=[1.100]:
     > Min 500kV-bus voltage PU, DV_MIN_500_PU=[1.000]:
     > Max sub-500kV voltage PU, DV_MAX_BUS_PU=[1.052]:
     > Min sub-500kV voltage PU, DV_MIN_BUS_PU=[0.950]:
     > Min EH Winter, 500kV-bus, DV_MIN_EH5_PU=[1.050]:
     > Min EH Winter, sub-500kV, DV_MIN_EHV_PU=[1.000]:
     > Max voltage change, pct,  DV_MAX_DELTA =[  1.0]:

     Check Loading: Enter loading limits for solution check:
     > Maximum loading   (% Rating), MAX_LOAD_PCT=[100.0]:

     Perform /OUTAGE_SIMULATION: Enter control file:
     > Outage sim. control file,   OUTG_CON_FILE = :

    /OWNER, /ZONE, /BASE_KV
     > Enter owners of interest..[all owners='***']:
     > Enter zones of interest.....[all zones='**']:
     > Enter base_kv of interest.[all base kV='>0']:

    /REPORT
     > SAVE_NEW_BASECASES with changes? [Y]:
     > SHOW_CHANGES files in report?    [Y]:

    /COMMENT
     > Enter an 80-character comment string:

OUTPUT FILES:
    Report File - text file containing table of reported values.
    Trace File - text file containing list of all input values.
    Stdout messages.
    Text file containing output from Powerflow (IPFSRV) called ipfsrv_cf.out.
    MIMIC.LOG
    *.pfo files generated with /OUTAGE_SIMULATION analysis.

INPUT FILE FORMATS:

    trace file - this file contains a 'trace' of all inputs to MIMIC on a
        previous run.  It can be edited and modified.  Information deleted from
        a trace file will be prompted for on the next run.  Thus, a trace file
        can be set up to contain a list of 'default' inputs.  To control the way
        cases solve, add (all solution options can be used):
            /SOLUTION
            > solution option
            > solution option
            > etc. until done.

    'base case/list' -
        basecase - a single basecase name.  MIMIC searches the local directory
            and then the various basecase directories to find it.

        basecase list - a text file containing a list of Powerflow cases with
            one file specification per line.  The file name should end in .DAT.

For example:
BASE.DAT
--------------------------------------------------------------------------------
! The symbol '!' in column 1 indicates a comment line and is ignored.
! List the names of the powerflow cases one per line.
!
--------------------------------------------------------------------------------

    'pri chngs/list' -
        pri chngs - Primary Changes file.  Contains change records to be applied
            to the base case(s).
        pri chngs list - Primary Changes List file.   A text file containing a
            list of change files.

    'aux chngs/list' -
        aux chngs - Auxillary Changes file.  Contains change records to be
            applied to the base case(s).
        aux chngs list - Auxillary Changes List file.   A text file containing a
            list of change files.

A change file example:
```
/CHANGES,FILE= *\
. REMOVAL OF THE COL FALL 230/115-kV TRANSFORMER FROM BASE	
. SIMULATED LOSS OF 230/115-KV INTERCONNECTION
T D   COL FALL 230 COL FALL 115
(END)
```

A list of primary changes example:

```
! The symbol '!' in column 1 indicates a comment line and is ignored.
! List the names of the change files one per line.
!
    'ref base /list' -
        ref base - a Reference Basecase name. MIMIC searches the local directory
            and then the various basecase directories to find it.

        ref base list - a text file containing a list of Powerflow cases with
            one file specification per line.  The file name should end in .DAT.

    'cor file /list' -
        cor file - a Coordinate File used for generating plots.   MIMIC searches
            the local directory and then the various basecase directories.

        cor file list - a text file containing a list of Coordinate files with
            one file specification per line.  The file name should end in .DAT.

     Outage simulation control file (OUTG_CON_FILE) - a text file containing
        commands for /OUTAGE_SIMULATION studies.  MIMIC will send a command
        structured as follows:
        /OUTAGE_SIMULATION, FILE = <OUTG_CON_FILE>, OUTFILE = <basecase>.PFO    
```

OUTPUT REPORT FORMAT:

    'output report' name - any valid file specification.  A default is provided.

    The data fields are delimited with vertical bars (|) to facilitate moving 
    the data into DECwrite and Excel.

Basic form of this report shown below to give the general idea.
```
/BASECASE      | FGROVE.BSE
/PRI_CHANGE    | FG.CHG
/AUX_CHANGE    | 
/NEW_BASECASE  | FGROVE_000.BSE
/REPORT        | NAME = MIMIC.RPT
/OWNER         | BPA
/ZONE          | **
/BASE_KV       | 115-230
/COMMENT       | 
/REPORT        | TRACE=ON, PLOT =NONE, SAVE_NEW_BASECASES=YES, SHOW_CHANGES=YES
/REPORT        | CHECK= CHECK_VOLTAGE
/LIMITS        | REPORT_0_RATINGS = NO   CHECK_TX_THERMAL = NO
   MAX_500_PU = 1.050,   MIN_500_PU = 1.000,   MIN_500_EH = 1.050
   MAX_BUS_PU = 1.020,   MIN_BUS_PU = 1.000,   MIN_BUS_EH = 1.000
```
Primary Change File: FG.CHG
```
/CHANGES,FILE = * \
C DELETE CORNELUS 59.8 TO ORENCO 59.8 CIRCUIT 1
L DPG CORNELUS59.8 ORENCO  59.81
T DBPAKEELER   115 KEELER   2302
(END)

   |   |FGROVE_000.B|
   |   |BUS VOLTAGES|  | Voltage|
TYc|OWN|< BUS  ><KV>|ZN| ( PU  )|
B  |BPA|BEAVER  115.|NB|   1.025|
B  |BPA|BRUSH CL115.|NB|   1.034|
BT |BPA|CARLTON 115.|NB|   1.028|
BT |BPA|CHEMAWA 115.|NB|   1.034|
BQ |BPA|CHEMAWA 230.|NB|   1.043|

  FGROVE.BSE + FG.CHG = FGROVE_000.BSE                                 PAGE    1
```

The TRACE FILE for the above report is give below.
```
/BASECASE
  FGROVE.BSE
/PRI_CHANGE
  FG.CHG
/AUX_CHANGE
/NEW_BASECASE
/REF_BASECASE
/COORDINATE
/PLOT_NAME
/OWNER
  BPA
/ZONE
  **
/BASE_KV
  115-230
/COMMENT
/REPORT
  NAME = MIMIC.RPT
  PLOT = NONE
  APPLY = 1
  CHECK = CHECK_VOLTAGE
  TRACE = ON
  SAVE_NEW_BASECASES = YES
  SHOW_CHANGES = YES
/LIMITS
  MAX_500_PU = 1.050
  MIN_500_PU = 1.000
  MAX_BUS_PU = 1.020
  MIN_BUS_PU = 1.000
  MIN_EH5_PU = 1.050
  MIN_EHV_PU = 1.000
./TRACE
. FGROVE.BSE + FG.CHG = FGROVE_000.BSE 
```

Run by typing its name.

    $ mimic 

Optionally follow with a list of .pfo, trace, or list files

Follow the prompts.  Just press return to use the default values which are given
in brackets [].

## IPF Network Data Extraction -- netdat
Usage:  
```
   netdat <cr>
	 > Enter OLD_BASE file name (or Q to quit): <binarybasefile.bse>
	 > Enter saved network file (default is "binarybasefile.net"):<cr>
	 > Enter <return> to extract data as a BPA NETWORK_DATA file,
   	   enter G to extract data as a GE_DATA file, 
   	   enter P to extract data as a PTI_DATA file, or 
   	   enter Q to quit): 
```

Converts binary base file (.BSE) to an ASCII network data base file (.NET, .BPA, .PTI, or .GE).

This is the batch version of the "save netdata file" function built into the
GUI/IPFSRV. This program generates a WSCC-formatted network data file in any
of the following dialects: BPA, WSCC, or PTI.  "Dialects" means that the file
is still WSCC, but the data is generated with special processing or
restrictions and is destined for use with other programs. In the case of the
PTI dialect, that data is preprocessed by the PTI-proprietary conversion
program WSCFOR. Documentation is in Appendix F of the [IPF Batch Users Guide].

### BPA NETWORK_DATA file format example:
```
netdat <cr>
	 > Enter OLD_BASE file name (or Q to quit): <test.bse>
	 > Enter saved network file (default is "test.net"):<cr>
	 > Enter <return> to extract data as a BPA NETWORK_DATA file,
   	   enter G to extract data as a GE_DATA file, 
   	   enter P to extract data as a PTI_DATA file, or 
   	   enter Q to quit):  <cr>

> Enter dialect ( BPA, WSCC,
                   WSCC1 :  +A INT, taps --> BPA taps,
                   WSCC2 :  +A INT, taps --> WSCC steps,
                or PTI ): BPA <cr>
> Enter record size (120, 80, or B80/L120):120 <cr>
> Section buses saved as Pseudo-buses (N or Y)? : Y <cr>
> Replace assymetric type E branches with type L branches (N or Y)? : Y <cr>
> Replace d-c system with equivalent bus injections (N or Y): N <cr>
 * Options selected - dialect  = BPA       
                      size     = 120       
                      rating   = EXTENDED  
                      sections = PSEUDOBUSES 
                      type_e   = SYMMETRIC   
                      ips_ref  =  
                      zone_ren =  
                      net d-c  = N
 > Are above options correct (Y or N)? Y <cr>
 BCDDAT option set to default % VARs.
 BCDDAT option set to minimum X =  0.00000

 Original branch [L     G31HIGH  5002MID PS   50011 100  00080 01553      5221006000              ]                                  
 Replaced branch [L     G31HIGH  5002G31HIG&1 5001  100  00080 01553      5221006000              ]                                  
 Original branch [L     G31HIGH  5002MID PS   50012 100       -00767                              ]                                  
 Replaced branch [L     G31HIG&1 5002G31HIG&2 5001  100       -00767                              ]                                  
 Original branch [TP    G31HIGH  5002MID PS   50013 100  00020 00663            -3000             ]                                  
 Replaced branch [TP    G31HIG&2 5002MID PS   5001  100  00020 00663            -3000             ]                                  

 Original branch [L     G32HIGH  5002MID PS   50011 100  00080 01553      5221006000              ]                                  
 Replaced branch [L     G32HIGH  5002G32HIG&1 5001  100  00080 01553      5221006000              ]                                  
 Original branch [L     G32HIGH  5002MID PS   50012 100       -00767                              ]                                  
 Replaced branch [L     G32HIG&1 5002G32HIG&2 5001  100       -00767                              ]                                  
 Original branch [TP    G32HIGH  5002MID PS   50013 100  00020 00663            -3000             ]                                  
 Replaced branch [TP    G32HIG&2 5002MID PS   5001  100  00020 00663            -3000             ]                                  
    80 records written to NETWORK_DATA file test.net                                                                                 
 > Extract another case (Y or N)? N <cr>
```

### GE_DATA file format example:
```
netdat <cr>
	 > Enter OLD_BASE file name (or Q to quit): <test.bse>
	 > Enter saved network file (default is "test.net"):test.ge <cr>
	 > Enter <return> to extract data as a BPA NETWORK_DATA file,
   	   enter G to extract data as a GE_DATA file, 
   	   enter P to extract data as a PTI_DATA file, or 
   	   enter Q to quit):  G <cr>

> Enter GE translation file name (or <RETURN> if none, or Q to quit)> <cr>
 * Warning -- a translation file is not specified and no saved GE bus numberdata is available  
> Enter new GE translation file name (or <RETURN> if none) > <cr>
> Enter optional Branch Data file name (or <RETURN>  if none) > <cr>
> Enter optional Extended Branch Data file name (or <RETURN> if none) > <cr>
> Enter optional Out-of-service Branch Data file name (or <RETURN>  if none) > <cr>
> Enter optional Out-of-service Shunt Data file name (or <RETURN>  if none) > <cr>
> Rename blank parallel ID's to '1' (y or n): y <cr>
> Replace d-c system with equivalent bus injections (N or Y): Y <cr>
> Preserve BPA bus zones (N or Y): Y <cr>
> Preserve BPA bus ownerships (N or Y): Y <cr>
> Season to deposit IPF branch data ratings (those not in the branch data file)
   1 : RATE1, RATE2 (Summer) 
   2 : RATE3, RATE4 (Winter) 
   3 : RATE5, RATE6 (Spring) 
   4 : RATE7, RATE8 (Fall) 
> Enter Season Code: 2 <cr>
 * Options selected - Rename blank id's            = Y
                      Net d-c system               = Y
                      Preserve BPA bus zones       = Y
                      Preserve BPA bus ownerships  = Y
                      Season of IPF branch ratings = 2

 > Are above options correct (Y or N)? Y <cr>
```

### PTI_DATA file format example:
```
netdat <cr>
	 > Enter OLD_BASE file name (or Q to quit): <test.bse>
	 > Enter saved network file (default is "test.net"):test.pti <cr>
	 > Enter <return> to extract data as a BPA NETWORK_DATA file,
   	   enter G to extract data as a GE_DATA file, 
   	   enter P to extract data as a PTI_DATA file, or 
   	   enter Q to quit): P <cr>
> Enter PTI translation file name (or <RETURN> if none, or Q to quit) >  <cr>
* Warning -- a translation file is not specified and no saved PTI bus number data is available
> Enter PTI version number (23 or 24): 24 <cr>
> Enter new PTI translation file name (or NULL if none) > NULL <cr>
> Enter optional Out-of-service Branch Data file name (or <RETURN>  if none) >  <cr>
> Rename blank parallel ID's to '1' (y or n): y <cr>
> Append bus name, basekv to end of record (Y or N): Y <cr>
 * Rebuilding zone and ownership hash tables...
 * Rebuilding cross-reference arrays ...
 BCDDAT option set to compute % VARs.
 BCDDAT option set to minimum X =  0.00000
 * Creating a new PTI Translation File
    36 name records written to translation file NULL                                                                                 
  Bus records:   original     1 inserted    26
  Area records:  original     1 inserted     3
  Zone records:  original     1 inserted     3
```

## Path Use Factors -- puf
A new version of 'puf' (Path Use Factors aka Line Flow Analysis) corrects a 
factorization error and introduces new command-line options.

Usage:  `puf [-x|] [-lossless] [-pv] [-ai] [-g] [-ps] [-p1] [-c <cutfile>]`


The parameters are:

```
-x<flag> : defines an Excel field separation character. 
	<flag> is usually set to "|" (vertical bar).

-pv : sets all buses as PV. Default is PV and PQ.

-ai : sets area interchange control on. Default is off.

-g : restricts sensitivities to generator buses only. Default is all buses.

-ps : includes phase-shifter sensitivities. Default excludes phase shifter 
sensitivities.

-lossless : formulates the lossless sensitivities and should be used in
conjunction with -pv option. The lossless formulation minimizes the affect
of slack bus location. Default uses full Jacobian solution.

-l1 : sensitivities expressed as dLine/dP (default)

-l2 : sensitivities expressed as 1.0 + dLine/dP.

-c <cutfile> : constrain the flow path with the cut system defined by cutting 
commands in <cutfile>.
```

Example, using a single parameter: `puf -x|`

You will be prompted for three files:
* basecase file name (.bse)
* output file name (.out)
* flowgate file name (.txt)

The flowgate file name consists of WSCC-formatted branches which define a 
particular cut plane. For this example, use a single but critical branch, `L RAVER 500 PAUL 5001`  put in a text file, flowgate.txt

## pvcurve_pro - multiple buses perturbed in sequence
Generates voltage reactance curves. Automates production of P-V curve plot files and plot routine setup files for multiple base cases and outages.

Original author:  William D. Rogers, BPA, TOP, 230-3806, wdrogers@bpa.gov

```
INPUT:
        Any or all data can be entered interactively when prompted for, or
        specified ahead of time in trace or data files.

    REQUIRED
        Interactive Powerflow (IPF) binary base case(s).
        Critical bus name and kv.

    OPTIONAL
        Text file(s) listing base case file names.
        Text file(s) listing change file names.
        User analysis file.
        Text file(s) listing branches to take as outages.
        Text file(s) listing perturbed buses.
        Text file(s) listing monitored buses.
        Text file(s) listing generator buses.
        Text file(s) listing cut-planes.
        Common-mode outage master data file.
        Text file(s) listing common-mode outages.
        Text file(s) containing solution parameters.
        Trace file specifying all or some of the desired inputs.

MODES OF OPERATION:
    PVCURVE_PRO uses built in intelligence to prompt the user based on the
    information already provided.  This serves to reduce the number of queries
    the user must answer to run the program.

    PVCURVE_PRO automates these operations:
        1)  Running P-V studies for load or generation changes
        2)  Applying changes to multiple base cases prior to P-V run
        3)  Taking branch and common-mode outages
        4)  Applying change_bus_type other controls
        5)  Solving basecases with /SOLUTION options
        6)  Applying user analysis file prior to P-V run
        7)  Reporting pre-outage and post-outage quantities
            a) dVdQ (kV/MVAR) against Pgen,Ptotal (MW) for monitored buses
            b) Voltage (kV) against Pgen,Ptotal (MW) for monitored buses
            c) Pin (MW) total against Pgen,Ptotal (MW) for cut-planes
            d) Qgen, Qres (MW) against Pgen,Ptotal (MW) for generators
            e) User-analysis quantities
        8)  For generation perturbation, allows a list of generators to each be
            brought up from start to stop values at specified steps.
            (this feature is what distinguishes PVCURVE_PRO for PVCURVE)
        9)  Building Excel-friendly report tables
       10)  Logging all user input into a "TRACE" file
       11)  Performing multiple similar studies
       12)  Re-running studies with updated base cases

    PVCURVE_PRO operates on these data:
        1)  Base cases
        2)  Change files
        3)  Perturbed buses
        4)  Monitored buses
        5)  Generator buses
        6)  Branches to take out
        7)  Common-mode outages
        8)  Cut-planes (grouped branch flows)
        9)  Other IPF control cards
       10)  User analysis files

    PVCURVE_PRO reports these quantities:
        1)  dVdQ (kV/MVAR) for monitored buses
        2)  Voltage (kV) for monitored buses
        3)  Pin (MW) for cut-planes
        4)  Qgen (MW) for generators
        5)  Qres (MW) for generators
        6)  User-analysis quantities
        7)  Pgen at perturbed generator buses
        8)  Ptot of all perturbed generator buses
        9)  Load Change (%) of perturbed load area

    PVCURVE_PRO generates these reports:
        1)  Pre- and Post-outage reports (.txt) - examples follow
            a) P00_0DV.TXT      dVdQ (MW/kV)  at Pgen,Ptot,  pre-outage
            b) P00L0DV.TXT      dVdQ (MW/kV)  at Pgen,Ptot,  branch outage
            c) P00_0KV.TXT      Voltage (kV)  at Pgen,Ptot,  pre-outage
            d) P00L0KV.TXT      Voltage (kV)  at Pgen,Ptot,  branch outage
            e) P00_0MW.TXT      Pin  (MW)     at Pgen,Ptot,  pre-outage
            f) P00L0MW.TXT      Pin  (MW)     at Pgen,Ptot,  branch outage
            g) P00_0QQ.TXT      Qgen,res (MW) at Pgen,Ptot,  pre-outage
            h) P00L0QQ.TXT      Qgen,res (MW) at Pgen,Ptot,  branch outage
            i) P00_0UA.TXT      User-analysis at Pgen,Ptot,  pre-outage
            j) P00L0UA.TXT      User_analysis at Pgen,Ptot,  branch outage

PROMPTS:
    /BASECASE, /TRACE, or /INCLUDE
     > Enter list of Basecase, *.TRC, or *.DAT files : J98CY94.BSE

    /REPORT
     > Enter default output name,    NAME = [PVCURVE]:
     > Enter output series code,   SERIES = [P]:
     > Enter user analysis file,     USER_ANALYSIS = :
     > Enter auxillary control file,  CONTROL_FILE = :

     Specify the type of change_parameter command to perform
       1.  BUS Perturbation      2.  LOAD Perturbation
     > Enter choice, PERTURBATION = [1]: 2

    /PRI_CHANGE
     > Enter list of Primary Change (or *.DAT) files :

    /PERTURBED_BUS
     > Enter file of perturbed buses for P-V analysis:
     > Enter list of perturbed buses for P-V analysis:
       > B-----< Bus  ><KV>     <START> <STOP > <STEP >
       > B     JOHN DAY 500
       > B     MAYFIELD13.8       197.0   147.0    -5.0


    /MONITORED_BUSES
     > Enter file of monitored buses for dvdq summary:
     > Enter list of monitored buses for dvdq summary:
       > B-----< Bus  ><KV>
       >

    /GENERATOR_BUSES
     > Enter file of generator buses for Qres summary:
     > Enter list of generator buses for Qres summary:
       > B-----< Bus  ><KV>
       >

    /CUT_PLANE
     > Enter file of cut-plane identifiers/branches  :
     > Enter list of cut-plane identifiers/branches  :
       : Lyc<O>< Bus1 ><V1> < Bus2 ><V2>C
       : > cut-plane TEST FLOW CUT
       : L  BPABIG EDDY 500 CELILO   5001
       : L  BPABIG EDDY 500 CELILO   5002
       : > cut-plane JOHNDAY-BIGEDDY-OST
       : L  BPABIG EDDY 500 JOHN DAY 500 
       : L  BPABIG EDDY 500 JOHN DAY 5002
       : L  BPABIG EDDY 500 OSTRNDER 500 

    /BRANCH
     > Enter file of branches for single-line outages:
     > Enter list of branches for single-line outages:
       > Tyc<O>< Bus1 ><V1> < Bus2 ><V2>C
       >

    /REPORT
     > Enter main common-mode file, COMMON_MODE_DATA=:

    /LIMITS
     P-V curve load perturbation: Enter change per step and number of steps:
     > Load change per step (%),LOAD_P_PCT = [ 5.00] :
     > Load change per step (%),LOAD_Q_PCT = [ 5.00] :
     > Number of steps,            LOAD_STEPS =  [10] :
     > Enter zones  to be perturbed, LOAD_ZONES  = [] :
     > Enter areas  to be perturbed, LOAD_AREAS  = [] :
     > Enter owners to be perturbed, LOAD_OWNERS = [] :

    /LIMITS
     P-V curve bus perturbation: Enter Start and Stop Generation and step size:
     > Starting generation (MW),  GEN_START = [ 0.0] :
     > Ending generation (MW),    GEN_STOP  = [75.0] :
     > Generation step size (MW), GEN_STEP  = [25.0] :

    /SOLUTION, /CHANGE_BUS_TYPES
     > Enter file of sol'n and change_bus_type defaults:

    /SOLUTION       (Enter /solution qualifier and parameters)
    :

    /CHANGE_BUS_TYPES (Enter /change_bus_type qualifiers and data)
    > /CHANGE_BUS_TYPES,


OUTPUT FILES:
    Trace File (.trc) - records all input parameters
    Error File (.log) - errors and operating messages
    IPFSRV File (.out) - contains output from Powerflow, ipfsrv_cf.out
    Output FILES (.txt) - various output quantaties
        P00X0DV.TXT
        ^^^^^^^
        ||||||____ File Data Type: (KV, MW, PV, QQ, DV, or UA)
        |||||_____ Outage Number: (0...9)
        ||||______ Outage Type: _(pre-outage) X(none) L(branch) C(common-mode)
        |||_______ Primary Change File Number: (0...9)
        ||________ Basecase Number: (0...9)
        |_________ Series Code: (A...Z)

        Notes:  The series code is used to distinguish between multiple similar
                data runs.
```

Run by typing its name.

    $ pvcurve_pro 

Optionally follow with a trace (.trc) file

Follow the prompts.  Just press return to use the default values which are given
in brackets [].


## qvcurve_pro - generate voltage reactive curves
Automates production of Q-V curve plot files and plot routine setup
files for multiple base cases, outages, and critical buses.

Original author:  William D. Rogers, BPA, TOP, 230-3806, wdrogers@bpa.gov

```
INPUT:
        Any or all data can be entered interactively when prompted for, or
        specified ahead of time in trace or data files.

    REQUIRED
        Interactive Powerflow (IPF) binary base case(s).
        Critical bus name and kv.

    OPTIONAL
        Text file(s) listing base case file names.
        Text file(s) listing change file names.
        User analysis file.
        Text file(s) listing branches to take as outages.
        Text file(s) listing critical buses.
        Text file(s) listing generator buses.
        Text file(s) listing cut-planes.
        Common-mode outage master data file.
        Text file(s) listing common-mode outages.
        Trace file specifying all or some of the desired inputs.

MODES OF OPERATION:
    QVcurve_Pro uses built in intelligence to prompt the user based on the
    information already provided.  This serves to reduce the number of queries
    the user must answer to run the program.

    QVcurve_Pro automates these operations:
        1)  Running /change_parameter of Q versus V at a bus
        2)  Applying changes to multiple base cases prior to Q-V run
        3)  Taking branch and common-mode outages
        4)  Applying change_bus_type, AGC,and other controls
        5)  Solving basecases with /SOLUTION options
        6)  Applying user analysis file prior to Q-V run
        7)  Generating Q-V curve plot setup files
            a. for PSAP 22 plotter
            b. for XYplot CFLOW program
        8)  Generating a Q reserve summary of specified generators
        9)  Generating a cut-plane flow summary of specified cut-planes
       10)  Building Excel-friendly report tables
       11)  Logging all user input into a "TRACE" file
       12)  Performing multiple similar studies
       13)  Re-running studies with updated base cases

    QVcurve_Pro operates on these data:
        1)  Base cases
        2)  Change files
        3)  Critical buses
        4)  Branches to take out
        5)  Common-mode outages
        6)  Generator buses
        7)  Branches monitored for flow
        8)  Cut-planes (grouped branch flows)
        9)  Other IPF control cards
       10)  User analysis files

    QVcurve_Pro reports these quantities:
        1)  Qknee (Q minimum, approximate)
        2)  Qlast (last value of Q obtained)
        3)  Qmargn (Q margin)
        4)  Vknee (Voltage at Qknee)
        5)  Vlast (last voltage solved)
        6)  Vop (operating voltage when Q=0)
        7)  Qmax,basecase (maximum available Q at each specified generator)
        8)  Qres,basecase (Qmax-Qgen at each specified generator)
        9)  Qres,Vop (Qmax-Qgen when critical bus at Vop at each generator)
       10)  Qres,Vknee (Qmax-Qgen when critical bus at Vknee at each generator)
       11)  Total Qmax,basecase
       12)  Total Qres,basecase
       13)  Total Qres,Vop (also reported as Qres@Qop)
       14)  Total Qres,Vknee (also reported as Qres@Qknee)
       15)  Branch flows
       16)  Cut-plane flows

    QVcurve_Pro generates these reports:
        1)  Critical bus with outage report (*.rpt)
        2)  Generator reserve and cut-plane report (*.gen)
        3)  Individual Q-V curves (*.qvpt)
        4)  Setup files for PSAP22 (*.setup)
	5)  Setup files for XYCURVE (*.xyc)

PROMPTS (with some example inputs):
    /BASECASE, /TRACE, or /INCLUDE
     > Enter list of Basecase, *.TRC, or *.DAT files : 97AG023.BSE

    /PRI_CHANGE
     > Enter list of Primary Change (or *.DAT) files : QV007C.CHNG

    /OUTPUT
     > Enter list of names for .qvpt and .user files :

    /BUS
     > Enter file of critical busses for Q-V analysis:
     > Enter list of critical busses for Q-V analysis:
       > Btc<O>< Bus  ><KV>
       > B     BIG EDDY500.
       >

    /GEN
     > Enter file of generator buses for Qres summary:
     > Enter list of generator buses for Qres summary:
       > Btc<O>< Bus  ><KV>
       > B     COULEE 213.8
       >

    /CUT_PLANE
     > Enter file of monitored cut-plane descriptions:
     > Enter list of monitored cut-plane descriptions:
       > Lyc<O>< Bus1 ><V1> < Bus2 ><V2>C
       > L  BPABIG EDDY500.0JOHN DAY500. 0
       > L  BPABIG EDDY500.0JOHN DAY500.20
       >

    /BRANCH
     > Enter file of branches for single-line outages:
     > Enter list of branches for single-line outages:
       > Tyc<O>< Bus1 ><V1> < Bus2 ><V2>C
       > L  BPABUCKLEY 500.0SLATT   500.
       >

    /REPORT
     > Enter main common-mode file, COMMON_MODE_DATA=: COMMON.DAT

    /COMMON_MODE
     > Enter file of common-mode outage identifiers  :
     > Enter list of common-mode outage identifiers  :
       > BFR ASHE-MARION-ALVEY
       >

    /REPORT
     > Enter summary report name, FILE =[qvcurve.rpt]:
     > Enter user analysis file,     USER_ANALYSIS = :
     > Enter auxillary control file,  CONTROL_FILE = :
     Each plot can have from 1 to 6 curves:
     > Enter number of curves,  CURVES_PER_PLOT = [5]:

    /LIMITS
     For Q-V curve analysis: Enter Min, Max, and Delta Voltage:
     > Max BE bus voltage in PU,  VX_MAX_PU = [1.100]:
     > Min BE bus voltage in PU,  VX_MIN_PU = [0.900]:
     > Max voltage change in PU,DELTA_VX_PU = [0.050]:

    /SOLUTION, /CHANGE_BUS_TYPES, or /AGC
     > Enter file of sol'n and change_bus_type defaults:

    /SOLUTION       (Enter /solution qualifier and parameters)
    : >AI_CONTROL=CON<
    : >LTC=ON_DCONLY<
    : >SOL_ITER,NEWTON=40<
    :

    /CHANGE_BUS_TYPES (Enter /change_bus_type qualifiers and data)
    > /CHANGE_BUS_TYPES, BQ=B,BG=BQ,BX=B,LTC=OFF
    : >LINE_DROP_COMPENSATION
    : B     CENTR G1  20, 50%
    : B     CENTR G2  20, 50%
    : B     COULEE19  15, 50%
    : B     COULEE20  15, 50%
    : B     COULEE23  15, 50%
    : B     COULEE24  15, 50%
    :

    /AGC (bus id data is fixed format, the rest is free field)
    : B     < Bus  ><KV>   Pmin=<##>, Pmax=<##>, Pgen=<##>, =<##>
    :

OUTPUT FILES:
    Report File (*.rpt) - critical bus with outage report
    Auxillary Report File (*.aux) - generator and cut-plane report
    Trace File (*.trc) - records all input parameters
    Error File (*.log) - errors and operating messages
    Q-V Files (*.qvpt) - list of Q and V values
    IPFSRV File (*.out) - contains output from Powerflow, ipfsrv_cf.out

INPUT FILE FORMATS: (similar to other CFLOW programs)
```

Sample Trace File

```
/REPORT
  FILE = 97AG023.RPT
  USER_ANALYSIS = 
  CONTROL_FILE = 
  CURVES_PER_PLOT = 6
  COMMON_MODE_DATA = COMMON.DAT
  TRACE = YES
/LIMITS
  VX_MAX_PU   = 1.100
  VX_MIN_PU   = 0.900
  DELTA_VX_PU = 0.010
/CHANGE_BUS_TYPES, BQ=B,BG=BQ,BX=B,LTC=OFF
>LINE_DROP_COMPENSATION
B     CENTR G1  20, 50%
B     CENTR G2  20, 50%
B     COULEE19  15, 50%
B     COULEE20  15, 50%
B     COULEE23  15, 50%
B     COULEE24  15, 50%
/AGC
/SOLUTION
>AI_CONTROL=CON<
>LTC=ON_DCONLY<
>SOL_ITER,NEWTON=40< 
/BASECASE
  97AG023.BSE
/PRI_CHANGE
  QV007C.CHNG
/OUTPUT
/SETUP
/BUS
B     BIG EDDY500.  
B     JOHN DAY500.  
/GENERATORS
B     COULEE 213.8  
B     CHIEF J213.8  
/CUT_PLANES
>
L  BPABIG EDDY500.0JOHN DAY500. 0
L  BPABIG EDDY500.0JOHN DAY500.20
>
L  BCHACK500  500.0NIC500  500.10
L  BCHACK500  500.0NIC500  500.20
L  BCHACK500  500.0REV500  500.10
L  BCHACK500  500.0REV500  500.20
L  BCHACK500  500.0SEL500  500.10
/BRANCH
/COMMON_MODE
BFR ASHE-MARION-ALVEY
./TRACE
. 97AG023.BSE + QV007C.CHNG - {BFR#ASHE-MARION-ALVEY} @ (BIG#EDDY500.) 
    > 97AG023-QV007C-C01-01.qvpt 
. 97AG023.BSE + QV007C.CHNG - {BFR#ASHE-MARION-ALVEY} @ (JOHN#DAY500.)
    > 97AG023-QV007C-C01-02.qvpt 
```

OUTPUT REPORT FORMAT:

    The data fields are delimited with vertical bars (|) to facilitate moving 
    the data into DECwrite and Excel.

Run by typing its name.

    $ qvcurve_pro

Optionally follow with a trace (.trc) file

Follow the prompts.  Just press return to use the default values which are given
in brackets [].

[IPF Basic/GUI Users Guide]: https://github.com/mbheinen/bpa-ipf-tsp/blob/master/manuals/IPFGUI.PDF
[IPF Batch Users Guide]: https://github.com/mbheinen/bpa-ipf-tsp/blob/master/manuals/IPFBAT.PDF
[IPF Advanced Users Guide]: https://github.com/mbheinen/bpa-ipf-tsp/blob/master/manuals/IPFADV.PDF
[IPF CFLOW Users Guide]: https://github.com/mbheinen/bpa-ipf-tsp/blob/master/manuals/CFLOW.PDF
[Motif X Window]: https://motif.ics.com/motif/downloads
