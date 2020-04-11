******************
libcflow C Library
******************

Overview
========
CLFOW is a C Application Programming Interface (API) for IPF. The library name is ``libcflow``. With CFLOW, users can access the bus and branch data within the power flow data structures in a highly flexible way. Once data is retrieved from the IPF "database engine", it can be manipulated within the user-written C program and either output to a file, screen, or plotter, or sent back to the IPF "database engine" itself. A CFLOW program is a totally separate process running concurrently with the "powerflow solution and database server" process and communicates with it via an IPC (Inter Process Communication) protocol, so CFLOW requires an
operating system capable of multi-processing and that supports “sockets” for IPC.

In order to use CFLOW, you must have some knowledge of the C language and a C compiler. Once a CFLOW program has been written, compiled, linked, and debugged, it is stored as an executable which can be run from the command line or from the IPF GUI using the “PROCESS -- CFLOW” menu option.

The CFLOW product is completely compatible with ANSI C language and compilers and portable to all environments that have an ANSI C compiler.

History
-------
CFLOW was written to perform analogous functions to the WSCC COPE language: Computationally Oriented Programming Environment. COPE is a stand-alone language integrated with the WSCC equivalent of BPA’s IPF, the Interactive Powerflow System (IPS). CFLOW is a library of C language functions, that have “sister” functions within the IPF. The function pairs form a “remote procedure call” library. All the power of the C language is immediately available to CFLOW users, whereas COPE users are limited to the COPE language and environment. Another example of similar functionality is PTI’s IPLAN which is interpreted similar to the way COPE is. CFLOW is more powerful and flexible than COPE.

Audience
--------
This documentation assumes that you are a beginning to mid-level C language programmer. This means you should have successfully written programs in languages such as Fortran, BASIC, Pascal, or C. These need not be complex programs, simply programs such as you would be required to complete in an undergraduate college programming course. If you already know C, you are ready to write CFLOW programs. If not, you should probably take a class, invest in a computer tutorial course, and/or spend some time with a good C language book.

The following C programming books are recommended for those users needing introductory or
refresher information:

  * Brakakati, Nabajyoti. The Waite Group’s Microsoft C Bible. Howard W. Sams &
Company, 1988. This MS-DOS environment reference book clearly describes ANSI C
compatibility for each function.
  * Harbison, Samuel P. and Guy L. Steele. C: A Reference Manual. 3rd ed. Prentice-Hall,
1991. This book shows ANSI C facilities contrasted with traditional or alternate facilities.
If you are well acquainted with C programming, but want to make sure your program
complies with ANSI C, look here.
  * Johnsonbaugh, Richard and Martin Kalin. Applications Programming in ANSI C.
MacMillan, 1990. This is a textbook used in beginning undergraduate college courses.
  * Kernighan, Brian W. and Dennis M. Ritchie. The C Programming Language. Second
Edition. Prentice-Hall, 1988. This is the standard book for learning the language, updated
to a second edition. Experienced programmers will do well with this book. Beginners
should use Kochan’s book.
  * Kernighan, Brian W. and Rob Pike. The UNIX Programming Environment. Prentice-Hall,
1984. This book describes how to develop programs in the UNIX operating system.
  * Kochan, Stephen G. Programming in ANSI C. Howard W. Sams & Company, 1988. This
book is a comprehensive tutorial for the beginning programmer.
  * Plauger, P. J. The Standard C Library. Prentice-Hall, 1992. This book shows you how to
use the standard ANSI and ISO C library functions. It provides code examples for
implementing many of the library functions.

Creating a Program
==================
CFLOW is a library of functions that a C program can link to (with an object file linker) to access IPF data and control IPF execution. The program you write is a C program. CFLOW source programs are created using a text editor. The program lines can be entered in “free format,” since there are no column restrictions like Fortran.

Indentation is recommended when designing nested logical constructs to reduce logic errors and enhance readability. For example:

.. code::

  if(condition) {/* beginning of first “if” block */
    if(condition) {/* beginning of second “if” block */
      statement;
      statement;
    }/* end of second “if” block */
    statement;
    statement;
  }/* end of first “if” block */
  statement;

Include the CFLOW header file, called ``cflowlib.h``, in each of your CFLOW files

Running a CFLOW Program
=======================

From the command line
---------------------
To run your CFLOW program from the command line, simply run the program as you would any
other program. If your system needs some setup to be done to allow your program to accept
command line arguments (VMS), you can use the script run_cflow_bg , which can be customized
for your system. For example, the following could be used:
my_cflow_program [ arg1 ] [ arg2 ] ...
run_cflow_bg my_cflow_program [ arg1 ] [ arg2 ] ...
The CFLOW program will launch the IPFSRV program and establish a connection. The script
run_ipfsrv_cf is used to run the IPFSRV program. This script can be customized for your
system. There are two optional command line arguments that are helpful in debugging. These
options must precede any of the arguments for the CFLOW program.
-n noserver, do not launch IPFSRV
-w < wait time > max wait time for socket connect

From the GUI
------------
There are two ways to execute a CFLOW program from the GUI: Either in its own terminal
window, or in the same terminal window that the GUI is running in (background).
CFLOW programs that are run in the same terminal window as the GUI that read/write to stdin
or stdout (i.e. use readln or printf) have their I/O intermixed in the same terminal window as
the GUI and IPF server. The background mode should usually only be used for programs that
generate a report or other output to a file.

To run a CFLOW program from the GUI, you start up the GUI, and set up whatever conditions
are required for your program to work, such as loading and solving a case, if the program is
designed to report on a currently loaded case. Then select Process - Run CFLOW. You will get a
file selection window. Double-click on a directory name, or change the filter field and click the
Filter button, to change the file list. When you see the file you want to run (your executable
CFLOW program), select it, and it will appear in the Selection field. Select either Window
(default) or Background, and, if your program has command line arguments, type these into the
CFLOW Program Arguments field. Then click the Launch CFLOW button. The Wait field
(default 30 seconds) is provided just in case your program takes longer than 30 seconds to start up.
The wait time is the length of time that the GUI/IPFSRV will wait for the CFLOW program to
start up and establish a socket (inter-process communication) connection. The value can be set
from 15 to 300 seconds. If the CFLOW program has not connected within the wait time, then the
CFLOW run is aborted and control is returned to the GUI.

The program is run synchronously. This means that you cannot use the GUI until the CFLOW
program is finished running. If the CFLOW program fails, control is returned to the GUI. However,
if the CFLOW program hangs (as in an infinite loop), you need to kill the CFLOW process through
operating system resources. (For example, on VMS this can be done with the stop process
command, on UNIX, this can be done with the kill command.) See your computer system
documentation or your system administrator for help.

When the program completes, the CFLOW window goes away and control returns to the GUI. You
can then load a different file, or make other changes, and select Process - Run CFLOW again to
rerun. The scripts run_cflow_win (window) and run_cflow_bg (background) are used to run
the CFLOW program. These scripts can be customized for your system.

From `ipfbat`
-------------
For batch, also called background or terminal window interactive processing, the ipfbat program
is provided. This program reads a control file rather than connecting to and receiving commands
from the GUI process.
A CFLOW program can be run by including the following command in the control file:
/CFLOW, PROGRAM =
[ directory path ] < CFLOW executable file or script file >
[ , WINDOW ]
[ , WAIT = < max wait time for socket connect > ]
[ , ARGS = < command line arguments for the specified program > ]
The brackets ([ ]) denote optional items. The command is free format with the restriction that any
“word” (like the [ path ] < file > ) must be all on the same line (not continued on the next line) with
no imbedded blanks or any of the following: “,=\n” . On a UNIX system, for example, you can use
the following:
/CFLOW, PROGRAM = my_cflow_program
This “launches” the program if it is in your directory search path.
ARGS = is required only if the CFLOW program requires command line arguments. Without the
WINDOW option, any I/O from the CFLOW program goes to standard input or standard output,
and will be to and from the same terminal window that the ipfbat program is run from (intermixed
with any I/O from the ipfbat program). The scripts run_cflow_win (window) and
run_cflow_bg (background) are use to run the CFLOW program. These scripts can be
customized for your system.

Debugging
=========
Use the standard system debugger for your computer system to debug CFLOW C programs.
Although it is possible to do debugging using the GUI/IPFSRV or the IPFBAT programs, it is
recommended that you debug by running your CFLOW program from the command line.
If your program expects some setup (i.e. a case already loaded and solved), because your program
is used like a subroutine that generates a report or other output based on whatever is currently there,
then for debugging purposes, you can create a function that does the setup (e.g. load base, apply
changes, solve) and call that function at the beginning of your program. When the program is
debugged, you can “comment out” the call to the setup function.
The pf_cflow_init() function uses the C library function system(“command line”) to launch
the script run_ipfsrv_cf which in turn runs the IPFSRV program with the output redirected to
a file. There is a default wait (time out) of 30 seconds for the IPFSRV program to extablish a socket
connection, but this can be increased, if needed, to up to 300 seconds with the -w option. See
running from the command line above. In general, when debugging, you want to “step over”
(versus “step into”) the pf_cflow_init() function, however, if you decide to step through the
function, be aware that part of the code has the time out in effect and will cause a “failed connect”
to occur if you proceed too leisurely.
If you experience problems with debugging that you suspect are related to the “system” call that
launches the IPFSRV program, you can use the -n (noserver) option as follows to debug from two
windows:
In one window run your CFLOW program.
my_cflow -n -w 300 [ arg1 ] [ ... ]
Wait for the message “using socket nnnn” (this will happen when the pf_cflow_init() function
is executed), then in another window run the IPFSRV program.
ipfsrv -socket nnnn
where “nnnn” is the same as what the CFLOW program stated. The socket connection should
happen in less than 10 seconds (usually a couple of seconds) depending upon the speed of your
system.
You can then debug your program in one window while the IPFSRV program runs in the other.

Functions Overview
==================
The CFLOW library is a set of functions, written in the C language, that allows access to IPF data.
Behind the scenes, the routines communicate with the IPF program via an interprocess
communication channel known as a socket (similar to a pipe or stream). The routines are
organized, as much as possible, as database access routines, since the powerflow program is
playing the role of database and compute “server.”
There are four major classes of functions:

  * Simple IPF “command” functions.
  * Record-oriented IPF functions.
  * Buffer-oriented IPF “command” functions.
  * Utility functions and local data translation.

The library currently accesses only the Powerflow program. In the future, a similar approach could
be used to provide a CFLOW interface for other programs. The various functions are documented
in Chapter 4. All of those that access Powerflow start with ``pf_``. All functions return a non-zero
integer for an error condition and a zero for successful completion

Simple Functions
----------------
Examples of simple functions are ``pf_area_of_zone``, ``pf_del_zone``, ``pf_rename_area``, ``pf_rename_zone``, and ``pf_rename_bus``. These functions perform an operation that requires little or no input data, other than a command, and usually return only a status, or a single piece of data such as ``pf_area_of_zone`` does.

Record Oriented Functions
-------------------------
Examples of record-oriented functions are ``pf_rec_bus`` and ``pf_rec_cbus``. These functions use C
language structures to manipulate a Powerflow record. Both generic and record-type specific
structure definitions are provided, so that field names specific to the record type can be used for a
little better “self-documenting” code. For example, the variable containing TAP2 for a transformer
contains B2 for an E type line, and the minimum phase shift for a type RM regulating phase shifter.
This same variable can be accessed by using the names ``r.i.branch.tap2``, ``r.i.pf_E.b2``, and
r.i.pf_RM.min_phase_shift_deg.

The functions all use an action code to specify what is to be done with the record, such as D for
delete, F2 to retrieve the first branch record associated with two named buses, and O to retrieve
solution (output) data for a bus or branch.

Buffer Oriented Operations
--------------------------
The buffer-oriented operations are all accessed through one function: ``pf_cflow_ipc``. This
function sends a buffer to Powerflow containing a command, command options, and usually input
data. A buffer is returned that contains the results of the request. Any command that is in the IPF
Advanced User Manual can be put in the buffer, with records separated by the ’\n’ (linefeed)
character. Much of the data sent and received in the buffers is WSCC-formatted data.

Utility Functions
-----------------
Most of the utility functions are provided to form an “abstraction layer” between your program
logic and WSCC formatted ascii records. There are functions to translate between the C structures
and WSCC ascii records, as well as functions to initialize the C structures.

Notes
=====
Below are a few notes, reminders, and definitions relating to the C language and the CFLOW library functions, for your convenience.

Strings
-------
Many of the function parameters, such as bus names and action codes, are described as strings. In C, the technical definition of a string is as follows:  An array of characters, with a null terminator (ASCII character ``\0``) in the element following the last valid character.

The library functions, like most C library functions, expect the strings you supply to conform to this definition.  The easiest way is to enclose the value you want in double quotes (e.g. ``“AMBROSIA”``).  You can also store a value like this when you initially declare the character array. 

.. note::

  When you declare a ``char`` array, remember to always size it one larger than you need to store your actual string, so there is room for the null character!

However, you cannot put this value in an array with an assignment statement (``name = "AMBROSIA"`` is illegal). To store a string in a character array, use the standard C ``strcpy`` or ``strncpy`` function.

A single character can be placed in one element of a character array by enclosing it in single quotes (``name[0] = 'A'``; is a legal C assignment statement). If you do this, be sure to store a null in the last character (``name[8] = '\0'``;).

Arrays
------
In C, an array dimension is declared to be the actual number of elements in the array:  

.. code::

  char name[9];   /* Sized to hold an 8-character bus name, plus a null.*/

But when you reference the array elements, the indices run from zero to one less than the declared dimension. In the example above, ``name[0]`` has the first character of the bus name; ``name[7]`` has the last character, and ``name[8]``, which is the last element of the array, contains a null.

Function Types
--------------
If a function is declared as ``void``, then no return value is expected, and it may be invoked without a place being provided to receive the returned value. Example:

.. code::

  pf_init_bus( &b, “B”, “AMBROSIA”, 230.0 );

Most of the CFLOW functions are ``int``, meaning that they return an integer value, which is usually zero for success and non-zero for any kind of error.  These must be called in some way which is compatible with the name of the function being a variable with a value. Example:

.. code::

  int error;
  error = pf_rec_bus( &b, “G” );

Main
----
Your main program must have the standard arguments ``int`` and ``char **``: 

.. code::

  int main ( int argc, char *argv[] )
  {
    ...
  }

You cannot merely call it “main”, as you may have done for programs in a C class, or which you will see as examples in the books. Look at the sample CFLOW programs for guidance.

Includes
--------
In most program you write, you at least need the standard C headers ``stdio.h`` and ``string.h``, and also the CFLOW library header, ``cflowlib.h``.  This is done with a preprocessor statement: ``#include <stdio.h>`` for libraries in the standard C include directory. ``#include "cflowlib.h"`` for libraries in some other directory.

Linking
-------
Your CFLOW program can be compiled by itself, but in order to execute, it must be linked with the cflow library, ``libcflow.a``.  The simplest way to do this (on a Unix system) is to use something like CMake or a Makefile. Look at the examples in this project for how to do this.

Buffers
-------
CFLOW communicates with ipfsrv by using buffers which are passed back and forth (see the next section, Global Buffers, for detailed information).  If you want to see what is in the buffers, set the variable cf_debug to one.  As long as it has this value, the contents of every buffer passed will be displayed in the terminal window.  Since this output will go by so fast you can’t read it, and it can be very voluminous, you will want to limit the number of buffers actually displayed to the ones you are interested in.  Set cf_debug back to zero to turn off the display.

Languages
---------
CFLOW routines do not have to be written in C, except for the main program.  If you are adept at the intricacies of calling C routines from a Fortran program and vice versa, you can write your main processing and reporting routines in Fortran, or use code you already have.  In general, it will be easier to write a C program to perform the function, rather than trying to retrofit.

Global Buffers and Variables 
============================
The CFLOW code defines four global buffers as described in the table below.

========================= =====================================================================
Buffer                    Description
========================= =====================================================================
``char *pf_cflow_inbuf``  A buffer of the data most recently received from Powerflow.
``char *pf_cflow_outbut`` A buffer of the data most recently sent to Powerflow.
``char *err_buf``         A buffer containing a null terminated list of error messages from the most recent Powerflow communication.
``char *reply_buf``       A buffer containing a null terminated list of data from the most recent Powerflow communication.
========================= =====================================================================

The CFLOW code defines two global variables as described in the table below.

================ ============================================================
Buffer           Description
================ ============================================================
``int cf_debug``   = 1 to turn on debug output. = 0 to turn off debug output.
``int cv_error``  Contains the most recent conversion error code.
================ ============================================================

Simple Report Example
=====================
The ANSI C program listed below was derived from a COPE program. This program illustrates
how the CFLOW library and ANSI standard C may accomplish many of the same tasks as COPE
programs. Detailed discussion follows the program code.
The following program outputs a Shunt Reactive Summary report to the screen. A loaded base case
in the Powerflow “database engine” provides the data for the report.

.. code::

  /* shreac.c
   * The following is an example of a COPE program, re-written
   * in the "C" programming language using the CFLOW library.
   * It does a Shunt Reactive Summary report on the currently
   * loaded case, for a user-entered zone. Output to gui T/W.
   */

   #include <stdio.h>
#include <string.h>
/*#include <cflowlib.h> /* use this form if cflowlib.h is in the
 "standard" include area for C */
#include "cflowlib.h" /* use this form if "cflowlib.h" is in a "user
library" include area. Your compile procedure
should use the "-I or /I" option that specifies
the path to "cflowlib.h" */
int readln( char *s, int lim ) /* Function to read input from the T/W. */
{
 int i;
 char c;
 for ( i=0; i < lim - 1 && ( c = getchar() ) != EOF && c != '\n'; ++i )
 s[i] = c;
 s[i] = '\0';
 return i;
}
main( int argc, char *argv[] ) /* Main Program */
{
 pf_rec r; /* CFLOW structure */
 char zn[3];
 int error, status ;
 float q_avail_react_tot, q_avail_cap_tot,
 q_used_react_tot, q_used_cap_tot,
 q_unused_react_tot, q_unused_cap_tot,
 q_unsched_react_tot, q_unsched_cap_tot;
 pf_cflow_init( argc, argv ); /* IPC connection function, required. */
/* Ask user for zone to report */
 printf("Enter Zone to report Shunt Reactive Summary > ");
 readln( zn, sizeof(zn) );
 zn[sizeof(zn)] = '\0';
 printf("\n\n Shunt Reactive Summary for Zone %s \n\n",zn);
 printf(" Avail_caps Avail_reac Used_caps Used_reac Unus_caps Unus_rx Unsch_caps
Unsch_rx\n\n");
 q_avail_react_tot = q_avail_cap_tot =
 q_used_react_tot = q_used_cap_tot =
 q_unused_react_tot = q_unused_cap_tot =
 q_unsched_react_tot = q_unsched_cap_tot = 0.0;
 /* Compute zone quantities */
 error = pf_rec_bus( &r, "F" ); /* get first bus in case */
 status = pf_rec_bus( &r, "O" ); /* get solution data for first bus */
 while ( !error && !status ) { /* Loop through all buses in case */
 if ( strcmp(r.i.ACbus.zone, zn )==0)
 { /* If bus is in the zone */
 q_avail_react_tot += r.s.ACbus.Bshunt_sch_rx;
 q_used_react_tot += r.s.ACbus.Bshunt_used_rx;
 q_avail_cap_tot += r.s.ACbus.Bshunt_sch_cap;
 q_used_cap_tot += r.s.ACbus.Bshunt_used_cap;
 if( r.s.ACbus.Qunsch < 0 ) {
 q_unsched_react_tot -= r.s.ACbus.Qunsch;
 }
 else
 {
 q_unsched_cap_tot += r.s.ACbus.Qunsch;
 }
 }
 error = pf_rec_bus( &r, "N" ); /* get next bus in case */
 status = pf_rec_bus( &r, "O" ); /* get solution data for next bus */
 }
 q_unused_react_tot = q_avail_react_tot - q_used_react_tot;
 q_unused_cap_tot = q_avail_cap_tot - q_used_cap_tot;
 /* Print zone summary */
 printf(" %6.1f %6.1f %6.1f %6.1f %6.1f %6.1f %6.1f %6.1f \n\n",
 q_avail_react_tot, q_avail_cap_tot,
 q_used_react_tot, q_used_cap_tot,
 q_unused_react_tot, q_unused_cap_tot,
 q_unsched_react_tot, q_unsched_cap_tot );
 pf_cflow_exit(); /* Drop IPC connection */

C programs usually have header files "included" somewhere near the beginning after a comment
header. Header files may be standard or programmer-created. One standard header file required is
stdio.h if the program does any I/O operations. Various macros and definitions make up these
files. string.h is also a standard header file. cflowlib.h is a special header file for the CFLOW
library. All CFLOW programs must include this file.
All C programs must include a call to main, which is where program execution starts.
Immediately after main follow a series of declarations of variables local to main. The variable r
is a special CFLOW structure of type pf_rec. This is the basic powerflow record structure used
to retrieve both input and output data for all types of records.
A three character array representing a two character bus zone code follows. (Strings should always
be declared one larger than needed to account for the terminating NULL (\0).) Two integer
variables, error and status, are declared next. These will be used to store the return value from
calls to pf_rec_bus. The variables to collect the zone total quantities are declared as float (single
precision real numbers).
pf_cflow_init is called to establish the IPC socket connection, which the program will use to
communicate with either the IPFSRV or IPFBAT programs.
We print a question to the terminal window, and use readln to retrieve the user selection of a zone to report on, and make sure the string is null-terminated by storing a null in the last element of the
array (remember that C indexes array elements from 0 to n-1). The input zone id is echoed back
in the heading of the report, and the floating point variables are initialized.
Now we begin the actual processing. The first bus record is retrieved by calling pf_rec_bus with
an action code of "F" (for First). This stores the input record data for the first bus in the currently
loaded system in the local structure r. But we want the output (solution) quantities. So we call
pf_rec_bus again, with action code "O" (for Output). The necessary id fields have been stored in
r by the first call, and these are passed back to ipfsrv so it knows what bus you want output values
for.
A while loop now executes. The purpose of the while loop is to sequentially access Powerflow bus
records and gather floating point data related to the shunt reactance. pf_rec_bus initially used
action code "F" to go to the first bus record in the base case. After this, pf_rec_bus uses action
code "N" to retrieve the next bus record. When the end of all records has been reached, pf_rec_bus
returns a -1, which causes the while loop to terminate.
Since only records from the user-specified zone are desired in this program, a test is first performed
on each bus record to see if it has the correct zone. The program could be made more efficient by
not bothering to retrieve output values except for the right buses; however it has been left this way
for simplicity in providing an example. If the bus is in the right zone, then the appropriate floating
point values are totaled in assignment statements. This program assumes that the zone has no dc
buses; if there are any, then the dc solution variables stored in the same fields as the ac shunt would
give you weird results, to say the least! Of course, it would be possible to also test for bus type,
along with testing for the zone, in order to avoid this problem.
When the loop teminates, the reactance totals are printed to the screen with printf statements
formatted for decimal output. Then we exit, and release the socket connection, by calling
pf_cflow_exit.

Standard Line Flow Summary Example
==================================
The slfs.c program was translated from a COPE procedure used by WSCC Technical Staff. It
reads an input data file of headings and branches to be reported, looks these up in the currently
loaded solved system, and sends the report to a file. The report includes the input headings, and
group totals where called for, in an attractive format for printing.
This is what the input data file looks like. Heading lines are those with neither ’LIN’ or ’TOT’ on
them. They are printed as encountered. ’LIN’ cards identify a branch to be reported; ’TOT’ cards
call for a total to be printed. The "2" in column 5 indicates that the reverse flow is to be reported.

  CANADA AND NORTHWEST
  --------------------
    1. Alberta - British Columbia
  LIN LANGDON 500 CBK500 500
  LIN LNGDN500 500 CRANBROK 500
  LIN LNGDN500 500 CBK500 500
  LIN 2 POCA TAP 138 EMC138 138
  TOTAL
    2. Canada - Northwest
  LIN ING500 500 CUSTER W 500
  LIN INGLEDOW 500 CUSTER W 500
  LIN 2 NLY230 230 BOUNDARY 230
  LIN 2 NELWAY 230 BOUNDARY 230
  LIN 2 NLYPHS 230 BOUNDARY 230
  LIN SELPHS-1 230 MARSHALL 230
  LIN SELPHS-2 230 MARSHALL 230
LIN SELPHS-1 230 BEACON N 230
LIN SELPHS-2 230 BEACON S 230
TOTAL
 3. Northwest - California
LIN MALIN 500 ROUND MT500.
LIN DELTA 115 CASCADE 115.
LIN CAPTJACK 500 OLINDA 500.
TOTAL
 4. Celilo - Sylmar
LIN 2 SYLMAR2I106. SYLMARLA230.
LIN 2 SYLMAR1I106. SYLMARLA230.
LIN 2 SYLMAR2R106. SYLMARLA230.
LIN 2 SYLMAR1R106. SYLMARLA230.
TOTAL
NORTHEAST
---------
 1. MPC High Line
LIN 2 CONRAD 115 CUT BANK115.
LIN GT FALLS 161 HAVRE 161.
 .
 .
 .

The program slfs.c prompts the user for the output file name to put the line flow listing in. The
input file name is hard-coded in the program.

.. code::

  /* slfs.c
  This CFLOW procedure looks up flows and creates a report
  of flows between buses as listed in an input data file.
  Before the CFLOW procedure is called, a solved power flow case
  must be resident in the powerflow server.
  This CFLOW procedure prompts the user for two file names:
  the output file name to put the line flow listing in,
  the input file name of a file to get line data from.
  The input file has a LIN card for each branch to be monitored.
  If there are multiple lines between the same buses, slfs.c
  picks up all lines. A TOT card flags printing of total flow
since the last TOT card (or since the beginning). Input lines
 without either LIN or TOT are printed directly to the output file.
 Each data card has LIN in columns 1-3 and the Branch identifiers
 in columns 7 to 31. First Bus name and KV in columns 7-18,
 Second bus name and KV in columns 20-31. Column 5 is a flag to
 tell slfs.c whether to use the "Pin" or "Pout" data quantity for
 the total flow. If the flag is "2", then "Pout" is used; otherwise
 "Pin" is used. The flag corresponds to the metering point.
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cflowlib.h"
/* cf_debug = 1; /* Put this where you want the buffers displayed,for debugging */
int readln( char *s, int lim ) /* Read a line from the terminal input */
{
 int i;
 char c;
 for ( i=0; i < lim - 1 && ( c = getchar() ) != EOF && c != '\n'; ++i )
 s[i] = c;
 s[i] = '\0';
 return i;
}
int freadln( FILE *fp, char *s, int lim ) /* Read a line from a file */
{
 int i;
 char c;
 for ( i=0; i < lim - 1 && ( c = getc( fp ) ) != EOF && c != '\n'; ++i )
 s[i] = c;
 s[i] = '\0';
 return i;
}
main ( int argc, char *argv[] )
{
 pf_comments comments; /* CFLOW structure */
 pf_rec r; /* CFLOW structure */
 FILE *out, *dat;
 char card[82], cout[31];
 char skv1[5], skv2[5];
 char direc;
 int error, lin, tot, head;
 float brflow, totalflow = 0.0;
 float kv1, kv2;
 pf_cflow_init( argc, argv );
/* Open the input file. This could be prompted for, but Don says
 he always uses the same input file, which will NOT be in the
 execution directory,so full pathname is supplied.
 If the file cannot be opened, the program terminates
 */
 dat=fopen( "/home/ipf/cflow_progs/slfs.dat", "r" );
 if ( dat == NULL ) {
 printf("Can't open data file\n");
 exit(0);
 }
 /* Prompt the user for the output file name, and open the file.
 Normally, file will be created in the execution directory,
 so cout is only 30 characters.
 If the file cannot be opened, the program terminates.
 */
 printf("Enter output file name > ");
 readln( cout, sizeof(cout) );
 printf("\n");
 out=fopen( cout, "w" );
 if ( out == NULL ) {
 printf("Can't open output file\n");
 exit(0);
 }
 /* Retrieve caseid and description, and print heading */
 error = pf_rec_comments( &comments, "G" );
 fprintf(out,"\n%s\n\n", &comments.h[0][33] );
 totalflow = 0.0;
 /* Loop for every line in the data file
 Read a line from the data file, if not "LIN" or "TOT" then
 print the text as is.
 If "LIN" retrieve and print line data.
 If "TOT" print totalflow and reinitialize it.
 */
 /* "C" array indices start at zero; below we use explicit starting index
 of 1 for "card" character array (string), so that column numbers
 will match array indices.
 */
 while ( freadln( dat, &card[1], sizeof( card ) - 1 ) )
 {
 lin = (!( strncmp( &card[1], "LIN", 3 )) );
 tot = (!( strncmp( &card[1], "TOT", 3 )) );
 head = lin + tot; /* = 0 if not either */
 if (head == 0) /* Heading card - print the text */
 {
 fprintf(out,"%s\n",&card[1]); /* Print from col. 1 - 1st char. is NULL! */
 } /* end if Heading */
 if (lin) /* LIN data card - process line */
 {
 /* Using CFLOW pf_init_branch function; bus kv’s must be real numbers */
 strncpy(skv1, &card[15], 4 ); skv1[4] = '\0';
 strncpy(skv2, &card[28], 4 ); skv2[4] = '\0';
 kv1 = atof(skv1);
 kv2 = atof(skv2);
/* pf_init_branch stores the passed ID fields in r, and zeroes all the other fields */
 pf_init_branch ( &r, "L", &card[7],kv1,&card[20],kv2,"*",0);
 /* Call CFLOW pf_rec_branch to retrieve output solution data */
 error = pf_rec_branch( &r, "O" );
 /* If branch not found, do not print anything. This happens quite often, since
 the input file is canned, and used on all cases. */
 if ( error ) { continue; } /* This sends it back to the ’while’ */
 /* Otherwise, retrieve flows. r is defined in this pgm as a structure of type pf_rec
 i indicates input data, s indicates output data. See cflowlib.h for definitions.
 */
 direc = card[5]; /* Meter flag for Pin or Pout */
 if ( direc == '2' )
 { /* meter at second bus */
 brflow = r.s.branch.Pout;
 totalflow += brflow;
 fprintf( out,"%s - %s(M) %s kV num ckts %d %7.1f\n",
 r.i.branch.bus1_name, r.i.branch.bus2_name, skv2,
 r.s.branch.num_ckt, brflow );
 }
 else /* meter at first bus */
 {
 brflow = r.s.branch.Pin;
 totalflow += brflow;
 fprintf( out,"%s(M) - %s %s kV num ckts %d %7.1f \n",
 r.i.branch.bus1_name, r.i.branch.bus2_name, skv2,
 r.s.branch.num_ckt, brflow );
 }
 } /* end if LIN */
 if (tot) /* TOT data card */
 {
 fprintf(out,
 "\n Total flow is %7.1f \n\n",totalflow);
 totalflow = 0.0 ;
 } /* end else if TOT */
} /* end while */
 pf_cflow_exit();
} /* end main */

Note that both input values (r.i.branch.) and output values (r.s.branch.) are reported. The "i"
stands for "input" and the "s" stands for "solution".

The variable cf_debug is provided for convenience in debugging a CFLOW program. When it is
"true" (set non-zero), all the input and output buffers will be dumped to the terminal window, so
you can see exactly what your program is sending and getting back. Since this can be very
voluminous, you would only want to turn it on in the area where you are having a problem.

increm Program
==============
The INCREM program was translated from a COPE procedure used by WSCC Technical Staff. It
reads an input data file of branches to be reported, and another file of buses to change generation
on. The starting case is hard-coded, and so is the bus that you want to study power transfer from,
in this case GADSBY 3 13.8. For each bus in the second file, the generation at GADSBY is
increased, that of the other bus decreased, area intertie schedules are adjusted as necessary, the case
is solved, and the flow is retrieved for all the branches in the first input file and stored in an array.
The report goes to a file; it consists mostly of a matrix showing the effect of the generation changes
on the monitored line flows.
The COPE procedure used the IPS feature INCREM to accomplish this task. IPF has no built-in
incrementals function, so the CFLOW program just does what is described in the paragraph above.
It is not a general-use incremental program mimicking the IPS function.
For simplicity, a lot of names are hard-coded in this program. The user would have to decide
whether it would be more efficient to change them in the code for each study, or fix the code to be
general and then have to type them in over again for each run.

.. code::

  /* increm.c
 This CFLOW procedure creates an incremental line flow listing of
 selected lines, sorted by areas. The incremental flows are computed
 as the change in flows from the base case to the incremental case.
 Several incremental cases can be submitted. The limit here is 10,
 compared with 64 for the INCREM COPE procedure for IPS. However, this
 limit can be extended with attendent changes in the incremental
 storage arrays and in the output reports.
 Two aspects makes this CFLOW procedure more complicated than the COPE
 equivalent.
 1. IPF uses area intertie "I" records to define the net area export.
 If these records are present, an interarea transfer is effected
 only by changing the scheduled interarea export. If "I" records exist
 but the particular Area1-Area2 "I" record does not exist, then a
 new "I" record must be added for Area1-Area2 with an export value
 of the desired transfer. If no "I" records exist, then the interarea
 transfer is effected by the ordinary means, namely, by increasing the
 area1 export and decreasing the area2 export.
 2. Process INCREM does not exist in IPF. Consequently, the sorting
 and listing of branches was implemented entirely within this CFLOW
 procedure.
 The code is intentionally batch. It could be made interactive by
 prompting for file names and bus names. All file names and bus names
 are hard coded. Changing these requires re-editing, recompiling, and
 relinking the program. Fortunately, these steps can be performed in
 a short time.
 The program's execution plan is as follows.
 1. Load in base case history file.
 2. Open branch data file, bus data file, and output report file.
 3. Process the branch data file. For each branch, obtain the base case
 line flow.
 4. Process the bus data file to identify each transfer pair of buses:
 "busname1" and "busname2". The "busname1" is a hard-coded global
 variable.
 a. For each bus pair, perturb busname1's generation +100MW (and its
 associated areaname1's export +100MW) and busname2's generation
 -100MW (and its associated areaname2 export -100MW).
 b. Solve the case.
 c. Loop through the monitored branches, obtaining the line flows
 for the perturbed case.
 5. Print the output report.
 a. Use a branch index array "keysrt" in conjunction with a
 user-written compare routine (to be used with qsort) to obtain a
 double-entry list of monitored branches sorted by the following
 fields: area1, bus1, area2, bus2, id, and section.
 b. Print out the monitored lines flows using the sort index.
 */
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include "../cflow/cflowlib.h"
#include "../cflow/ft.h"
#define NULLC '\0'
#define MAXFLOWS 1000
#define MAXCASES 10
#define TRUE 1
#define FALSE 0
#define LINES_PER_PAGE 64
/* Declare global variables */
struct FLOWS {
 char bus1[13]; /* bus1, base1 field */
 char bus2[13]; /* bus2, base2 field */
 char id; /* id field */
 int section; /* section field */
 char area1[11]; /* area name corresponding to bus1 */
 char area2[11]; /* area name corresponding to bus2 */
 float Pin[MAXCASES+1]; /* Tie line flows in MWs */
 float Pout[MAXCASES+1]; /* Tie line flows in MWs */
 /* Declare hard-coded files and bus names */
char *progname = "increm";
char *basefile = "../cflow_progs/98hs3inc.bse";
char *branchfile = "../cflow_progs/line.dat";
char *busname1 = "GADSBY 313.8";
char *busfile = "../cflow_progs/name2.dat";
char *outfile = "98hs3inc.out";
/* declare function prototypes */
FILE *efopen (char *, char *);
int compare( const void *, const void * );
/* static int compare( const void *key1, const void *key2 ); */
int increment (char *, char *, float, int);
int get_bus_area (char *, char *);
int mod_bus_gen (char *, float);
int mod_area_export (char *, float);
int mod_itie_export (char *, char *, float);
void title (int *, int *, FILE *, pf_comments *, int, char *);
FILE *efopen (char *file, char *mode) /* fopen file, return message */
 /* if can't */
{
 FILE *fp;
 if ((fp = fopen(file, mode)) != NULL)
 return fp;
 fprintf (stderr, " %s: can't open file %s mode %s\n",
 progname, file, mode);
 return NULL;
}
main(int argc, char *argv[])
{
 pf_comments c;
 pf_rec br, itie;
 int i, j, k, status, numcases, keysrt[2*MAXFLOWS], numbr, lineno = 0,
 pageno = 0, intertie_flag = FALSE, section;
 FILE *fp_busfile, *fp_branchfile, *fp_outfile;
 char id, base[5], busname2[13], oldarea[11], string[133], areaname1[11];
 pf_cflow_init( argc, argv);
 /* Load the history data file */
 status = pf_load_oldbase (basefile);
 if (status) {
 fprintf (stderr, " Unable to open history file %s status %d",
 basefile, status);
 pf_cflow_exit();
 return 1;
 }
 /* Determine if any Area Intertie "I" records exist in the base case */
 pf_rec_init (&itie, PF_REC); /* Initialize structure prior
 to calling pf_rec_itie.
 PF_REC is in header file "ft.h" */
 status = pf_rec_itie (&itie, "F");
 if (status == 0) intertie_flag = TRUE; /* If success, "I" records exist */
 /* Obtain case comments */
 pf_rec_init (&c, PF_REC); /* Initialize structure prior
 to calling pf_rec_comments.
 PF_REC is in header file "ft.h" */
 status = pf_rec_comments (&c, "G");
 /* Open the monitored branch data file for read only */
 fp_branchfile = efopen(branchfile, "r");
 if (fp_branchfile == NULL) {
 fprintf (stderr, " Unable to open monitored branch file %s \n",
 branchfile);
 pf_cflow_exit();
 return 1;
 }
 /* Open the bus data file for read only */
 fp_busfile = efopen(busfile, "r");
 if (fp_busfile == NULL) {
 fprintf (stderr, " Unable to open buses list file %s \n",
 busfile);
 pf_cflow_exit();
 return 1;
 }
 /* Open the output report file for write only */
 fp_outfile = efopen(outfile, "w");
 if (fp_outfile == NULL) {
 fprintf (stderr, " Unable to open INCREM output report file %s \n",
 outfile);
 pf_cflow_exit();
 return 1;
 }
 fprintf (fp_outfile, " Base case from history file %s \n", basefile);
 for (i=0; i < 3; i++) {
 if (strlen (c.h[i]) > 0)
 fprintf (fp_outfile, " %s \n", &c.h[i][1]);
 }
 /*
 Process each record in the monitored branch file. The procedure invokes
 the following steps.
 1. Parse the ID fields of each branch entity into the branch data
 structure.
2. A call to routine "pf_rec_branch" with appropriate arguments will
 obtain the branch output quantitys Pin and Pout.
 3. Parse the ID fields again of each branch entity into arrays used
 for sorting and printing the output report. The "area1" and "area2"
 arrays are obtained indirectly through the zone. First the zone is
 obtained from the bus data structure of each terminal bus. From each
 zone, the corresponding the area name is obtained via the call
 "pf_area_of_zone".
 4. An slightly unusual convention is employed here for subscrip "numbr".
 Normally, a C-arrays begins with subscript "0". Here, it begins with
 subscript "1". The reason is that it is necessary to distinquish
 normal branches from transposed branches in the "keysrt" array, which
 will be built after all lines have been read in and all incremental
 cases have been processed. The distinction is done by positive
 and negative subscripts: +n denotes the normal orientation, -n its
 transpose. (C cannot distinquish between +0 and -0.)
 */
 fprintf (stderr, " Reading in monitored line data \n");
 numbr = 0;
 while ( fgets (string, 132, fp_branchfile) != NULL) {
 /*
 Parse the branch data id fields from "string" into structure
 "br.i.branch" and call "pf_rec_branch" with the "O" option to
 retrieve the base case flow.
 Copy the parsed branch data id fields into a second structure
 "br_flow[]" for storing the flow results.
 Note that the strncpy command requires that the strings be
 explicitly null-terminated.
 */
 pf_rec_init (&br, PF_REC); /* Initialize structure prior
 to calling pf_rec_branch.
 PF_REC is in header file "ft.h" */
 strcpy (br.i.branch.type, "L ");
 strncpy (br.i.branch.bus1_name, &string[6], 8);
 br.i.branch.bus1_name[8] = NULLC;
 strncpy (base, &string[14], 4);
 base[4] = NULLC;
 br.i.branch.bus1_kv = atof (base);
 strncpy (br.i.branch.bus2_name, &string[19], 8);
 br.i.branch.bus2_name[8] = NULLC;
 strncpy (base, &string[27], 4);
 base[4] = NULLC;
 br.i.branch.bus2_kv = atof (base);
 id = string[31];
 if (id == '\n' || id == '\t' || id == '\0') id = ' ';
 br.i.branch.ckt_id = id;
 if (strlen (string) > 32) {
 base[0] = string[32];
 base[1] = NULLC;
 section = atoi (base);
 } else {
 section = 0;
 }
 br.i.branch.section = section;
 /*
 The branch data is now loaded in structure "branch". A call to
 "pf_rec_branch" will retrieve the desired information.
 */
 status = pf_rec_branch (&br, "O");
 if (!status) {
 numbr++;
 strcpy (br_flow[numbr].bus1, &string[6]);
 br_flow[numbr].bus1[12] = NULLC;
 strcpy (br_flow[numbr].bus2, &string[19]);
 br_flow[numbr].bus2[12] = NULLC;
 br_flow[numbr].id = br.i.branch.ckt_id;
 br_flow[numbr].section = br.i.branch.section;
 br_flow[numbr].Pin[0] = br.s.branch.Pin;
 br_flow[numbr].Pout[0] = br.s.branch.Pout;
 /* Get the area name associated with bus1 indirectly through the
 zone of bus1 */
 status = get_bus_area (br_flow[numbr].bus1, br_flow[numbr].area1);
 if (status) {
 fprintf (stderr, " Unable to retrieve area associated with bus1 of \
monitored branch %s %s \n", br_flow[numbr].bus1, br_flow[numbr].bus2);
pf_cflow_exit();
return 1;
 }
 /* Get the area name associated with bus2 indirectly through the
 zone of bus2 */
 status = get_bus_area (br_flow[numbr].bus2, br_flow[numbr].area2);
 if (status) {
 fprintf (stderr, " Unable to retrieve area associated with bus2 of \
monitored branch %s %s \n", br_flow[numbr].bus1, br_flow[numbr].bus2);
pf_cflow_exit();
return 1;
 }
 }
 }
 /* Begin the incremental bus loop. This is the list of buses in
 testdc3.bdat, processed one-by-one. */
 while ( fgets (string, 80, fp_busfile) != NULL) {
 strncpy (busname2, string, 12);
 busname2[12] = NULLC;
 status = increment (busname1, busname2, 100.0, intertie_flag);
 if (!status) {
     numcases++;
 /* Solve the modified case */
 status = pf_solution ();
 /* Write change case comments */
 if (status) {
fprintf (fp_outfile, "\n Incremental case number %d failed \n",
 numcases);
fprintf (fp_outfile, " 100 MW scheduled from \"%s\" to \"%s\"\n",
 busname1, busname2);
fprintf (stderr, " Incremental case number %d failed \n",
 numcases);
for (i = 1; i <= numbr; i++) {
 br_flow[i].Pin[numcases] = 0.0;
 br_flow[i].Pout[numcases] = 0.0;
 }
 } else {
fprintf (fp_outfile, "\n Incremental case number %d \n", numcases);
fprintf (fp_outfile, " 100 MW scheduled from \"%s\" to \"%s\"\n",
 busname1, busname2);
fprintf (stderr, "\n Incremental case number %d \n", numcases);
fprintf (stderr, " 100 MW scheduled from \"%s\" to \"%s\"\n",
 busname1, busname2);
for (i = 1; i <= numbr; i++) {
 pf_rec_init (&br, PF_REC); /* Initialize structure prior
 to calling pf_rec_branch.
 PF_REC is in header file "ft.h" */
 strcpy (br.i.branch.type, "L ");
 strncpy (br.i.branch.bus1_name, br_flow[i].bus1, 8);
 br.i.branch.bus1_name[8] = NULLC;
 br.i.branch.bus1_kv = atof (&br_flow[i].bus1[8]);
 strncpy (br.i.branch.bus2_name, br_flow[i].bus2, 8);
 br.i.branch.bus2_name[8] = NULLC;
 br.i.branch.bus2_kv = atof (&br_flow[i].bus2[8]);
 br.i.branch.ckt_id = br_flow[i].id;
 status = pf_rec_branch (&br, "O");
 br_flow[i].Pin[numcases] = br.s.branch.Pin;
 br_flow[i].Pout[numcases] = br.s.branch.Pout;
}
 }
 }
 }
 /* Obtain sorted double entry index "keysrt" for branch flows
 keysrt[i] > 0 means "br_flow" is processed as is
 < 0 means "br_flow" is processed transposed
 */
 for (i = 0; i < numbr; i++) {
 keysrt[2*i] = i + 1;
 keysrt[2*i+1] = -keysrt[2*i];
 }
 qsort ( keysrt, 2*numbr, sizeof (keysrt[0]), compare );
 title (&lineno, &pageno, fp_outfile, &c, numcases, "");
 oldarea[0] = NULLC;
 for (i = 0; i < 2*numbr; i++) {
 j = keysrt[i];
 if (j > 0) {
 strcpy (areaname1, br_flow[j].area1);
 } else {
 strcpy (areaname1, br_flow[-j].area2);
 }
 if (strcmp (areaname1, oldarea) != 0) {
 if (lineno+4 > LINES_PER_PAGE) {
 lineno = 0;
 title (&lineno, &pageno, fp_outfile, &c, numcases, areaname1);
 } else {
 fprintf (fp_outfile, "\n From area %s \n\n", areaname1);
 lineno += 3;
 }
 strcpy (oldarea, areaname1);
 }
 if (lineno+1 > LINES_PER_PAGE) {
 lineno = 0;
 title (&lineno, &pageno, fp_outfile, &c, numcases, areaname1);
 }
 if (j > 0) {
 fprintf (fp_outfile, " %s %s %c %s %8.2f ", br_flow[j].bus1,
br_flow[j].bus2, br_flow[j].id, br_flow[j].area2,
br_flow[j].Pin[0]);
 for (k = 1; k <= numcases; k++) {
fprintf (fp_outfile, " %7.2f", br_flow[j].Pin[k] - br_flow[j].Pin[0]);
 }
 } else {
 fprintf (fp_outfile, " %s %s %c %s %8.2f ", br_flow[-j].bus2,
br_flow[-j].bus1, br_flow[-j].id, br_flow[-j].area1,
-br_flow[-j].Pout[0]);
 for (k = 1; k <= numcases; k++) {
fprintf (fp_outfile, " %7.2f",
 -br_flow[-j].Pout[k] + br_flow[-j].Pout[0]);
 }
 }
 fprintf (fp_outfile, "\n");
 lineno++;
 }
 pf_cflow_exit();
 fclose (fp_busfile);
 fclose (fp_branchfile);
 fclose (fp_outfile);
 if (!status) {
 exit (0);
 } else {
 fprintf (stderr, " %s aborted with errors \n", progname);
  exit (1);
 }
}
int get_bus_area (char *name, char *area)
/* This routine returns the area name given the bus name. It is obtained
 indirectly through the bus bus data structure. First, essential information
 is inserted into the bus data structure and the remaining data in the
 structure is obtained after calling "pf_rec_bus". From the zone name in
 the bus structure, the area name is obtained by calling "pf_area_of_zone".
 */
{
 pf_rec bus;
 char base[5], zone[3];
 int len, status;
 pf_rec_init (&bus, PF_REC); /* Initialize structure prior
 to calling pf_rec_bus.
 PF_REC is in header file "ft.h" */
 strcpy (bus.i.ACbus.type, "B ");
 strncpy (bus.i.ACbus.name, name, 8);
 bus.i.ACbus.name[8] = NULLC;
 strcpy (base, &name[8]);
 bus.i.ACbus.kv = atof (base);
 status = pf_rec_bus (&bus, "G");
 if (status) {
 fprintf (stderr, " Bus %s is not in history file %s \n", name, basefile);
 return 1;
 }
 strncpy (zone, bus.i.ACbus.zone, 2);
 zone[2] = NULLC;
 status = pf_area_of_zone (area, zone);
 if (status) {
 fprintf (stderr, " No area data in history file %s \n", basefile);
 return 1;
 }
 return 0;
}
int increment (char *busname1, char *busname2, float delta_gen, int flag)
/* This routine applies +/- delta_gen to pairs of buses, areas, and/or
 intertie records to effect the desired transfer. */
{
 pf_rec b, a, itie;
 char areaname1[11], areaname2[11];
 int status;
 /* The following are declared "static" such that they can be initialized
 to NULL, are local, and are not volatile, i.e., they retain their value
 after the routine is executed. In FORTRANese, they are similar to local
 variables, initialized with a DATA statement, and retained with a
 SAVE statement.
  */
 static char oldbus1[13] = "", oldarea1[11] = "", oldbus2[13] = "",
 oldarea2[13] = "";
 /* Get areaname1 associated with busname1 */
 status = get_bus_area (busname1, areaname1);
 if (status) {
 fprintf (stderr, " Unable to retrieve area associated with bus %s \n",
 busname1);
 pf_cflow_exit();
 return 1;
 }
 /* Get areaname2 associated with busname2 */
 status = get_bus_area (busname2, areaname2);
 if (status) {
 fprintf (stderr, " Unable to retrieve area associated with bus %s \n",
 busname2);
 pf_cflow_exit();
 return 1;
 }
 /* Restore original Pgen on "oldbus1" and modify Pgen on "busname1".
 In this case, restoring the original Pgen on "oldbus1" is not
 necessary because it never changes its name once it is assigned.
 The same is true for area "oldarea1" and for intertie "oldarea1-oldarea2".
 This extra logic (two lines in each instance) is added for generality.
 */
 if (strcmp (busname1, oldbus1) != 0) {
 if (strlen (oldbus1) > 0)
 status = mod_bus_gen (oldbus1, -delta_gen);
 status = mod_bus_gen (busname1, delta_gen);
 }
 /* Restore original Pgen on oldbus2 and modify Pgen on busname2 */
 if (strcmp (busname2, oldbus2) != 0) {
 if (strlen (oldbus2) > 0)
 status = mod_bus_gen (oldbus2, delta_gen);
 status = mod_bus_gen (busname2, -delta_gen);
 }
 if (flag) {
 /* Area Intertie "I" records exist and these records define the net
 area interchange export. Restore the original export on intertie
 "oldarea1-oldarea2" and modify the export on intertie
 "areaname1-areaname2" */
 if (strlen (oldarea1) > 0 && strlen (oldarea2) > 0)
 status = mod_itie_export (oldarea1, oldarea2, -delta_gen);
 status = mod_itie_export (areaname1, areaname2, delta_gen);
 } else {
     /* Area intertie "I" records do not exist. Work directly on the
 area records. Modify export on each area "areaname1" and
 "areaname2" */
 if (strlen (oldarea1) > 0)
 status = mod_area_export (oldarea1, -delta_gen);
 status = mod_area_export (areaname1, delta_gen);
 if (strlen (oldarea2) > 0)
 status = mod_area_export (oldarea2, delta_gen);
 status = mod_area_export (areaname2, -delta_gen);
 }
 strcpy (oldbus1, busname1);
 strcpy (oldbus2, busname2);
 strcpy (oldarea1, areaname1);
 strcpy (oldarea2, areaname2);
 return status;
}
int mod_bus_gen (char *busname, float delta_gen)
/* This routine changes Pgen on bus "busname" an amount "delta_pgen" */
{
 pf_rec b;
 int status;
 pf_rec_init (&b, PF_REC); /* Initialize structure prior
 to calling pf_rec_bus.
 PF_REC is in header file "ft.h" */
 strcpy (b.i.ACbus.type, "B ");
 strncpy (b.i.ACbus.name, busname, 8);
 b.i.ACbus.name[8] = NULLC;
 b.i.ACbus.kv = atof (&busname[8]);
 status = pf_rec_bus (&b, "G");
 if (status) {
 fprintf (stderr, " Bus %s is not in history file %s \n",
 busname, basefile);
 pf_cflow_exit();
 return 1;
 }
 /* Add "delta_gen" to bus generation */
 b.i.ACbus.Pgen = b.i.ACbus.Pgen + delta_gen;
 status = pf_rec_bus (&b, "M");
 return status;
}
int mod_area_export (char *areaname1, float delta_export)
/* This routine changes Export on Area "areaname1" an amount "delta_export" */
{
 pf_rec a;
 int status;
  pf_rec_init (&a, PF_REC); /* Initialize structure prior
 to calling pf_rec_bus.
 PF_REC is in header file "ft.h" */
 strcpy (a.i.area.type, "A ");
 strcpy (a.i.area.name, areaname1);
 status = pf_rec_area (&a, "G");
 if (status) {
 fprintf (stderr, " Area %s is not in history file %s \n",
 a.i.area.name, basefile);
 pf_cflow_exit();
 return 1;
 }
 /* Add "delta_export" to area export */
 a.i.area.sched_export = a.i.area.sched_export + delta_export;
 status = pf_rec_area (&a, "M");
 return status;
}
int mod_itie_export (char *areaname1, char *areaname2, float delta_export)
/* This routine changes Export on area intertie "areaname1-areaname2" and
 amount "delta_export" */
{
 pf_rec itie;
 int status;
 pf_rec_init (&itie, PF_REC); /* Initialize structure prior
 to calling pf_rec_itie.
 PF_REC is in header file "ft.h" */
 if (strcmp (areaname1, areaname2) == 0) {
 return 0;
 } else {
 strcpy (itie.i.itie.type, "I ");
 strcpy (itie.i.itie.area1_name, areaname1);
 strcpy (itie.i.itie.area2_name, areaname2);
 status = pf_rec_itie (&itie, "G");
 if (status) {
 itie.i.itie.sched_export = delta_export;
 status = pf_rec_itie (&itie, "A"); /* Add a new "I" record */
 } else {
 itie.i.itie.sched_export = itie.i.itie.sched_export + delta_export;
 status = pf_rec_itie (&itie, "M"); /* Modify existing "I" record */
 }
 return status;
 }
}
int compare( const void *key1, const void *key2 )
/* static int compare( const void *key1, const void *key2 ) */
/* This comparing function obtains the relative sort order of *key1 and
 *key2 upon the following fields:
 1. area1.
 2. bus1.
 3. area2.
 4. bus2.
 5. id.
 */
{
 int i = *((int *) key1), j = *((int *) key2), comp;
 char *area11, *area12, *area21, *area22, *bus11, *bus12, *bus21, *bus22,
 id1, id2;
 int sect1, sect2;
 if (i == j) {
 return 0;
 } else {
 if (i > 0) {
 area11 = br_flow[i].area1;
 area12 = br_flow[i].area2;
 bus11 = br_flow[i].bus1;
 bus12 = br_flow[i].bus2;
 id1 = br_flow[i].id;
 sect1 = br_flow[i].section;
 } else {
 area11 = br_flow[-i].area2;
 area12 = br_flow[-i].area1;
 bus11 = br_flow[-i].bus2;
 bus12 = br_flow[-i].bus1;
 id1 = br_flow[-i].id;
 sect1 = br_flow[-i].section;
 }
 if (j > 0) {
 area21 = br_flow[j].area1;
 area22 = br_flow[j].area2;
 bus21 = br_flow[j].bus1;
 bus22 = br_flow[j].bus2;
 id2 = br_flow[j].id;
 sect2 = br_flow[j].section;
 } else {
 area21 = br_flow[-j].area2;
 area22 = br_flow[-j].area1;
 bus21 = br_flow[-j].bus2;
 bus22 = br_flow[-j].bus1;
 id2 = br_flow[-j].id;
 sect2 = br_flow[-j].section;
 }
 comp = strcmp (area11, area21);
 if (comp == 0) comp = strcmp (bus11, bus21);
 if (comp == 0) comp = strcmp (area12, area22);
 if (comp == 0) comp = strcmp (bus12, bus22);
 if (comp == 0) comp = (unsigned int)id1 - (unsigned int)id2;
 if (comp == 0) comp = sect1 - sect2;
 return comp;
  }
}
void title ( int *lineno, int *pageno, FILE *fp_outfile, pf_comments *c,
 int numcases, char *areaname)
{
 /* Write base case comments */
 int i;
 fprintf (fp_outfile, "\f\n\n");
 (*pageno)++;
 for (i=0; i < 3; i++) {
 if (strlen (c->h[i]) > 0)
 fprintf (fp_outfile, " %s \n", &c->h[i][1]);
 (*lineno)++;
 }
 fprintf (fp_outfile, "\n INCREMENTAL report (DELTA) line flows in MW \
page No. %d \n\n", *pageno);
 fprintf (fp_outfile, " From bus To bus cir To area Base Flow ");
 for (i = 1; i <= numcases; i++) {
 fprintf (fp_outfile, "Incr %2d ", i);
 }
 (*lineno) += 6;
 if (strlen (areaname) > 0) {
 fprintf (fp_outfile, "\n From area %s \n\n", areaname);
 (*lineno) += 3;
 }
}
