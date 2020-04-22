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

  * Brakakati, Nabajyoti. *The Waite Group’s Microsoft C Bible*. Howard W. Sams & Company, 1988. This MS-DOS environment reference book clearly describes ANSI C compatibility for each function.
  * Harbison, Samuel P. and Guy L. Steele. C: A Reference Manual. 3rd ed. Prentice-Hall, 1991. This book shows ANSI C facilities contrasted with traditional or alternate facilities. If you are well acquainted with C programming, but want to make sure your program complies with ANSI C, look here.
  * Johnsonbaugh, Richard and Martin Kalin. *Applications Programming in ANSI C*. MacMillan, 1990. This is a textbook used in beginning undergraduate college courses.
  * Kernighan, Brian W. and Dennis M. Ritchie. *The C Programming Language*. Second Edition. Prentice-Hall, 1988. This is the standard book for learning the language, updated to a second edition. Experienced programmers will do well with this book. Beginners should use Kochan’s book.
  * Kernighan, Brian W. and Rob Pike. *The UNIX Programming Environment*. Prentice-Hall, 1984. This book describes how to develop programs in the UNIX operating system.
  * Kochan, Stephen G. *Programming in ANSI C*. Howard W. Sams & Company, 1988. This book is a comprehensive tutorial for the beginning programmer.
  * Plauger, P. J. *The Standard C Library*. Prentice-Hall, 1992. This book shows you how to use the standard ANSI and ISO C library functions. It provides code examples for implementing many of the library functions.

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

From ``ipfbat``
---------------
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
In one window run your CFLOW program.::

  my_cflow -n -w 300 [ arg1 ] [ ... ]

Wait for the message “using socket nnnn” (this will happen when the pf_cflow_init() function
is executed), then in another window run the ``ipfsrv`` program.::

  ipfsrv -socket nnnn

where “nnnn” is the same as what the CFLOW program stated. The socket connection should
happen within a second.

You can then debug your program in one window while the ``ipfsrv`` program runs in the other.

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

.. literalinclude:: ../../cflow/shreac.c

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

We print a question to the terminal window, and use readln to retrieve the user selection of a zone
to report on, and make sure the string is null-terminated by storing a null in the last element of the
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
buses; if there are any, then the dc solution variables stored in the same fields as the AC shunt would
give you weird results, to say the least! Of course, it would be possible to also test for bus type,
along with testing for the zone, in order to avoid this problem.

When the loop terminates, the reactance totals are printed to the screen with printf statements
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
call for a total to be printed. The "2" in column 5 indicates that the reverse flow is to be reported.::

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

.. literalinclude:: ../../cflow/slfs.c

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

.. literalinclude:: ../../cflow/increm.c

libcflow API Reference
======================
.. doxygenindex:: ipfdoxygen