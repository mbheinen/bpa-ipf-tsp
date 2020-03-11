***************************
Power Flow Control Language
***************************

Overview
========
This section describes the batch Power Flow Control Language (PFC) and its syntax, commands and subcommands. Command entries follow the PFC description in alphabetical order. The table below helps you turn quickly to a specific command entry. The table also gives you a quick description of all of the commands.

Each command entry explains the meaning of the command and gives its syntax. Some commands have subcommands, which are also described. Many entries have additional discussion, and some have examples, particularly where a command’s usage may not be immediately obvious.

The BPF Control Language
========================
The BPF Power Flow Control Language (PFC) consists of a sequence of program control statements, each of which in turn consists of commands, subcommands, keywords, and values. All statements have a reserved symbol in column 1 to identify a command or subcommand.

Every statement is scanned, and each command or subcommand found is compared with a dictionary in the program to find the relevant instructions. With the exception of the identifier in column 1 of each statement, PFC is free-form. All statements must be in the PFC file.

PFC has three levels of control, which are identified by one of three identifiers in column one.

1. The left parenthesis ``(`` identifies the top (or process) level of control. Only four commands are valid here — ``(POWERFLOW``, ``(NEXTCASE``, and ``(STOP`` or ``(END``.
2. The slash `/` identifies the second (or command) level of control. Many commands are valid here, and they are listed and described in this chapter. Commands generally enable or disable output options, define parameters needed for the process, etc. Subprocesses are major operations involving considerable processing and additional data. Only optional IPF processes are requested with these commands.
3. The right angle bracket ``>`` identifies the third (or subcommand) level of control. A few commands have subcommands associated with them. These subcommands are described in the associated command entries. These subcommands act as qualifiers for the second-level commands. 

In addition to the foregoing syntactic units, a command enabling a microfiche option is available. Its control symbol is the left square bracket (``[``)

 .. image:: ../img/Hierarchical_Levels_of_PFC_Statements.png

Almost every PFC statement fits one of the following formats, and the few that do not are very similar.

.. note::

   Spaces can be used for readability. Commas are used to separate syntactic units such as a list of values or keyword/value assignments.
   
Most statements fit on one line, but some extend over multiple lines. These exceptions are noted. When used, put a hyphen (``-``) where you want to break and continue the command parameters starting in or after column 2 of the next line, column 1 must be blank.

Each general format is followed by an example.
 
 * A simple command with no keywords or values:

   .. code::

     / command
     / REDUCTION

 * A command assigned a simple keyword. (This is a “telescoped” syntax available for some commands.)

   .. code::
 
     / command = keyword 
     / AI_CONTROL = CON

 * A command followed by a comma with a keyword. (This is a “telescoped” syntax available for some commands.)

   .. code::

     / command , keyword 
     / F_INPUTLIST,NONE

 * A command followed by a comma with a value assigned to a keyword.

   .. code::

     / command , keyword = value 
     / P_ANALYSIS_RPT, LEVEL = 4

 * A command followed by a comma with multiple values assigned to a keyword. Note optional continuation with hyphen (``-``).

   .. code::

     / command , keyword = value , value , value ,... 
     / P_INPUT_LIST, ZONES=NA,NB,NC,ND,NE,NF, -
              NG,NH,NI,NJ,NK

  * A command followed by a comma with multiple value/keyword assignments.

   .. code::

     / command , keyword = value , keyword = value ,... 
     / MERGE_OLD_BASE, SUB_SYSTEM_ID = AREA-1, OLD_BASE = TESTDC.BAS
 
  * A command followed by a data record(s).

   .. code::

     / command data record 
     / NETWORK_DATA 
     B     GEN1 HI    230  2  -0.0  -0.0  0.0  0.0     -0.0 -0.0 -0.0 
     B     GEN2 HI    230  1  -0.0  -0.0  0.0 0.0      -0.0 -0.0 -0.0 
     B     GEN3 HI  230   2  -0.0  -0.0  0.0 0.0      -0.0 -0.0 -0.0
  
  * A subcommand followed by a comma with multiple comma-separated values.

   .. code::

     >subcommand, value, value, value ,... 
     >SAVE_ZONES,NA,NB,NC,ND,NE,NF,NG,NH,NI,NJ,NR

  * A subcommand followed by a data record on the next line.

   .. code::

     >subcommand
     data record 
     >SAVE_AREAS
     A AREA 2

Special Characters
==================
