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
Two special characters are available to document the control stream or to improve readability.

 * A period (``.``) in column 1 of a record identifies a command comment and the record will be ignored by the processing. It is used to document a PFC file or to improve readability. This comment is only visible in a listing of the PFC file or in the editor used to create it. 
 * The underscore symbol ``_`` has no syntactic significance and may be used freely to punctuate a word for visual readability.
 
.. note::

  The hyphen or minus sign “-” and the underscore “_” symbol are different characters! Thus, ``P_O_W_E_R_F_L_O_W`` is the same as ``POWER_FLOW`` which is the equivalent of ``POWERFLOW``. ``OLD_BASE`` is the same as ``OLDBASE`` but not the same as ``OLD-BASE``, etc.

Default Convention
==================
All default values for a command are listed on the first line in the command descriptions. Various keywords are listed below the default values. Default values have been selected to satisfy a majority of users; therefore, their use is to invoke exceptions to standard conventions. Once a default value has been enabled, it remains in force for the duration of the process. There is one exception to this:

  ``/ P_INPUT_LIST``

After the first case has been processed, ``P_INPUT_LIST`` is set to ``NONE``. This conforms to the default philosophy of selecting all options that fulfill a majority of requirements.

Microfiche Control Statement
============================

  ``[ FICHE,COPIES = n ]`` or
  ``( FICHE,COPIES = n )``

This command requests "n" copies of microfiche listings to be made. If it is omitted, the fiche file is not saved. If ""n" is zero or omitted, no copies are made. When it is used, this control must be first in the job stream.

Level 1 PFC Commands
====================

``( POWERFLOW )``

  This command initiates the processing of the network which is defined with subsequent commands and subcommands.

``( NEXTCASE )``

  This is the same as ``( POWERFLOW )`` except that the base network to be processed is the current network. Changes are expected; otherwise, the same network is processed again with the same data and controls in memory from the previous case. ``( NEXT_CASE )`` cannot be the first command in a program control file.

``( END )`` or ``( STOP )``

  This stops the execution of the IPF program. Each network is processed with a ``( POWERFLOW )`` or ``( NEXTCASE )`` command. The first must always be ``( POWERFLOW )``. Several cases may be concatenated (stacked) in the following format:

    ``( POWERFLOW )`` statement for case 1
    ``( POWERFLOW )`` statement or ``( NEXTCASE )`` statement for case 2
    .
    .
    .
    ``( POWERFLOW )`` statement or ``( NEXTCASE )`` statement for case n
    ``( STOP )``

The following control statement and the optional keywords that go with it identify the ``OLD_BASE`` file, optionally perform miscellaneous temporary changes to ``OLDBASE``, set solution parameters, and solve the resultant network. 

  ``(POWERFLOW CASEID = <casename>, PROJECT = <projname>)``

``casename`` is a user-assigned 10-character identification for the case. ``projname`` is a user-assigned, 20-character identification for the project or study to which this case applies. (No blanks are allowed; use hyphens instead.) 

The following statement is used if the Powerflow solution is to be run starting with data and controls from the previous base case in residence.

  ``( NEXTCASE, CASEID = <casename>, PROJECT = <projname> )``

Note that ``/ OLD_BASE`` is not used with a ``( NEXTCASE )`` statement since a base data file is already in residence.

Level 2 and 3 PFC Commands
==========================
Each Level 2 statement starts with a slash (``/``) in the first position.

After the slash are keywords and/or values separated by a comma (``,``). Specific values are assigned to the keywords in the following format:

  ``keyword = value``

When a keyword is requesting a list, for example, a zone list, the list may be continued on the next record by leaving column 1 of that record blank or by putting a comma in column 1 and continuing the list.

Level 3 statements consist of subcommands that specify keyword values for second-level commands only. Each subcommand for level 3 statements starts with the right angle bracket (``>``) in column 1. After the right angle bracket are keywords and/or values separated by commas (``,``). Most often, specific values are assigned by following a keyword with an equal sign (``=``) and then the desired value.

PFC Commands
============
The rest of this chapter discusses all the PFC commands, in alphabetical order. Each command entry includes the details of syntax and usage. The more involved commands show examples of use. Refer to the table below to locate a PFC command quickly.

In the format statement for each command, the keywords and parameter values are all vertically aligned in the same column. The top row is the default value. Alternate value assignments such as ``ON`` or ``OFF`` are identified by the appropriate symbols and have the syntax ``keyword=value``.

Required text is shown in ``UPPER-CASE`` while parameter values specified by the user are printed in ``lower-case`` and usually enclosed by angle brackets, thus, ``<list>``. Angle brackets are omitted when they may cause confusion with the Level 3 control symbol.

The optional underscore symbol (``_``) may be used to break up words for visual readability. The computer will read the words as though they were not broken.

==== ============ ===============================
Page Command Name Description
==== ============ ===============================
4-10 AGC Emulates automatic generation control.
4-13 AI_LIST Speciﬁes detail in area interchange listing.
4-14 ANALYSIS_SELECT Selects analysis reports for printing or microﬁche.
4-16 BRANCH_DATA Speciﬁes a master branch and bus data ﬁle for base case.
4-17 BUS_SENSITIVITIES Calculates system response to capacitor switching operations.
4-22 CHANGE_BUS_TYPE Disables voltage control in system.
4-27 CHANGE_PARAMETERS Perturbs parameters for start of new solution.
4-33 CHANGES Speciﬁes system data change records.
4-34 COMMENT Speciﬁes comment records.
COMMON_MODE_ANALYSIS Analyzes the results from a series of common mode cases in the format of the OUTAGE_SIMULATION
4-36 F_ANALYSIS_RPT Speciﬁes report of zones or owners for microﬁche.
4-38 F_INPUT_LIST Lists input data on microﬁche.
4-39 F_OUTPUT_LIST Lists output data on microﬁche.
4-40 GEN_DROP Balances generation drop by picking up generation.
4-44 HEADER Speciﬁes header information for reports.
4-45 INCLUDE_CONTROL Speciﬁes a ﬁle for control commands.
4-46 LINE_EFF Deﬁnes minimum percent line loading for report.
4-47 LINE_SENSITIVITIES Determines line sensitivity by controlling LTC and AI_CONTROL.
4-50 %LOAD_DISTRIBUTION Converts constant power, etc., into user-speciﬁed MVA, etc.
4-63 LOSS_SENSITIVITIES Gives information about system losses.
4-65 MERGE_OLD_BASE and MERGE_NEW_BASE
Extracts information from two subsystems to create a new system.
4-71 MVA_BASE Changes base MVA to an assigned MVA.
4-72 NETWORK_DATA Speciﬁes bus and branch data.
4-73 NEW_BASE Deﬁnes the ﬁle name for a new case.
4-74 OI_LIST Lists ownership interchange.
4-75 OLD_BASE Speciﬁes a previously existing solved case as the new case to start with.
4-76 OUTAGE_SIMULATION Simulates line outages, load dropping, generator outages and rescheduling.
4-83 OVERLOAD_RPT Speciﬁes overload parameter limits for report.
4-84 P_ANALYSIS_RPT Creates an analysis report.
4-86 P_INPUT_LIST Lists input data on paper.
4-87 P_OUTPUT_LIST Lists output on paper.
4-89 REBUILD Rebuilds all data tables from current OLDBASE ﬁle.
4-90 REDUCTION Reduces a network.
4-95 RPT_SORT Sorts output data of solved network.
4-96 SAVE_FILE Creates various output ﬁles, including the SIF (Stability Interface File).
4-98 SOLUTION Enables solution options and post-solution processes.
4-104 SORT_ANALYSIS Controls sort order for analysis listings.
4-105 TRACE Monitors data to aid data veriﬁcation.
4-106 TRANSFER_SENSITIVITIES Causes analysis of transfer sensitivities.
4-107 TX_EFF Compares total and core transformer losses.
4-108 USER_ANALYSIS Generates custom analysis listings
==== ============ ===============================

AGC
===
This command emulates automatic generation control (AGC) in the solution algorithm. Under AGC, real power excursions on several generators from base values are allocated in proportion to their total excursion. This in effect distributes the slack bus real power excursions to a set of selected units. The slack bus excursion, which drives AGC, may be either a system slack bus or an area slack bus.

The individual AGC units are identified with type ``B`` (bus) records which follow the ``/ AGC`` command. columns (1:18) correspond with the original format. Beyond column 18, data is free field.

  ``B  <bus_name,base kV>  Pmin=<##>, Pmax=<##>, Pgen=<##>, %=<##>``

where

  ``Pmin`` Minimum generation in MW. Default value is 0.0.

  ``Pmax`` Maximum generation in MW. Default is Pmax, which is specified on the bus record.
 
  ``Pgen`` Base generation is MW, which is used to compute the excursions. Default is scheduled or actual MW from the base case.

  ``%``  Percentage. The default allocates% in proportion to Pmax

A maximum of 24 AGC units may be specified. One of the units must be a system or area slack bus. Usually, AGC schemes converge faster than non-AGC. The exception occurs when ``Pmin`` or ``Pmax`` limits are hit and some readjustment occurs.

General Description
-------------------
An example illustrates the concept. In Case 1, there are two generators, GEN1 and GEN2, with initial and final values shown in the table below.

============ ========= ======= ===========
Bus          Initial P Final P Excursion
============ ========= ======= ===========
GEN1 (slack) 1000      1442    442
GEN2         1000      1000    0
TOTAL        2000      2442    442
============ ========= ======= ===========

In Case 2, we apply AGC with 50% on each machine. Presuming that losses are unchanged (for simplicity), the initial and final values are shown in the table below.

============ ========= === ======= ===========
Bus          Initial P %   Final P Excursion
============ ========= === ======= ===========
GEN1 (slack) 1000      50  1221    221
GEN2         1000      50  1221    221
TOTAL        2000      100 2442    442
============ ========= === ======= ===========

**Notes and Restrictions**
A maximum of 24 generators are permitted. One of the generators must be a system slack bus or an area interchange bus. Recall that the dynamics which drive AGC comes from slack bus P excursions.

If any unit hits a limit, the remaining active units redistribute their percentages and continue AGC control.

The results are summarized in the listing ``AGC Control``. This listing is controlled with ``/ ANALYSIS_SELECT`` command.

  ``/ ANALYSIS_SELECT``
  ``> SUM%VAR``

If area interchange control is ON, all AGC units should reside in the same area. Violations of this rule are flagged with warning diagnostics.
AGC control will obscure the change in slack bus power shown in the tie line Summary of Area Interchange. The true slack bus effects within the area would be the aggregate effects of all AGC units. The area interchange summary obscures this effect.

When ``/ AGC``’s and ``/ GEN_DROP`` coexist, ``/ AGC`` operates with a higher priority. In actuality, the two should not coexist.

The validity of AGC can be verified in the analysis summary ``AGC Control``. In normal conditions, the scheduled and actual percentage participation should be equal.
If these quantities are not equal, it is usually because ``Pmax`` or ``Pmin`` limits have been hit. In this instance, a comment appears.

  ``Actual % / Sched % = ****.*``

All of the active units should have an individual ratio

AI_LIST
=======
This command controls the level of detail in the area interchange listing.

  ``/ AI_LIST = FULL``

``FULL`` is the default. The options are:

  ``FULL`` Area interchange matrix, Area slack bus summary, and tie line flows.
  
  ``MATRIX`` Area interchange matrix.
  
  ``TIELINE`` Tie line flow summary

ANALYSIS_SELECT
===============
This selects individual analysis reports for printing or microfiche. It supersedes ``/ F_ANALYSIS`` and ``/ P_ANALYSIS``. Unlike these commands which select groups of reports according to their “level” the ``/ ANALYSIS_SELECT`` command selects reports individually.

A solitary ``/ ANALYSIS_SELECT`` command defaults all analysis listings to no print/no fiche status.

Printing and/or microfiche are enabled with the commands: ``> FICHE`` and ``> PAPER``. These commands independently restrict the contents of ``FICHE`` or ``PAPER`` reports to subsets of Zones, Ownerships or Areas.

The desired analysis reports are individually selected using ``>`` commands containing abbreviated report names, e.g., ``> UNSCH``.

Each ``> (report)`` command accepts an optional ``F`` or ``P`` qualifier. This will restrict the selected report to Fiche or Print respectively. If neither appear, both ``F`` and ``P`` are presumed to be selected. For example, ``> UNSCH, P`` will print the unscheduled reactive report.

A special option exists on the ``> LINEFF`` report. Its entirety is:

.. literal::

 > LINEFF, SORT = BUS_NAME, OUTPUT=filename, FIELD_WIDTH =  132,F,P
                                                             80,F,P 
 >   VOLTAGE,
     OWNER,
     ZONE,

All quantities are optional.

.. literal::

  ``SORT`` Controls sorting by bus kV_name, owner_name, or name.

  ``OUTPUT`` Copies a duplicate report to the named file.

  ``FIELD_WIDTH`` Controls the report width or the named file.

The following is a full list of the ``/ ANALYSIS_SELECT`` command set.

.. literal::

  / ANALYSIS_SELECT 
  > FICHE,ZONES=<zone1,...>,AREAS=<area1,...>,OWNERS=<owner1,...> 
  > PAPER,ZONES=<zone1,...>,AREAS=<area1,...>,OWNERS=<owner1,...> 
  > USERAN - User-defined analysis listing. (Used with / USER_ANALYSIS.) 
  > UNSCH - Buses With Unscheduled Reactive. 
  > LOSSOWN - Total System Generations and Loads by Owner. 
  > SYSTEMZONE - System Generations,Loads,Losses and Shunts by Zones. 
  > UVOV - Undervoltage-Overvoltage Buses. 
  > LINELOAD - Transmission Lines Loaded Above xxx.x% of Ratings or with more than 30 degrees of electrical angle.
  > TRANLOAD - Transformers Loaded Above xxx.x% of Ratings.
  > TRANEX - Transformers Excited Above xxx.x% over Tap.
  > XSYSTEMLOSS - Transmission System Losses.
  > BPALOADS - BPA Industrial Loads.
  > DCSYSTEM - DC System.
  > SHUNTSUM - Shunt Reactive Summary.
  > SUMLTC - Summary of LTC Transformers. - Summary of LTC Reactive Utilization
  > SUMPHASE - Summary of Phase-shifters.
  > SUM%VAR - Summary of %Var-controlled buses. - Summary of AGC Control - Summary of Line Drop Compensation
  > SUMBX - Summary of Type BX buses. > SUMRAN - Summary of Adjustable Var compensation.
  > SERIESCOMP - Transmission Lines Containing Series Compensation.
  > BUS - Bus Quantities. > SPIN - Spinning Reserves.
  > LINEEFF - Transmission Line Efficiency Analysis. (Lines Loaded  Above xxx.x % of Nominal Ratings).
  > TRANEFF - Transformer Efficiency Analysis. - Total Losses Above xx.xx % of Nominal Ratings.
  > TRANLOSS - Transformer Efficiency Analysis - Core Losses Above xx.xx % of Nominal Ratings.

BRANCH_DATA
===========

.. literal::

  / BRANCH_DATA, FILE = <filespec>, DATE = <myy>,                
                 BUSDATA_FILE = * 
                               <filespec>

This command specifies that the base case will be established from a master branch data file and associated bus data file. Branch data selected from this file will have an energization date (date in) and a de-energization date (date out) corresponding with the ``DATE`` specified on the above command.

If ``BUSDATA_FILE`` is not specified or has parameter value ``*``, the program expects bus data to follow in the input stream.

See ``MERGE_OLD_BASE`` and ``MERGE_NEW_BASE`` for more information about branch data file merging. Using the ``MERGE_OLD_BASE`` and ``MERGE_NEW_BASE`` commands is preferred.

BUS_SENSITIVITIES
=================

  ``/ BUS_SENSITIVITIES``

The primary motive of sensitivity is to calculate the instantaneous system response to sudden capacitor switching operations. This is difficult to model in the Powerflow because all LTCs must be turned off. This may cause solution divergence because LTCs are an integral part of any DC system. This problem is circumvented using sensitivities.

By recalculating the Jacobian matrix, various constraints can be changed. The flexibility of these constraints is evident in the format of the sensitivity command.

.. literal::

  / BUS_SENSITIVITIES,LTC=ON,AI_CONTROL=CON,Q_SHUNT=ADJ,Q_GEN=ADJ
                   OFF,          OFF         FIXED     FIXED
                                 MON

The top line defines the default values.

The first two options correspond with the standard solution options. The second two options define the conditions in which type ``BQ`` and ``BG`` buses can operate holding constant voltage.

For example, enabling the option ``Q_SHUNT = FIXED``, type ``BQ`` buses have all shunt fixed. If there is no rotating machinery (:math:`Q_{max}` and :math:`Q_{min}` are zero), then the bus holds constant :math:`Q` (:math:`PQ`). Since type ``BG`` buses always have ``Q_shunt`` fixed, this option has no affect on generator buses.

Similarly, by enabling the option ``Q_GEN = FIXED``, type ``BQ`` and ``BG`` buses have all generation fixed and operate in state :math:`PQ`. Type ``BG`` buses will operate in state :math:`PQ`. If ``BQ`` buses have no shunt, they also will operate in state :math:`PQ`.

In order of time response, the generators respond within several seconds. Thus, ``Q_GEN`` will normally be adjustable. LTC’s, DC LTC’s, and switched capacitors are controlled by time-delayed voltage relays to minimize spurious operation.

  LTC’s 0.5 - 3.0 minutes

  DC LTC’s 5 seconds

  CAP/REACTORS:5 - 30 seconds

The slowest component is area interchange control. Its response time is 0.5 to 10 minutes.

By appropriate selection of options, the Jacobian matrix can represent nearly any time frame of response.

Selected Buses
--------------
Following the BUS_SEN``SITIVITIES record, individual buses are selected for perturbation. These buses are identified by the ``B`` formatted records that follow them. A maximum of 50 buses may be specified.

The perturbed quantity is identified by nonzero entities in one of the fields: ``P_load``, ``Q_load``, ``G_shunt``, ``B_shunt``, ``P_generation`` or ``Q_generation``.

The fields on the ``B``-blank record determine which sensitivity :math:`\frac{dP}{d\theta}`, :math:`\frac{dP}{dV}`, or :math:`\frac{dQ}{dV}` is computed.

.. image:: ../img/B-blank_Record_Sensitivity_Fields.png

========================== ============ =================== =====================
Sensitivity                Column 2     PLOAD, PSHUNT, PGEN QLOAD, QSHUNT, QSCHED
========================== ============ =================== =====================
:math:`\frac{dP}{d\theta}` Not required Required            Not required
:math:`\frac{dP}{dV}`      Literal: “V” Required            Not required
:math:`\frac{dQ}{dV}`      Not required Not required        Required 
:math:`\frac{dQ}{dV}`a     (blank)      (blank)             (blank)
========================== ============ =================== =====================

a. This is the default.

Repeat Sensitivities
--------------------
A powerful feature of the sensitivity process is the ability to refactor the Jacobian matrix under different control schemes. For example, one ``/ BUS_SENSITIVITIES`` record could enable only the ``Q_GEN`` option (exciters on, everything else off) for an instantaneous response. Following the necessary ``B`` formatted records a second ``/ BUS_SENSITIVITIES`` record could enable all options for a long term response. Assuming the same bus list is repeated, then a comparison between the two corresponding sensitivities would yield the short-term and long-term effects of the bus’s injection perturbation.

Example
-------
The following is an actual case. Bus ``OLYMPIA 230`` was specified for a -172 MVAR shunt application. If ``Q_Load`` or ``Q_Generator`` was specified, the actual ``Q_Perturbation`` would be -172 MVAR. For ``Q_Shunt``, the ``Q_Perturbation`` is calculated.

.. literal::

  Delta (Q)= Q_Shunt * V**2
  = -172 * (1.067)**2
  = -195.82 MVAR
  New_Voltage (kV) = Old_Voltage (kV) + Sensitivity * Delta_Q 
  = 245.45 kV + 0.0334 * (-195.82) 
  = 245.45 kV - 6.54 kV 
  = 238.91 kV

The correct computed value on the listing is 238.90 kV. The different figures in the example are due to round off.

The correlation with actual Powerflow cases is very close. The calculated voltage excursion -6.54 kV is within two percent of the actual excursion. The accuracy is significant because the actual and estimated voltages will differ 0.001 per unit at most!

Sample Deck Setup
-----------------

.. literal::
  
  (POWERFLOW,...) 
  .
  .
  .
  / CHANGES
  .
  .
  .
  / BUS_SENSITIVITIES,LTC=ON,AI_CON=OFF,Q_SHUNT=ADJ,Q_GEN=ADJ
  B    MONROE   500       316

Output

.. literal::

 BUS_SENSITIVITIES COMPUTED WITH THE FOLLOWING CONTROLS:

                 LTC CONTROL              ( )   OFF
                 --- -------              (X)   ON (FULL CONTROL     )
                                          ( )   ON (NO VOLTAGE CONTROL)

                 AI CONTROL               (X)   OFF
                 -- -------               ( )   CONTROL (DEFAULT)
                                          ( )   MONITOR

                 Q_SHUNT CONTROL          (X)   ADJUSTABLE
                 ------- -------          ( )   FIXED

                 Q_GENERATION CONTROL     (X)   ADJUSTABLE
                 ------------ -------     ( )   FIXED

0 MONROE    500.0 A  316.0 MVAR CHANGE IN SHUNT AT THIS BUS WILL CAUSE THE FOLLOWING VOLTAGE CHANGES

0       BUS       BASE    BUS       BASE    ----- DVOLT/DQ  -----   ------ BASE VOLTAGE -------    ------ NEW VOLTAGE ------
                                            (P.U./P.U.)   (KV/MVAR) (P.U. KV)             (KV)     (P.U.KV)            (KV)
        MV-SVC    19.6                      -0.0106     -0.0023        1.118             21.92      1.081             21.10        
        MONROE   500.0                       0.0061      0.0321        1.052            525.84      1.073            537.05        
        WRK 25    25.2                       0.0040      0.0010        1.041             26.22      1.054             26.59        
        MONROE   230.0                       0.0039      0.0092        1.037            238.54      1.051            241.77        
        GDK 4      4.0                       0.0039      0.0002        1.049              4.20      1.063              4.25        
        MONROE T 230.0                       0.0038      0.0090        1.035            237.98      1.048            241.12        
        RIM 12    12.6                       0.0037      0.0005        1.041             13.11      1.054             13.29        
        RYL 12    12.6                       0.0037      0.0005        0.996             12.55      1.009             12.71        
        WRK 60B2  60.0                       0.0037      0.0023        1.020             61.17      1.033             61.97        
        WRK 60B1  60.0                       0.0037      0.0023        1.020             61.22      1.033             62.01

CHANGE_BUS_TYPE
===============

  ``/ CHANGE_BUS_TYPE``

This command disables voltage control in selected areas of the system and performs bus type changes from a voltage control type to a more passive type. The changes it makes are permanent and apply to the case in residence. If this command appears before any system changes, the bus type changes will apply before the system changes, exempting any new or changed buses. If this command appears after any system changes, any new or changed buses will be subject to bus type changes invoked with this command. See Table 4-5.

An example is shown below.

.. literal::

  / CHANGE_BUS_TYPE, BQ=B,  BG=BQ, BT=B , BX=B , -
                     LTC = OFF, -
                     AREAS=<area_1,...>, -
                     ZONES=<zone_1,...>, -
                     LIST=ON

Bus type changes
----------------
Four types of buses may be changed: ``BQ``, ``BG``, ``BT`` and ``BX``. All possible bus type transitions are depicted above using the format ``<old_type> = <new_type>``.

The full repertoire is listed in the table below. (Note that some restrictions apply.)

========== ==================================
Change     Restriction
========== ==================================
BQ --> B   If PGEN = 0.0 and QGEN =0.0
BQ --> BF  If PGEN <= 0.0.
BQ --> BF* Unconditional.
BG --> BQ  If PGEN > 0.0.
BG --> B   If PGEN <= 0.0.
BG --> BF  If PGEN = 0.0.
BG --> BF* Unconditional.
BT --> B   (This deletes any adjacent LTCs which are controlling BT nodes.)
BX --> B
BX --> BF
BX --> BF*
========== ==================================

LTC Transformer Control
-----------------------
A bus type change BT=B will delete LTCs only if the controlled bus is type ``BT``. A more general option:

  ``LTC = OFF``

disables all LTCs within the specified area except for any dc commutating transformers.

LIST
----
The ``LIST`` parameter accepts two values -- ``ON`` and ``OFF``. The default is ``ON``. This applies to the ``CHANGE_BUS_TYPE`` summary where the initial and final state of each bus affected is depicted. Setting ``LIST=OFF`` is recommended for repetitious batch runs.

Excluded Buses
--------------
Means are available to exempt individual buses from type changes defined in the ``/ CHANGE_BUS_TYPE`` command. These buses are excluded with the following command:

.. literal::

  >EXCLUDE_BUS 
  B          name     base 
  B          name     base
  . . . 
  . . .

Line Drop Compensation
----------------------
This feature temporarily replaces the ordinary ``BG`` -> ``BC`` voltage control of a remote bus with a ``BG`` control of a compensated voltage, which is specified as a percentage within the step up transformer. This control scheme is valid only for this case, and may be introduced only within context of a ``CHANGE_BUS_TYPE`` command. In subsequent cases, these generators revert to their normal control mode

The target compensated voltage is defined with a computed voltage limit. That limit is derived from two base case terminal voltages -- the ``BG`` bus and the remote ``BC`` bus (the remote bus may be another type). The formula used is

.. math:: 

  V_{max} = V_{min} = PCT * V^{base}_{BG} + (1 - PCT) * V^base_{BC}

Example:

.. literal::

  / CHANGE_BUS_TYPE, BQ = B, BX = B, BG = BQ 
  ... 
  > LINE_DROP_COMPENSATION 
  BG COULEE 13.8, 70% 
  BG CHIEF JO13.8, 80%

**Restrictions on Line Drop Compensation**
The following restrictions apply to line drop compensation:

  * All buses selected for Line Drop Compensation must be type ``BG``. All buses selected are exempt from any bus type change ``BG`` -> ``BQ`` or ``BG`` -> ``B``.
  * The controlled remote bus must be immediately adjacent to the generator.
  * The specified percentage should be between 0 and 100%. A warning is issued if the specified percentage is outside this range.
  * A maximum of 20 generators may be selected for line drop compensation.
  * The line drop compensation is case specific. It defines the base solution, but is not saved on the base history data file.

Reactive Compensation
---------------------
This feature is similar to the Raindrop Compensation; it temporarily replaces the ordinary ``BG`` -> ``BC`` voltage control of a remote bus with a ``BG`` control of a compensated voltage, which is specified as the voltage drop from the bus terminal voltage computed with the generator reactive power in series with a user-specified impedance. This control scheme is valid only for this case, and may be introduced only within context of a ``CHANGE_BUS_TYPE`` command. In subsequent cases, these generators revert to their normal control mode

The target compensated voltage is defined with a computed voltage limit. That limit is derived from two base case terminal voltages -- the ``BG`` bus and the remote ``BC`` bus (the remote bus may be another type). The formula used is

.. math::

  V_{max} = V_{min} = V^{base}_{BG} - \frac{Q_{BG}}{V_{BG}} * X_{BC} \\
  X_{c} = \frac{PCT}{100} * \frac{100}{MVA_{base}}

where :math:`PCT` is the user-specified percentage and :math:`MVA_{base}` is the user-specified machine MVA.

Example:

.. literal::

  / CHANGE_BUS_TYPE, BQ = B, BX = B, BG = BQ 
  ... 
  > REACTIVE_COMPENSATION 
  BG COULEE  13.8, 5%, 100
  BG CHIEF JO13.8, 5%, 100

**Restrictions on Reactive Compensation**
The following restrictions apply to reactive compensation are identical to those which apply to line drop compensation:
 
  * All buses selected for Reactive Compensation must be type ``BG``. All buses selected are exempt from any bus type change ``BG`` -> ``BQ`` or ``BG`` => ``B``.
  * The controlled remote bus must be immediately adjacent to the generator.
  * The specified percentage is typically in the range 5-6%. It may be negative if the voltage is internal to the machine.
  * A maximum of 20 generators may be selected for reactive compensation.
  * The reactive compensation is case specific. It defines the base solution, but is not saved on the base history data file.

**Output Reports**
A special summary of all line drop compensation buses is listed in the analysis group under the title Summary of Line Drop Compensation. It is available either as a level 4 option on the ``/P_ANALYSIS`` or ``/F_ANALYSIS`` command or as the SUM%VAR option on the ``/ANALYSIS_SELECT`` command.

.. literal::

  /ANALYSIS_SELECT
  SUM%VAR

Example
-------
An example is shown below.

.. literal::

  ( POWERFLOW, ...) 
  ...
  ...
  ...
  / CHANGES, ...
  ...
  ...
  ... 
  / CHANGE_BUS_TYPE, BG=BQ, BT=B ,BX=B, -
                      LTC = OFF, -
                      AREAS = NORTHWEST, LIST=OFF 
  / CHANGE _BUS_TYPES,BQ=B,BX=B,BG=BQ,LTC=OFF,AREAS=NORTHWEST,BC-HYDRO
  >EXCLUDE_BUSES
  B    CENTRALA20.0
  B    BONN PH213.8
  B    BONNVIL213.8
  B    DALLES 313.8
  B    DALLES2113.8
  B    DALLES2213.8
  / SOLUTION
  ( END )

In this example, the disabling of remote voltage control is restricted to area ``NORTHWEST``. Within this area, all ``BG`` generators are permanently changed to type ``BQ``; all LTCs are disabled; and all ``BX`` buses are frozen to their discrete value.

CHANGE_PARAMETERS
=================

.. literal::

  / CHANGE_PARAMETERS, BUS=<name, base>,V=<set_value>,Q=?
                                        VX=<set_value>,QY=?
                                        Q=<set_value>,V=? 
                                        QY=<set_value>,QV=?
                                        P=<set_value>,V=?
                                        P=<set_value>,V=?
  > BX = LOCKED
  B OSTRNDER 500
  B MALIN 500

or

.. literal::

  / CHANGE_PARAMETERS,  BUS=<name, base),V=?, -
            %LOAD_CHANGE,%PY=<##>, %QX=<##,  -
                 %PX=<##>, %QY=<##, -
            ZONES=NA, NB, ..., -
            OWNERS=BPA, PSP, ...,  -
            AREAS=NORTHWEST

or

.. literal::

  / CHANGE_PARAMETERS,  BUS=<name, base),V=?, -
             %GEN_CHANGE,%PY=<##>, %QX=<##,  -
                %PX=<##>, %QY=<##, -
                ZONES=NA, NB, ..., -
                OWNERS=BPA, PSP, ..., -
                AREAS=NORTHWEST

.. note::

  This is one of the three commands which are order-dependent on the ``/SOLUTION`` command (the other two commands are ``LINE_SENSITIVITIES`` and ``LOSS_SENSITIVITIES``). Each of these must follow the ``/SOLUTION`` command.

The ``/CHANGE_PARAMETERS`` command perturbs a specified network parameter immediately after a successful solution, and initiates a new solution. The process continues until the last ``/CHANGE_PARAMETERS`` command has been read. All changed network parameters are permanent in the base case in residence. The output, analysis, and saved base case reflecting the final values of the parameters from the last change.

The ``/CHANGE_ANALYSIS`` feature is extremely useful to quickly and accurately generate a set of points for plotting Q-V and P-V curves. When used in conjunction with ``/USER_ANALYSIS``, the values of additional network quantities can be extracted during each ``/CHANGE_PARAMETERS``, enriching the scope of examination into the network.

The distribution VX, VY, etc., designates both the quantity and the axis on the X-Y data file. Default values (V, Q, etc.) are shown in Table 4-6.

BX = LOCKED
-----------
Type ``BX`` buses selected with this feature emulate the characteristics of mechanically switched shunt capacitors (MSC) controlled by a voltage relay. This voltage relay operates within a voltage deadband (:math:`V_{min}`, :math:`V_{max}`):

  1. If :math:`V_{min} < V < V_{max}`, then freeze present :math:`X_{shunt}` value.
  2. If :math:`V < V_{min}`, switch in additional capacitor steps or switch out connected reactor steps to raise the voltage, one step at a time.
  3. If :math:`V > V_{min}`, switch out connected capacitor steps or switch in additional reactor steps to lower the voltage, one step at a time.

For exposition, the feature is called BX Locking. In the absence of this feature, the normal operation is to switch :math:`X_{shunt}` one step per iteration to bias the bus voltage to :math:`V_{max}`.

Restrictions on BX Locking
--------------------------
The following restrictions apply to BX locking:

  * Only bus type ``BX`` buses may be selected for BX locking.
  * The feature is limited to a maximum of 10 BX locked buses.
  * This feature can be inserted after any ``/ CHANGE_PARAMETERS`` command. It defines *when* BX switching on selected ``BX`` buses becomes locked. Once defined, BX locking remains in effect for the duration of the study.
  * The voltage limits may be temporarily modified for BX locking. The new voltage limits are entered in columns (58:65) in the ordinary manner. These limits are temporary. After the solution, the original limits will be used for analysis reports.
  * The BX locking feature is not saved on any generated base case.

Bus Perturbation
----------------
Two forms of ``/ CHANGE_PARAMETERS`` are shown. The first form is bus perturbation. Three types of quantities may be perturbed:

  ``V = <set_value>`` Perturbs the Bus voltage magnitude (p.u.).

  ``P = <set_value>`` Perturbs the :math:`P_{gen}` (MW).

  ``Q = <set_value>`` Perturbs the :math:`Q_{gen}` (MVAR).

Restrictions
------------
V-perturbations are applied on V-constrained buses: ``BQ`` not at Q-limits, ``BE`` and ``BS`` types. If the bus type is unacceptable, it is automatically changed to a type ``BE`` and a warning diagnostic is issued.

Q-perturbations are applied on Q-constrained buses: ``B``, ``BC``, ``BT`` and ``BQ`` in state ``Q_min`` or ``Q_max``. If the bus type is unacceptable, it is automatically changed to a type ``B`` and a warning diagnostic is issued.

P-perturbation can only be applied on P-constrained buses: all types except ``BS``, ``BD``, ``BM``, and area slack buses.

Load Perturbation
-----------------
The second form of ``/ CHANGE_PARAMETERS`` is ``LOAD`` perturbation.   Either the ``P_load`` or the ``Q_load``, or both, may be perturbed a set percentage.

If no ``ZONES``, ``OWNERS``, or ``AREAS`` are specified, the percentage change applies to the entire system.

Note that the %P or %Q quantities in the output file correspond to the load that is changed. It may not be the total system load.

The inclusion of ``OWNERS`` with either ``ZONES`` or ``AREAS`` select candidates that are mutually inclusive.

Note that continuation records are accepted here.

For best results, the ``%LOAD_CHANGE`` option should be used in conjunction with ``GEN_DROP``. Otherwise, all increase in load is picked up by the area and system slack buses.

Generation Perturbation
-----------------------
The third form of ``/ CHANGE_PARAMETERS`` is GENERATION perturbation. Either the ``P_gen`` or the ``Q_gen``, or both, may be perturbed a set percentage.

If no ``ZONES``, ``OWNERS``, or ``AREAS`` are specified, the percentage change applies to the entire system.

Note that the %P or %Q quantities in the output file correspond to the generation that is changed. It may not be the total system generation.

The inclusion of O``WNERS with either ``ZONES`` or ``AREAS`` select candidates that are mutually inclusive.

Note that continuation records are accepted here.

For best results, the ``%GEN_CHANGE`` option should be used in conjunction with ``GEN_DROP``. Otherwise, all increase in generation is compensated by the area and system slack buses.

Bus Monitored Quantities
------------------------
Two types of bus quantities can be monitored:

  ``V = ?`` Monitors the voltage magnitude.

  ``Q = ?`` Monitors the reactive allocation including short and unscheduled VARS.

Restrictions apply. ``V = ?`` pertains to a ``BE`` bus. ``Q=?`` pertains to a ``B`` bus. Warning diagnostics flag these conditions.

PLOT File
---------
Each ``/ CHANGE_PARAMETERS`` command generates an x,y plot point in a file with subtype ``.QVPT``.

The composition of these points is dependent upon the composition of the ``CHANGE_PARAMETERS`` command. The table below summarizes the output.

========= =============== ======= ========
Set Point Monitored Point X Value Y Value
========= =============== ======= ========
V=<##>    Q=?             Q       V 
Q=<##>    V=?             Q       V 
P=<##>    Q=?             Q       P
P=<##>    V=?             P       V
%P=<##>   V=?             P_Load  V
%P=<##>   Q=?             P_Load  Q
%Q=<##>   V=?             Q_L0ad  V
%Q=<##>   Q=?             Q_Load  Q
========= =============== ======= ========

Each line in the ``QVPT`` file is interpreted in the ``.PFO`` (power flow output) file.

Example

  ``PLOT POINT 3 X (Q) = ####   Y (V) = ####``

This statement says point :math:`X_3` pertains to :math:`Q` and :math:`Y_3` pertains to :math:`V`.

User Analysis
-------------
To circumvent the limitations of monitoring a single bus’s V or Q, additional quantities may be monitored using a user-defined analysis file defined with the ``/ USER_ANALYSIS`` command.

The user analysis file is processed for each encountered ``/ CHANGE_PARAMETERS`` command. Its output is appended into an output file with subtype ``.USR_REPORT``

Example 1
---------

.. literal::

  / CHANGES,FILE= *
  .
  / CHANGE_BUS_TYPES,BQ=B,BQ=BQ,BX=B,LTC=OFF,AREA=NORTHWEST
  .
  / SOLUTION
  . 
  / CHANGE_PARAMETERS, BUS = RAVER    500., VY = 1.065, QX = ?
  / CHANGE_PARAMETERS, BUS = RAVER    500., VY = 1.060, QX = ?
  / CHANGE_PARAMETERS, BUS = RAVER    500., VY = 1.055, QX = ?
  .
  .
  .
  / CHANGE_PARAMETERS, BUS = RAVER    500., VY = 1.000, QX = ?
  (END)

In this example, buses in area ``NORTHWEST`` with types ``BQ``, ``BG``, and ``BX`` were changed to bring about a freeze in voltage control. The ``/ SOLUTION`` command is a dummy command, introduced to illustrate the position of the pure ``/ CHANGE_PARAMETERS`` commands. If the bus name following the ``BUS =`` keyword has imbedded blanks, insert a pound sign (``#``), for example, ``BELL#BPA``.

At the conclusion of an ordinary successful solution, the ``/ CHANGE_PARAMETERS`` records are processed, one by one. The first encounter will internally change the bus type of ``RAVER 500`` to ``BE``, if it is another type, and set its voltage to :math:`V = 1.065 p.u.` The perturbation will force a new Newton-Raphson solution. The :math:`Q` of ``RAVER`` is monitored. Its perturbed solved values will be printed out.

Subsequent ``/ CHANGE_PARAMETERS`` commands will perform additional perturbations.

Example 2
---------

.. literal::

  / USER_ANALYSIS,FILE=DRB2:[EOFBMJL]USANLINE.DAT
  / CHANGE_BUS_TYPES, BQ=B,BX=B,BG=BQ,LTC=OFF,AREA=NORTHWEST,BC-HYDRO
  / CHANGE,FILE= *
  . 
  .   THIS CASE MODELS THE P-V CURVE FOR THE POST TRANSIENT 
  .   CONDITIONS FOLLOWING 
  .   LOSS OF THE COULEE - RAVER #1 500 kV LINE. 
  .   INSTALL LINE DROP COMPENSATORS ON COULEE 
  .   500 UNITS AND JOHN DAY 
  .   AND ALL DALLES UNITS (EXCEPT 115 kV) AND 
  .   BONNEVILLE (EXCEPT 115 kV) 
  .   AND CENTRALIA AND CHIEF JOE 
  .   300 MVAR SVC AT KEELER AND MAPLE VALLEY 
  . 
  BGM   CENTRALA20.0 
  BGM   BONN PH213.8 
  . 
  / GEN_DROP, INIT=75,AREA=NORTHWEST,BC-HYDRO 
  B       LIBBY    13.8,   PMIN= 289.2,  PMAX=289.2 
  . 
  / SOLUTION 
  >AI_CONTROL=MON 
  . 
  .MONITOR RAVER 500 VOLTAGE AND INCREASE ZONE NA LOAD 
  . 
  / CHANGE_PARAMETERS, BUS = RAVER    500., V = ?
                             %LOAD_CHANGE  %P = 0.5, %Q = 0.5, ZONES = NA 
  / CHANGE_PARAMETERS, BUS= RAVER     500.,  V= ?
                             %LOAD_CHANGE  %P = 0.5, %Q = 0.5, ZONES = NA
  / CHANGE_PARAMETERS, BUS= RAVER      500., V= ?
                             %LOAD_CHANGE  %P = 0.5, %Q = 0.5, ZONES = NA 
  .
  .
  (END)

Miscellaneous Notes
-------------------
If the system is severely perturbed, ``/ CHANGE_PARAMETERS`` will cause divergence. If this happens, it is assumed that subsequent perturbations will be severe, so divergence will cause them to be ignored. A diagnostic will be issued.

CHANGES
=======

  ``/ CHANGES, FILE = file_name``
  
This command introduces system data change records. Column 3 on all bus, branch, area interchange and area intertie records contain a change code:

  blank Add.

  ``M`` Modify (non-blank fields are changes).

  ``D`` Delete.

  ``R`` Restore (previously deleted, available only on (``NEXTCASE``) runs).

The following is a list of some specialized change commands.

  ``DA`` Delete all buses within named areas.

  ``DZ`` Delete all buses within named zones.

  ``PO``, ``PZ``, ``PN``, ``PA`` Perform percentage changes according to type.

``FILE`` is optional. If included, records in that file will be processed before any additional change records, which may be in the input stream.

COMMENT
=======

  ``/ COMMENT``

This command introduces comment records into the output report. The comments will appear at the beginning of some output listings. The ``/COMMENT`` command is optional; all ``C`` comments in the ``bpf`` control file will be processed.

Comment text must have a ``C`` in column 1. Up to 20 comment records are permitted. Comment text is put in columns 2-80. Comments are saved in any ``NEW_BASE`` file for use when getting a plot.

When ``bpf`` loads a base file, any previous comments are deleted, then all comments in the ``bpf`` control file are added. The result is that only the comments in the ``bpf`` control file are saved.

COMMON_MODE_ANALYSIS
====================

