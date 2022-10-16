.. _overview:

********
Overview
********
You can interact with IPF using the command line tools (``bpf``, ``ipfbat``, ``ipfcut``, etc.), the ``libcflow`` C API, or the `Motif X Window`_ GUI. Many of the functions and features of the command line tools are available through specially designed GUI features. The GUI simplifies creating network data, running the base case solutions, and graphically visualizing the network diagrams but is rather dated and in need of a refresh. For this reason, many users will likely prefer the command line tools.

Executables
===========
IPF can be thought of as a family of executable programs. ``bpf`` is the batch form of the powerflow program allowing one or more scenarios to be defined in input files and run all at once via a call to ``bpf``. When the editing and displaying of buses and branches is being handled by ``gui``, the work of calculating solution voltages for a given power system network is done by ``ipfsrv``, which is just the ``bpf`` program in a different guise. Auxiliary programs allow you to do plots in batch mode, do a save of network data in batch mode, perform a "cut" of a solved base case, etc. Each of the executables are briefly described below. 

  ``bpf``
   
    Command line program that performs power flow. It executes using the commands from a Power Flow Control (PFC) file. Example usage: ``bpf bench.pfc.`` The PFC commands (.pfc) used with ``bpf`` allow for complete power flow runs including defining the network model and commands to perform various operations. The :ref:`record-formats` section describes the network model records available and the :ref:`power-flow-control` section describes the PFC syntax and commands available. More documentation about this program is in :ref:`bpf` section.

  ``ipfcut``
  
    Command line program that cuts out a subsystem from a solved base case file (.bse). Flows at the cut branches are converted into equivalent generation or load on specially formatted ``+A`` continuation bus records (read more about :ref:`continuation-bus-data`). An ensuing power flow run should solve with internal branch flows and bus voltages which are identical to those quantities in the original base case. More documentation about this program is in :ref:`ipfcut` section. Several methods are available to define the cut system: bus names, zones, base kVs, and individual branches. A pi-back feature replaces selected buses with a passive-node sequence (lines consisting of sections) with the original loads pi-backed in proportion to the line admittances.

  ``ipfplot``
   
    Command line plotting program to produce printed maps. The program accepts a coordinate file (.cor) and a base case file (.bse) on the command line, as well as an optional second base case file. When the second base case file is specified, a difference plot is produced. You can also use ``ipfplot`` to produce bubble diagrams. The same coordinate files are used for both ``gui`` and ``ipfplot``, but not all capabilities are available in ``gui``. Documentation is in :ref:`ipf-network-diagrams`.  More documentation about this program is in :ref:`ipfplot` section.

  ``tsp``
  
    Command line program that performs transient stability studies. Users specify power system network distrubances (line faults, generator trips), and the program simulates the affects on power system dynamics including key data points like generator rotor angle. This tool can be used to evaluate short term (on order of seconds) affects of these disturbances. Example of use: ``tsp bench.fil``.  More documentation about this program is in :ref:`tsp` section.

  ``ipfnet``
  
    The command line batch version of the "save netdata file" command built into the ``gui``/``ipfsrv``. This program generates a WSCC-formatted network data file in any of the following dialects: BPA, WSCC, or PTI. "Dialects" means that the file is still WSCC, but the data is generated with special processing or restrictions and is destined for use with other programs. In the case of the PTI dialect, that data is preprocessed by the PTI-proprietary conversion program WSCFOR. Detailed documentation is in :ref:`ipfnet`.

  ``gui``
  
    Launches the push button and menu-driven Graphical User Interface program built with `Motif X Window`_ that works in conjunction with the power flow server, ``ipfsrv``.  More documentation about this program is in :ref:`gui` section.

  ``ipfsrv``
  
    The power flow service daemon which runs as the backend component of the ``gui``. It executes power flow commands through :ref:`powerflow-command-language` scripts dispatched from the ``gui``. It gets launched automatically by the ``gui``.

  ``ipfbat``
   
    Command line program that is the batch version of ``ipfsrv``. It accepts a Powerflow Control Language (.pcl) file. Plotting can be done with a control file; however, for most plots ``ipfplot`` is easier to use. Example of use: ``ipfbat bench.pcl``. The PCL commands used with ``ipfsrv`` and ``ipfbat`` are described in :ref:`powerflow-command-language`.

  ``ipf_reports``

    The command line report tool to create output reports and summaries from solved base cases (.bse). Enter ``ipf_reports`` from the command line and follow the prompts.
  
  ``ips2ipf``
  
    The program that converts a network data file from WSCC's Interactive Powerflow System (IPS) format to IPF format. Duplicate buses are renamed; Load Tap Changer (LTC) steps are converted to taps, shunt susceptance on slack and ``BQ`` buses are transferred to ``+A`` records; sectionalized lines containing a section 0 are renumbered 1, 2, ... ; ``BX``, ``X``, and remote controlled bus data are converted to IPF format, etc. Documentation is in :ref:`ips-ipf-differences`.

IPF Interaction Model
=====================
The conceptual model of IPF is quite simple. You load power system network data into IPF; the IPF solution "engine" performs the calculations for the solution, and then outputs this solution data.

IPF offers different approaches to accomplish power system solutions. Their style of interaction and processing are quite different.

 * The batch power flow (can use either ``bpf`` or ``ipfbat``) approach. This is an Input-Process-Output approach. You write a command file containing all of the "orders" that you want filled, and the program performs the actions it determines are necessary to produce the ordered results.
 * The Graphical User Interface (``gui``) approach. This is command oriented – you click a button or enter a command, and it is executed immediately by the backend power flow engine (``ipfsrv``).
 * The CFLOW approach (the C API called ``libcflow``). This is a programming based approach where you write C code to interact with IPF.

Two Domain Specific Languages (DSLs) called  Powerflow Command Language (PCL) and Power Flow Control (PFC) allow users to define the "scenarios" or "orders". PCL is directly available through a Command Dialog window in the X Window GUI and can be used with the ``ipfbat`` program. PFC is used with the ``bpf`` program. See :ref:`power-flow-control` for details of the PFC syntax and :ref:`powerflow-command-language` for details of the PCL sytanx.

 .. note::

  The PFC syntax was developed first and later `Bonneville Power Administration`_ (BPA) added the PCL syntax. They referred to PCL as the "new style", and the PFC as the "old style". These two sets of commands are not completely compatible even though the "new style" command set and syntax is closely modeled on the "old style."

The ``bpf`` Batch Approach
--------------------------
When you use ``bpf``, you must first create a PFC file with the appropriate commands to accomplish the solution task at hand. At runtime these commands are accepted by ``bpf`` and executed according to a logical processing order determined by the program. Hence you need not be concerned with the ordering of commands in your PFC file. Input commands will be processed first, and a solution done automatically before any output is produced. Finally, a new base file will be created, if you have requested one. See :ref:`pfc-examples` for examples of PFC files.

.. figure:: ../img/BPF_Information_Flow_Model.png

   BPF Information Flow Model

The ``ipfbat`` Batch Approach
-----------------------------
``ipfbat`` allows you fine control over the data and solution "engine" (``ipfsrv``). You first create a PCL file with the appropriate commands, in the right order, to accomplish the solution task at hand. At runtime these commands are interpreted by ``ipfbat``. The PCL file commands are processed sequentially. Additional PCL command files may be specified by name, so that a "chain" of PCL files may be processed in one run.

The ``gui`` Approach
--------------------
When you use the ``gui`` approach, you use an X Window graphical interface with dialog boxes, menus, windows, etc. This makes data input, output, and manipulation easy. In addition to allowing basic case solution tasks to be accomplished, certain specialized tasks such as line impedance calculations are available. However, for more involved tasks, you need to use the ``bpf`` approach. For information about how to work with the GUI dialog boxes, menus, windows, etc., see :ref:`x-window-graphical-interface`. This guide also has a tutorial to show you how to solve straightforward power system cases.

.. figure:: ../img/PCL_Information_Flow_Model.png

   PCL Information Flow Model

The core powerflow program (``ipfsrv``) can be ran on separate machine from the graphical user interface. These two halves of the IPF program are joined by interprocess communication (IPC) routines that shuttle data and instruction messages back and forth between the GUI and powerflow program. The powerflow program serves primarily as a solution and data engine that sends and receives data when requested by the user through the GUI. Note that all of this commuication is basic socket communication so it likely doesn't work if GUI runs on one platform (Windows) and powerflow programs run on another (Linux).

The CFLOW Approach
------------------
Many times users need to do a large number of similar runs or they need to process data from another system or set of files before running studies. CFLOW is a C library API (``libcflow``) for IPF. To use CFLOW, you write a C program, including the header file ``cflowlib.h``, which defines all the structures and unions which allow access to the powerflow input and solution values. To retrieve these values, you call various CFLOW functions. The API interacts with ``ipfsrv`` interally. This allows you to do things like ask for a new solution, change the model, etc. See :ref:`libcflow-c-library` for details on the API and examples.

Network Data
============
A core component to any power system analysis problem is the power system network (the interconnected lines, generators, loads, transformers, nodes/buses, breakers, and other components). In IPF, this is referred to as Network Data. Network Data defines the structure (connectivity) and properties (base voltage, real power injections, real and reactive power loads, transformer tap settings, etc.) of the power system network. This data is defined in files that contain bus (nodes) and branch (lines, transformers, etc.) records. Most of the input files are ASCII text files. But one important file, the base case file (.bse), is in binary format. You can, of course, edit ASCII text files with any text editor. This is often done, but your data integrity is safer if you do all the editing you can in the ``gui``. The base case binary file cannot be edited with an ordinary text editor, but can in effect be edited via the ``gui`` when you have a base case file loaded.

There are multiple methods for specifying the Network Data. The following is a list of the various methods with descriptions.

``.pfc``

  This input file contains job control information for the ``bpf`` program. This file may contain Network Data explicitly (listing out :ref:`record-formats` right in the file), but more often includes a property called ``NETWORK_DATA`` that specifies a file containing the :ref:`record-formats` in ASCII format. Alternatively, users can specify a property called ``OLD_BASE`` that specifies a base case (.bse) file, or other job control data to be described. Keeping the Network Data out of the ``.pfc`` file allows more clear separation between the commands of the batch study and the network model.

  You can edit this file using any ASCII text editor to add, modify, and delete commands and data records. See :ref:`pfc-examples` for examples.

``NETWORK_DATA`` 

  This ASCII text input file contains a series of records of bus and branch data. It must not contain modification records.
  
  This file can be maintained by using an ASCII text editor. Or you can edit the records you want in the GUI through the various dialog boxes and then save a new ``NETWORK_DATA`` file. In the file, data records may be in random order, but actual processing is done in the following order:
  
   1. ``A`` and ``I`` records (area interchange)
   2. ``B``, ``+``, and ``X`` records (bus)
   3. ``L``, ``R``, ``E``, and ``T`` records (branch).

``BRANCH_DATA``
  
  This ASCII text input file contains the branch data of all branches coded with in-service date and out-of-service date. This file is searched for branches in service on the date requested. BPF selects the appropriate branches.

``NEW_BASE``

  This program-generated, binary output file contains complete base network data and steady-state operating values for the case being processed. This file is identical in format to the ``OLD_BASE`` file. ``NEW_BASE`` simply designates the file when it is produced as the output from a recently concluded case study.

``OLD_BASE`` 

  This program-generated, binary input file contains complete base network data and steady-state operating values. This file is identical in format to the ``NEW_BASE`` file. ``OLD_BASE`` simply designates the file when it functions as an already existing input file.

``CHANGE`` 

  This ASCII text input file contains changes (new and modification records) to the data input from any combination of ``NETWORK_DATA``, ``BRANCH_DATA``, and ``OLD_BASE`` files making up the case to be studied. These change records change the input data for the base case.

``Printout File`` 

  This is an ASCII text output file that contains bus, branch, and solution data from a completed case study and is intended for ordinary, paper hardcopy output.

``Microfiche file`` 

  This is a special format output file that contains bus, branch, and solution data from a completed case study and is intended for microfiche format.

.. table:: IPF Input/Output Files

   ========================== ====== ============================ =============== ======= =================================
   File                       Format Input/Output (I/O)           Created by      Editing Information Contained
   ========================== ====== ============================ =============== ======= =================================
   PFC                        ASCII  ``bpf`` (I)                  User            Yes     Bus, Branch, Commands, File Names
   PCL                        ASCII  ``gui``, ``ipbat`` (I)       User            Yes     Commands, File Names
   NETWORK_DATA               ASCII  ``bpf`` (I) GUI,IPFBAT (I/O) User gui ipfnet Yes     Bus, Branch
   BRANCH_DATA                ASCII  Input Only                   User            Yes     Branch
   OLD_BASE                   Binary Input Only                   IPF             No      Bus, Branch, Solution Values
   CHANGES                    ASCII  Input or Output              User ``gui``    Yes     Bus, Branch, Modiﬁcations
   NEW_BASE                   Binary Output Only                  IPF             No      Bus, Branch, Solution Values
   Printout ﬁle (<name>.PFO)  ASCII  Output Only                  ``bpf``         No      Input Data and Solution Reports, User Analysis
   Microﬁche ﬁle (<name>.PFF) ASCII  Output Only                  ``bpf``         No      Input Data and Solution Reports, User Analysis
   Debug ﬁle (<name>.PFD)     ASCII  Output Only                  ``bpf``         No      Solution arrays and iteration processing
   Printout ﬁle (<logon>.PFO) ASCII  Output Only                  ``gui``         No      Messages, Iteration Summary
   Debug ﬁle (<logon>.PFD)    ASCII  Output Only                  ``gui``         No      Solution arrays and iteration processing
   ========================== ====== ============================ =============== ======= =================================

The NETWORK_DATA File
=====================
This ASCII text data file consists of area, bus, and branch records in the format used by the Western Systems Coordinating Council (WSCC) back in the 1990s. However, note that IPF supports many record types which are not recognized by IPS, and in some cases the interpretation and application of the data values entered is different. See :ref:`ips-ipf-differences` for a list of IPS-IPF differences. This file must not contain modification records, only new data.

 1. Area interchange records.
   
   Each area record identifies a composition of zones whose member (associated) buses define specific aggregate quantities that may be controlled to specified export values.

   ``A`` (Area interchange records)

   ``I`` (Area intertie records)

 2. Bus data record group containing at least two records.
   
   Each bus data record identifies one bus in the network. Buses are uniquely identified by their bus name and base kV.
   
   ``B`` (Bus records) 

   ``+`` (Continuation bus records)

   ``X`` (Continuation bus records)

   ``Q`` (PQ Curve data records)

 3. Branch data record group containing at least one record.

  ``L`` (AC or DC Transmission line records)

  ``E`` (Equivalent Branch records) 

  ``T`` (Transformer records) 

  ``R`` (Regulators (Automatic or LTC transformer) records)

Branch data entered in any of the ASCII files is *single-entry* or one-way only. This means, for example, that a branch connecting buses A and B has a user-submitted entry (A,B) or (B,A) but not both. The program transposes the record internally as required during execution. Normally which way the branch is entered does not matter, but it does affect the default end metered on a tie line, and the physical position of line sections. See :ref:`record-formats`, for a discussion of this feature.

Branches are uniquely identified by three fields:

 * Their terminal bus names and base kVs.
 * Their circuit or parallel ID code.
 * Their section code.

The BASE (.bse) File
====================
This file, designated ``OLD_BASE`` if you are loading it, or ``NEW_BASE`` if you are saving it, is binary in format and contains the following data:
 
 * The case identification, project ID, and two header records.
 * The date the case was generated.
 * The program version used to generate the file (so future program versions can read the file if file structures change).
 * Up to 100 comment records.

.. _Motif X Window: https://motif.ics.com/motif/downloads
.. _Bonneville Power Administration: https://www.bpa.gov/