Welcome to Interactive Power Flow!
==================================
Interactive Power Flow (IPF) is a family of programs for doing power flow studies. IPF models the balanced steady-state operation of an electric power network. It can be used by power system planners and design engineers to:
 
 * Investigate electric power networks
 * Determine bus voltage distribution
 * Study real and reactive power flows in lines
 * Evaluate line overloads
 * Plan area interchange control
 * Determine transformer tap settings
 * Determine remote-bus voltage controls
 * Plan for system reactive requirements
 * Determine effect of load shedding, generator dropping, and line outages

Electric power system network design encompasses the following tasks:

 * Determination of load centers (points) and generation patterns as well as sizes of loads and generation.
 * Determination of available transmission corridors (rights-of-way) and assessment of the capacity of these corridors to accommodate transmission lines.
 * Evaluation of existing or planned networks with regard to adequate power-carrying capability, voltage regulation, reliability of service, and operating economics. 
 * Determination of size and routing of new transmission lines, and size and location of terminal equipment for achieving efficient and economical reinforcements when needed.
 * Evaluation of proposed reinforcements in light of power flow capability, ability to withstand transient disturbances, reliability of overall service, economics, impact on regional economy, environment, energy conservation and operational constraints such as construction lead times, coordination of various facility ownership interests, flexibility for future growth and compatibility with other long-range plans.

The dynamic nature of load growth, load distribution, and generation patterns make the problem of network design one of planning. To plan for the future, a design must look at the past and present. This makes the Interactive Power Flow program a key tool for the network design engineer. Thanks to a comprehensive, structured database, it permits a complex network structure to be modeled and evaluated at various points in time. IPF also incorporates most modern modeling and analysis concepts. In addition, IPF assists the engineer in documenting major design decisions and changes.

The family of programs includes: ``bpf`` (batch powerflow), ``ipfplot`` (batch program for outputting printed maps),
``ipfcut`` a batch program that cuts out a subsystem from a solved base case file, and more.

BPA also developed a GUI component built with `Motif X Window`_ system. The goal was to simplify the running of base case solutions and the printing of network diagrams. This guide shows how to use the major features of IPF. Users who need details about data input record formats or system models should consult the ??.

Features
========
Interactive Power Flow (IPF) has the following high level feature set:

 * Domain specific languages (PCL or PFC) for batch runs
 * Extensive error messages for maximum aid to the user
 * Basic and extended power flow capabilities
 * Printed output reports
 * `Motif X Window`_ based graphical user interface allowing easy-to-edit graphical display of network diagram (coordinate file)

User may choose to interact with the powerflow program through the windows, menus, dialog boxes, keyboard, and mouse of the `Motif X Window`_ GUI or through the command line. Many of the functions and features of the command line programs (e.g. ``ipfbat``, ``ipfcut``, etc.) are available through specially designed GUI features. A Domain Specific Language (DSL) called Power Flow Control Language (PCL) is directly available through a Command Dialog in the GUI. See ?? for details on the syntax. 

The ``bpf`` program preserves all the traditional “batch run” capabilities, using control files in an alternate PFC syntax format. See ?? for details on this syntax.
 
 .. note::

  The PFC syntax was developed first and later BPA added the PCL syntax. They referred to PCL as the “new style”, and the PFC as the “old style”. These two sets of commands are not completely compatible even though the “new style” command set and syntax is closely modeled on the “old style.” 

History
=======
IPF was developed by `Bonneville Power Administraton`_ (BPA) and its contractors in the 1990s with about 20% of the cost 
supported by the `Electric Power Research Institute`_ (EPRI). By mutual agreement, as described in EPRI Agreement RP2746-03 entitled *Graphical User Interface for Powerflow*, March, 1992, 
 
 "all results of this project--including the computer program and its documentation--are to be in the public domain."
 
In a separate Memorandum of Understanding with the Western Systems Coordinating Council (WSCC), BPA agreed in March, 1992, to 

 "keep WSCC informed of progress, to make its best effort to 
develop the program according to the Guidelines adopted by the WSCC Computer Program Management Subcommittee, and to make the final results available for possible further development by WSCC."

Prior to the development of the Interactive Power Flow (IPF)in the 1990s, BPA had developed the the core powerflow program and a set of tools called the Power System Analysis Package (PSAP), which were a collection of Fortran-coded computer programs permitting the analysis of the steady-state operation of an electric power network. PSAP was developed over a 20-year period and many features were added during this time. These programs were designed for character-based terminals connected to mainframes. However, the 1980s and 90s saw the creation and maturation of graphical user interfaces (GUI) based on the mouse, windows, and menus. Studies showed that GUIs were more intuitive and easier-to-use interface than the older character/keyboard-based interfaces.

Starting in 1991, BPA began a two-way joint development of a GUI interface for the powerflow program, based upon the X-Window System and the `Motif X Window`_ GUI. The joint partners were the `Electical Power Research Institute`_ EPRI and the Western Systems Coordinating Council (WSCC) now known as the `Western Electricity Coordinating Council`_ (WECC). The powerlow program itself was also restructured and enhanced in the process, and a ``libcflow`` library was added as a means to provide users API access to the IPF powerflow functions for use in other programs.

Since the core powerflow program was developed in 70s and 80s, it had to make efficient use of computer memory space and computation time, the program uses advanced techniques of large power system analysis including the Newton-Raphson method of solution of algebraic equations and sparse matrix computation techniques.

.. toctree::
   :maxdepth: 2
   :caption: Contents:
   basic/installation
   basic/architecture

Check out the :ref:`genindex`, :ref:`modindex`, or :ref:`search` if you're looking for something specific.

.. _Bonneville Power Administraton: https://www.bpa.gov/
.. _Motif X Window: https://motif.ics.com/motif/downloads
.. _Electical Power Research Institute: https://www.epri.com/
.. _Western Electricity Coordinating Council: https://www.wecc.org/
