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

Contents
========

.. toctree::
   :maxdepth: 3

   basic/installation
   basic/architecture
   basic/process_scenarios
   basic/history
   basic/record_format

Check out the :ref:`genindex`, :ref:`modindex`, or :ref:`search` if you're looking for something specific.

.. _Motif X Window: https://motif.ics.com/motif/downloads
.. _Bonneville Power Administraton: https://www.bpa.gov/
.. _Electric Power Research Institute: https://www.epri.com/
