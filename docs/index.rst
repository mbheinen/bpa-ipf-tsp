Welcome to Interactive Power Flow!
==================================
Interactive Power Flow (IPF) is a software package for doing power flow studies. IPF models the balanced steady-state operation of an electric power network. It can be used to:
 
 * Investigate electric power networks
 * Determine bus voltage distribution
 * Study real and reactive power flows in lines
 * Evaluate line overloads
 * Plan area interchange control
 * Determine transformer tap settings
 * Determine remote-bus voltage controls
 * Plan for system reactive requirements
 * Determine effect of load shedding, generator dropping, and line outages

.. image:: img/IPF_GUI.png

Electric power system network design encompasses the following tasks:

 * Determination of load centers (points) and generation patterns as well as sizes of loads and generation.
 * Determination of available transmission corridors (rights-of-way) and assessment of the capacity of these corridors to accommodate transmission lines.
 * Evaluation of existing or planned networks with regard to adequate power-carrying capability, voltage regulation, reliability of service, and operating economics. 
 * Determination of size and routing of new transmission lines, and size and location of terminal equipment for achieving efficient and economical reinforcements when needed.
 * Evaluation of proposed reinforcements in light of power flow capability, ability to withstand transient disturbances, reliability of overall service, economics, impact on regional economy, environment, energy conservation and operational constraints such as construction lead times, coordination of various facility ownership interests, flexibility for future growth and compatibility with other long-range plans.

The dynamic nature of load growth, load distribution, and generation patterns make the problem of network design one of planning. To plan for the future, power system planners and design engineers must look at the past and present. This makes Interactive Power Flow a key tool for the network design engineer. It permits a complex network structure to be modeled and evaluated at various points in time. In addition, IPF assists the engineer in documenting major design decisions and changes.

Features
========
Interactive Power Flow (IPF) has the following high level feature set:

 * Set of command line tools for running studies
 * Domain specific languages (PCL or PFC) for batch command line runs
 * Basic and extended power flow capabilities
 * Printed output reports
 * `Motif X Window`_ based graphical user interface allowing easy-to-edit graphical display of network diagram (coordinate file)
 * Extensive error messages for maximum aid to the user in troubleshooting

Contents
========

.. toctree::
   :maxdepth: 3

   basic/installation
   basic/overview
   basic/history
   basic/process_scenarios
   basic/record_formats
   basic/pfc

.. _Motif X Window: https://motif.ics.com/motif/downloads
.. _Bonneville Power Administraton: https://www.bpa.gov/
.. _Electric Power Research Institute: https://www.epri.com/
