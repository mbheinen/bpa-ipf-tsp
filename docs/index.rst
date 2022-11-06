Welcome to Interactive Power Flow!
==================================
Interactive Power Flow (IPF) is a software package for doing power flow studies.
IPF models the operation of a bulk electric power network. It can be used to:
 
  * Investigate bulk electric power networks
  * Determine bus voltage distribution
  * Study real and reactive power flows in lines
  * Evaluate line overloads
  * Plan area interchange control
  * Determine transformer tap settings
  * Determine remote-bus voltage controls
  * Plan for system reactive power requirements
  * Determine effect of load shedding, generator dropping, and line outages
  * Run transient stability studies on generator rotor dynamics

.. figure:: img/IPF_GUI.png

   Interactive Power Flow X Window GUI

Electric power system network design encompasses the following tasks:

  * Determination of load centers and generation patterns as well as sizes of 
    loads and generation.
  * Determination of available transmission corridors (rights-of-way) and 
    assessment of the capacity of these corridors to accommodate transmission 
    lines.
  * Evaluation of existing or planned networks with regard to adequate 
    power-carrying capability, voltage regulation, reliability of service, and 
    operating economics. 
  * Determination of size and routing of new transmission lines, and size and 
    location of terminal equipment for achieving efficient and economical 
    reinforcements when needed.
  * Evaluation of proposed reinforcements in light of power flow capability, 
    ability to withstand transient disturbances, reliability of overall 
    service, economics, impact on regional economy, environment, energy 
    conservation and operational constraints such as construction lead times, 
    coordination of various facility ownership interests, flexibility for 
    future growth and compatibility with other long-range plans.

The dynamic nature of load growth, load distribution, and generation patterns 
make the problem of network design one of planning. To plan for the future, 
power system planners and design engineers must look at the past and present. 
This makes Interactive Power Flow a great tool for the network design engineer, 
but it can also be used by students for academic studies. IPF models the complex
network structure and can evaluate it at various points in time.

Features
========
Interactive Power Flow has the following high level feature set:

  * Set of :ref:`command-line-tools` for running power flow studies using domain
    specific languages (See :ref:`powerflow-command-language` and 
    :ref:`power-flow-control` commands) for "batch" command line runs in which 
    users can run multiple powerflow scenarios, plot results (See :ref:`ipfplot`,
    and create reports (See :ref:`ipf_reports`)
  * :ref:`x-window-graphical-interface` allowing easy-to-edit graphical display 
    of the network diagram
  * :ref:`libcflow-c-library` functions for running studies
  * Advanced capabilities like REI :ref:`network-reduction` and :ref:`transient-stability-program`

Contents
========

.. toctree::
   :maxdepth: 3

   basic/installation
   basic/overview
   basic/history
   basic/application_examples
   basic/record_formats
   basic/pfc
   basic/pcl
   basic/command_line
   basic/tsp
   basic/x_window_gui
   basic/cflow
   basic/network_diagrams
   basic/line_impedance
   basic/dc_line_modeling
   basic/network_reduction
   basic/retained_network
   basic/voltage_limits_starts
   basic/developer

