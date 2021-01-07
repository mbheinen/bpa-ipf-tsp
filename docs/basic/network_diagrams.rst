.. _ipf-network-diagrams:

********************
IPF Network Diagrams
********************

Overview
========
IPF has two different network diagram presentations. One is the display you see in the ``gui`` graphics, and the other is the hard copy map. The display is for convenience in accessing system data graphically - it does not represent what the plotted map will look like, nor does it need to be 'pretty'. The hard copy diagram is designed for reports, documentation, and analysis. It can be generated as a report from the graphic display or produced in a batch environment.

Both presentations use the same coordinate file format. The most important coordinate data, like bus icon and name locations, and line bending points, can be edited graphically from the ``gui`` by moving things around and saving the altered coordinate file. However, you will have to plot out the map in order to see how your changes have affected the hard copy appearance.

The basic diagram shows power system components modeled in a power flow study. The diagram is less detailed than a Powerflow listing, but may have more (or different) information than the graphic display. It shows essential bus and branch solution data. The diagram also shows identification data. For example, it shows date, case identification, program version, and the options used to generate the diagram. The diagram can be enhanced by adding to the coordinate file such items as:

  * A legend.
  * A border.
  * A control block for case identification, signatures, etc.
  * A case title heading.
  * Selected tie line flows and loss summary.
  * An inset showing detail in a selected area.
  * Any PostScript language objects.

These items will show up on the map plotted from the ``gui``, even though many of them cannot be added or edited with ``gui``. For details on all the hard copy diagram options and capabilities, and the usage of the ``ipfplot`` and ``ipfbat`` programs, see ??.
