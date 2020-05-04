.. _x-window-graphical-interface:

****************************
X Window Graphical Interface
****************************
This section presents information about the X Window graphical user interface (GUI) for IPF.

IPF’s GUI interface makes data entry and modification easy. It also simplifies the running of base case solutions and the printing of network diagrams. This guide shows how to use the major features of IPF. Users who need details about data input record formats or system models should consult the :ref:`record-formats` section.

IPF’s GUI uses the X Window System and the OSF/Motif window manager interface. The graphical user interface features an interface that will be familiar to people who use an X Window System before, but it will feel a bit cumbersome compared to modern day applications.

Audience
========
You will be expected to already know the basics of power flow programs in general. You will find that small changes in bus and branch values are  easier to make in the GUI than hunting through ASCII text files. For example, users will get a feel for how changes to the input data affect the solution voltages in a network much more quickly with IPF than by using a batch style interaction.

The X Window System and the OSF/Motif window manager use certain descriptive terms for actions performed with the mouse. These terms are defined here. In addition, some keyboard actions are also defined. See the table below.

=========== ==================================
Term        Meaning
=========== ==================================
Select      Perform a mouse (or keyboard) action that singles out a user interface object. Usually the object is highlighted in some fashion, such as reverse video for menu selections. Subsequent commands or operations are performed on the selected object
Select Text Click the left mouse button inside a text box. If the text is modifiable, the cursor will change to the text cursor (I-beam) cursor, which can be moved to edit or enter text in the box
=========== ==================================

.. figure:: ../img/Motif_Window_Components.png

  Motif Window Components