.. _ipfbat:

``ipfbat``
==========

Overview
--------
``ipfbat`` is a command line program that is the batch version of ``ipfsrv``. It accepts a Powerflow Control Language (.pcl) file. Plotting can be done with a control file; however, for most plots ``ipfplot`` is easier to use. Example of use: ``ipfbat bench.pcl``. The PCL commands used with ``ipfsrv`` and ``ipfbat`` are described in :ref:`powerflow-command-language`.

Batch Mode Plotting
-------------------
Batch mode plotting can be used when a coordinate file already exists, and the user simply wants
a hard copy diagram based on that file and Powerflow data. If the Powerflow data is on a saved
base case (``*.bse``) file, the simplest method is to use the ``ipfplot`` program. However, ``ipfbat``
offers more flexibility and control. For example, with ``ipfbat`` you can load, solve, and plot a
netdata file.

This technique can be used to produce diagrams that are generally produced through the GUI or
for access to features that have not yet been implemented in the GUI. These features include
plotting bubble diagrams, plotting difference diagrams, and plotting diagrams from a master list of
coordinate files.

An example of batch mode plotting is accomplished through the ``ipfbat`` program as follows::
    
  ipfbat bubble.pcl

where the ``.pcl`` file is a control file with the IPF commands and data necessary to produce a hard
copy diagram.

Commands in the examples are record groups starting with a ``/`` (slash) command and ending with
the next ``/`` (slash) command or ``(end)`` for the last command in the file.

Under the command ``/plot``, the first line must name the coordinate file to be used, and the second
must name the output PostScript file to be produced. Any subsequent records following, before
the next ``/command``, are interpreted as comments, and will be placed in the standard position
following the last comment defined in the coordinate file.

Two special uses for comment records must be noted. If the record begins with an ampersand (``&``),
it will be interpreted as an instruction to append the auxiliary coordinate file named on the record.
At most one such file may be named. If the record begins with an 'at' symbol (``@``), it will be
interpreted as an option record. Any diagram option indicated on this type of record will override
the option specified in the coordinate file. Multiple ``@`` records are allowed and will not be printed
on the diagram.

Example 1
^^^^^^^^^
Make a "standard" diagram (similar to the GUI operation).::

  /network_data,file=a92cy91.dat        ! Load the powerflow network data
  /solution                             ! Solve the powerflow case
  /plot                                 ! Make a hard copy diagram
  aberdeenmetric.cor                    ! using this coordinate file
  diagram.ps                            ! to build this postscript file.
  Case prepared by: A. Perfect Planner  ! Include this comment
  Priority of study: RWI ! and this comment
&aberdeeninset.cor ! and this additional coordinate file.
@OPtion DIagram_type=Pq_flow ! Supplement/Override *.cor options.
/syscal ! Hello operating system ...
lpr diagram.ps ! ... send this file to the printer.
/exit ! This job is finished.
(end)


