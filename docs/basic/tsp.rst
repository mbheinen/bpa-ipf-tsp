.. _transient-stability-program:

*************************************
Transient Stability Program (``tsp``)
*************************************
The ``tsp`` program performs dynamic (e.g. generator rotor angle, governor output,
power system stabilizer output, etc.) simulation of a power system when disturbed 
from its steady-state condition under various perturbations such as three phase 
faults, line to line faults, line to ground faults, loss of generation, etc.

Program Description
===================
There are basically two methods of simulation used in this program. All differential equations are linear and solved by the trapezoidal rule of integration. The network equations are solved iteratively using the triangularized admittence matrix. Both of these methods are described in the paper *Fast Transient Stability Solutions* (IEEE Transactions on Power Apparatus and Systems, July/August 1972) written by Hermann Dommel and Nobuo Sato.

The diagram ?? shows that the swing program is divided into solution and output portions. The solution portion creates a swing solution file which saves all output data for all busses in the study. This structure allows the user to run the solution portion and save the swing solution file. Then, using the swing solution file, the power flow output file, and the swing output data file, the user can run any number of output jobs without resolving the solution.

The program is designed to run in conjunction with the IPF power flow program (``bpf``). The diagram ?? shows the interconnection between the power flow and transient stability program. Note that both the soution and output portions of the swing program require a power flow output file (``.bse``) as input.

The program also has a save data feature which allows the user to enter the majority of the swing input data via a save data file and enter only the line switching and FF records in the swing input file. This is useful when a series of swing studies are run using the same basic data but with different system perturbations.

Appendix C shows examples of how to create a save data file, how to run a study using a save data file, and how to run an output only job using the swing output file. The command files show how to make the necessary logical file assignments.

The DEBUG and COMMENT records are optional. Any information on the COMMENT records appears on the output listing as well as on the plot headings.

The switching records (``LS``) contain a variety of features and enable the user to properly define each event in chronological order. Several examples in preparing ``LS`` records are covered in ??.

The ``LZ`` record is used with the ``LS`` record and permits changing branch admittance quantities in the transient stability program.

The ``MZ`` record has the special feature which permits remote control signals for power system stabilizers and excitation systems. This card also inputs the generation transformer data for low-side generator bus determination not represented in the power flow but required for the excitation system.

Several control parameters and codes are available to the user and are entered in the DEBUG, CASE, FF, and 99 records. The CASE record and the 99 record have print options and the FF record contains several control parameters and codes for proper operation.

.. waring::

  Using the DEBUG record will produce very large output files.

The output portion of the program provides the output data in the form of listings and/or plots. The plotting routines were written for a VERSATEC plotter and this code is not very portable.

If line impedances were modified during the solution portion of the program, to correctly output data for these lines the output portiong of the program must be given the modified impedances via line modifiation (``LM``) records. ?? gives an example of how to use ``LM`` records.

The remaining information for the Transient Stability Program has not yet been migrated to ReadTheDocs format. See `TSP Users Guide`_ PDF for details.

.. _TSP Users Guide: https://github.com/mbheinen/bpa-ipf-tsp/blob/master/manuals/TSP_UsersGuide.pdf