History
=======
IPF was developed by `Bonneville Power Administraton`_ (BPA) and its contractors in the 1990s with about 20% of the cost 
supported by the `Electric Power Research Institute`_ (EPRI). By mutual agreement, as described in EPRI Agreement RP2746-03 entitled *Graphical User Interface for Powerflow*, March, 1992, 
 
 "all results of this project--including the computer program and its documentation--are to be in the public domain."
 
In a separate Memorandum of Understanding with the Western Systems Coordinating Council (WSCC), BPA agreed in March, 1992, to 

 "keep WSCC informed of progress, to make its best effort to develop the program according to the Guidelines adopted by the WSCC Computer Program Management Subcommittee, and to make the final results available for possible further development by WSCC."

Prior to the development of the Interactive Power Flow (IPF)in the 1990s, BPA had developed the the core powerflow program and a set of tools called the Power System Analysis Package (PSAP), which were a collection of Fortran-coded computer programs permitting the analysis of the steady-state operation of an electric power network. PSAP was developed over a 20-year period and many features were added during this time. These programs were designed for character-based terminals connected to mainframes. However, the 1980s and 90s saw the creation and maturation of graphical user interfaces (GUI) based on the mouse, windows, and menus. Studies showed that GUIs were more intuitive and easier-to-use interface than the older character/keyboard-based interfaces.

Starting in 1991, BPA began a two-way joint development of a GUI interface for the powerflow program, based upon the X-Window System and the `Motif X Window`_ GUI. The joint partners were the `Electric Power Research Institute`_ EPRI and the Western Systems Coordinating Council (WSCC) now known as the `Western Electricity Coordinating Council`_ (WECC). The powerlow program itself was also restructured and enhanced in the process, and a ``libcflow`` library was added as a means to provide users API access to the IPF powerflow functions for use in other programs.

Since the core powerflow program was developed in 70s and 80s, it had to make efficient use of computer memory space and computation time, the program uses advanced techniques of large power system analysis including the Newton-Raphson method of solution of algebraic equations and sparse matrix computation techniques.

.. _Bonneville Power Administraton: https://www.bpa.gov/
.. _Motif X Window: https://motif.ics.com/motif/downloads
.. _Electric Power Research Institute: https://www.epri.com/
.. _Western Electricity Coordinating Council: https://www.wecc.org/