*******
History
*******
IPF was developed by `Bonneville Power Administration`_ (BPA) and its contractors in the 1990s with about 20% of the cost 
supported by the `Electric Power Research Institute`_ (EPRI). By mutual agreement, as described in EPRI Agreement RP2746-03 entitled *Graphical User Interface for Powerflow*, March, 1992, 
 
 "all results of this project--including the computer program and its documentation--are to be in the public domain."
 
In a separate *Memorandum of Understanding with the Western Systems Coordinating Council* (WSCC), BPA agreed in March, 1992, to 

 "keep WSCC informed of progress, to make its best effort to develop the program according to the Guidelines adopted by the WSCC Computer Program Management Subcommittee, and to make the final results available for possible further development by WSCC."

Prior to the development of the Interactive Power Flow in the 1990s, BPA had developed the core power flow program and a set of tools they called the Power System Analysis Package (PSAP), which were a collection of Fortran-coded computer programs permitting the analysis of the steady-state operation of an electric power network. PSAP was developed over a 20-year period and many features were added during this time. Since the core power flow code was developed in 70s and 80s, it had to make efficient use of computer memory space and computation time, the core engine uses traditional techniques of large power system analysis including the Newton-Raphson method of solution of algebraic equations and sparse matrix computation techniques.

The Western Systems Coordinating Council (WSCC) now known as the `Western Electricity Coordinating Council`_ (WECC) had a similar set of tools known as Interactive Powerflow System (IPS) complete with a Domain Specific Language called Computationally Oriented Programming Environment (COPE) for defining power flow studies in a programmatic way. Many of the features and data record formats for IPF, especially for transient stability, were influenced by WSCC's IPS.

These programs were designed for character-based terminals connected to mainframes. However, the 1980s and 90s saw the creation and maturation of graphical user interfaces (GUI) based on the mouse, windows, and menus. These components were generally available on the Macintosh computer since 1984 and on IBM PC-compatible computers as an option since 1987. The third most popular GUI in the 90s was the one associated with the X Window System. GUIs were more intuitive and easier to use than character/keyboard-based interfaces. Starting in 1991, BPA began a two-way joint development of a GUI interface for the powerflow program, based upon the X-Window System and the `Motif X Window`_ GUI. The joint partners were the `Electric Power Research Institute`_ EPRI and WSCC. The powerflow program itself was also restructured and enhanced in the process, and a ``libcflow`` library was added as a means to provide users API access to the IPF  functions for use in other programs.

Original License
================
Below is a copy of the original license notice from BPA.::

  The BPA Powerflow software (IPF) is Public Domain; here is a copy of the License. 

  IMPORTANT NOTE:
  IPF is in the public domain and certain conditions apply. The terms in which 
  it is released are described in the section "Notice to Non-BPA Users" in 
  each of the IPF manuals.

  NOTICE TO NON-BPA USERS:
  The Bonneville Power Administration (BPA) releases BPA-developed computer 
  programs under the following conditions:

  1. BPA does not charge for program development costs; however, a fee to cover
     costs incurred in answering inquiries is assessed against the organization 
     receiving the material. This fee typically includes costs for personnel 
     and computer resources, reproduction, shipping, and postage.

  2. BPA cannot provide assistance with conversion to other computers or 
     consulting services to the program users.

  3. In consideration of receipt and acceptance of these programs or portions 
     thereof, if sold, assigned, or transferred to another organization, you 
     and your organization agree to advise any third-party recipient in writing
     that the program(s) and/or documentation are in the public domain and 
     available from BPA. The intent of this agreement is to ensure that 
     BPA-developed or supplied programs, and/or documentation, whether in 
     whole or in part, that are in the public domain, are identified as such
     to recipients.

  "LEGAL NOTICE"

  Neither BPA nor any person acting on behalf of BPA:

  1. Makes any warranty or representation, expressed or implied, with respect 
     to the accuracy, completeness, or usefulness of the information contained 
     in this report, or that the use of any information, apparatus, method, or 
     process disclosed in this report may not infringe upon privately owned 
     rights; or

  2. Assumes any liability with respect to the use of, or for damages resulting
     from the use of any information, apparatus, method or process disclosed in 
     this report.

  9/20/95

In keeping with the license, please know that original code is in public domain. Also, you should be able to obtain the original code released by BPA by contacting `Bonneville Power Administration`_ . However, for your convenience the `v0.0.1 release of this project <https://github.com/mbheinen/bpa-ipf-tsp/releases/tag/v0.0.1>`_ contains the original code released by BPA.

Original Contributors
=====================
The original project is the result of a lot of hard work from engineers at BPA and EPRI. These include, but are not limited to:

  * Dan L. Clark
  * Jay G. Coleman
  * Tsu-huei Liu
  * `Walter L. Powell`_
  * Bill E. Rogers
  * William D. Rogers
  * K E Rowell
  * John L Rutis
  * David M Stefonik
  * D M Syzmanski

.. _Bonneville Power Administration: https://www.bpa.gov/
.. _Motif X Window: https://motif.ics.com/motif/downloads
.. _Electric Power Research Institute: https://www.epri.com/
.. _Western Electricity Coordinating Council: https://www.wecc.org/
.. _Walter L. Powell: https://linkedin.com/in/walter-powell-18506b53/