.. _ipfcut:

**********
``ipfcut``
**********

``ipfcut`` is a command line program that cuts out a subsystem from a solved base case file. The full system resides in a base case file; the cut system is a card image Bus/Branch data file. Flows at the cut branches are converted into equivalent generation or load on specially formatted ``+A`` continuation bus records. An ensuing power flow run should solve with internal branch flows and bus voltages which are identical to those quantities in the original base case.

  * Several methods are available to define the cut system: specifying individual buses, zones, base kVs, or individual branches.
  * A pi-back feature replaces selected buses with a passive-node sequence (lines consisting of sections) with the original loads, generation, and shunts, pi-backed in proportion to the line admittances.

The function of ``CUTTING`` and ``REDUCTION`` are similar, but their methodologies are different. Both generate subsystems whose internal composition and characteristics are identical to that of the base case. REDUCTION generates equivalent branches, shunt admittances, and injections such that internal nodes still "see" the full system. CUTTING generates equivalent shunt admittances and injections such that internal nodes can determine that the boundary has changed and the external system has been cut out, even though the internal flows and nodal voltages are identical.

The CUTTING program mandates that the flow into the cut-out system is constant. This is valid for eliminating radial feeder circuits, but not for eliminating a strongly interconnected external network. In the latter case, REDUCTION yields a more responsive equivalent.

A simple criterion can be used to determine whether CUTTING or REDUCTION is more appropriate.

  * Will a line outage or other major perturbation near the boundary of the retained subsystem and eliminated system significantly alter the flow between the two systems?

If the answer is no, the flow will not be significantly altered, then CUTTING is acceptable. (It is the author's opinion that REDUCTION is always superior.)

The CUTTING program is initiated by entering IPFCUT at the keyboard after the computer displays the system prompt.

From this point on the operation is interactive. You should respond to the questions as they are asked.

Cutting Methodologies
=====================
Two simple techniques are employed. Both may be used.

  * **Cutting the eliminated branches.** In cutting, the active and reactive power flowing into a cut branch is replaced with an equivalent but fictitious load, which is appended to the terminal bus with continuation buses (``+A``).

  * **Pi-backing loads of retained buses.** In pi-back, the loads and shunt susceptances on selected pi-back buses are distributed to neighboring terminal buses in proportion to their branch admittances. Only branch transfer susceptance is used (a good approximation when X >> R). Also, the pi-back bus may contain at most two branches. This corresponds with early reduction schemes. The quantities pi-backed are appended to the terminal buses on specially coded continuation cards (``+A ***``).

Input Commands
==============
The syntax of CUT commands conform to the convention that has been adopted for the other IPF programs that use commands.

::

  [FICHE, copies=n] 
  (CUTTING, Project=name, case ID=name)

The qualifiers that select the subsystem and enable special options are listed below::

  >DEBUG< 
  >EXCLUDE_BUSES< 
  >INCLUDE_BUSES< 
  >PI_BACK_BUSES< 
  >SAVE_BUSES...< 
  >CUT_BRANCHES< 
  >SAVE_ZONES...,SAVE_BUSES...< 
  >WSCC<

``>WSCC<`` Enables the WSCC option. The default is no WSCC. Special processing is effected with this option:

  1. Active power flowing from a cut branch into a bus is treated as a bus load under the WSCC option. Otherwise, it is treated as a load or generation depending upon the sign of the quantity.
  2. Base kV fields omit the decimal point without the WSCC option. For example, a 115.0 KV appears as "115". Under the WSCC option, the same field appears as "115.". The WSCC Powerflow program interprets the base fields as character instead of decimal, and those two fields are unique! Lane 115. is in New Mexico but LANE 115 is in Oregon!
  3. Line sections created from pi-back are consolidated into a single equivalent pi branch with the WSCC option enabled. Otherwise, the branch records are preserved (with the necessary name changes). There is one exception: If a step-up/step-down transformer-line transformer occurs, the branch is unconditionally made into an equivalent section.
  4. Any branch in the cut list that has an ``INT`` in the ownership field has its flow transferred to a ``+A INT`` continuation bus instead of a ``+A ***`` bus.

``>DEBUG<`` Opens the program debug file. Output appears on a file with extension .pfd. This is used only by the program developers.

``>SAVE_ZONES...,SAVE_BUSES...<`` Defines the retained network as all buses whose bases and zones both match the specified list. If ``SAVE_BUSES`` is null or omitted, only zones are considered. Continuation cards begin with a ``+`` in column 1.

For example: ``>SAVE_ZONES NA,NB,NC,ND,NE,NF,NG,NH, NI,NJ,NK,RM<``

Any number of ``>SAVE_ZONE...SAVE_BASE<`` commands may be submitted. ``>SAVE_BASES...<`` defines the retained network as all bases whose buses match the specified list. It is not necessary to type a decimal part unless it is part of the base kV, for example, 13.8 but not 3.46. Continuation cards begin with a ``+`` in column 1.

The system is initialized as an eliminated network. The following commands define the composition of the retained system. With the exception of 

``>CUT_BRANCHES<``, the effect of the commands may be repeated in any order. Their effects are overlaid.

``>INCLUDE_BUSES< >EXCLUDE_BUSES< >SAVE_BUSES<`` These commands introduce buses that are specified on bus records that follow (``B`` in column 1).
``>SAVE_BUSES`` is used to specify the entire cut system, bus by bus.

``>INCLUDE_BUSES`` is used to expand the cut system with individually named buses. This is used in context with ``>SAVE_ZONES`` or ``>SAVE_BUSES`` to provide more flexibility in the cut system.

``>EXCLUDE_BUSES`` is used to contract the cut system with individually named buses. This is used in context with ``>SAVE_ZONES`` or ``>SAVE_BUSES`` to provide more flexibility in the cut system.

A maximum of 1000 records are permitted. In the unlikely event that this is insufficient, the above command(s) may be simply repeated with an additional block of bus records.

``>CUT_BRANCHES<`` This command introduces branches that are specified on line records that follow (``L``, ``T``, or ``E`` in column 1). A maximum of 500 cut branch records is permitted.

The ``CUT_BRANCHES`` are oriented in the following order: retained bus, cut bus.

The cut system is defined in the following manner. Starting from the set of all cut branches, each bus on the cut side, which is in the eliminated system, is expanded one-adjacent by examining each branch connected to that bus. All branches that are not connected to any bus on the retained bus side are in the cut system. Those terminal buses are eliminated.

The first pass determines all buses 1-adjacent that are in the cut system. The process is repeated, starting with all buses 1-adjacent to the cut boundary to find all buses 2-adjacent. This process is repeated until no further expansion occurs in either system. The major advantage of this approach is that any incomplete cut enclosure is properly diagnosed near the missing branch.

If the WSCC qualifier is selected, any branch in the cut list that has an INT in the ownership field will have its flow transferred to a +A INT continuation bus instead of a ``+A ***`` bus. This is done so that if this cut system is to be reintegrated into another system the cut points can be easily identified and discarded.

Unlike other ``>...<`` commands, ``CUT_BRANCH`` cannot be repeated.

``>PI_BACK_BUSES<`` This process replaces a bus having one or two branches with an equivalent consisting of bus generation, load, and shunt admittances on the adjacent terminal buses.

If the bus originally had two branches, the new system has the following changes:

  * The buses' generation, load, and shunt admittance are proportioned by the branch admittance to each terminal node.
  * The bus is eliminated.
  * The subsystem consisting of a bus and two branches is replaced with a single branch spanning the two terminal buses.

If the bus originally had one branch, the new system has the following changes:

  * The buses' generation, load, and shunt admittance are transferred to the terminal node.
  * The bus and its branch are eliminated.

In essence, a pi-backed bus becomes a passive node in a branch that now consists of sections. Since the quantities are pied-back in proportion to their branch admittances, the redistribution approximates the effects of ``REDUCTION``. A maximum of 1000 pi-back records may follow. If this limit is insufficient, the remaining pi-back records may follow another ``>PI_BACK<`` command.

Interactive Approach
--------------------
The following is an example of the dialogue that occurs during an interactive execution.

.. code::

  * command file is: J8301FY84.CUT 
  ENTER NAME for BUS/BRANCH output file > J83CUT.DAT 
  ENTER file name for OLD_BASE > A8301FY84.BSE