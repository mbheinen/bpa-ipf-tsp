.. _ips2ipf:

***********
``ips2ipf``
***********
The :ref:`record-formats` used by IPF are defined in ASCII format and consists of area, bus, and 
branch records. This format is very similar to the format used by the Western Systems Coordinating
 Council (WSCC) back in the 1990s in their similarly named Interactive Powerflow System (IPS) 
 application. However, note that IPF supports many record types which are not recognized by IPS, 
 and in some cases the interpretation and application of the data values entered is different.

The ``ips2ipf`` command line program is designed to ease the burden of converting an IPS data deck
into one which can be input to the IPF program with the expectation of getting the same powerflow
solution results, within normal engineering tolerances. However, the conversion is not 100% automatic.
See :ref:`ips-ipf-differences` section for more detail on the data input and internal modeling
differences between the two programs.

Before running ``ips2ipf`` on an IPS data file, you should remove from the file all COPE commands
(``TITLE``, ``NEW``, ``ATTACH``, etc.) The program will handle the standard 'control cards' HDG, BAS,
and ZZ. Title records may be retained by putting an HDG in front of them, or by putting a period
(``.``) in column 1 of each. An unlimited number of (``.``) comments are is allowed, but these only 
annotate the data; they are not printed anywhere in the output.

``ips2ipf`` performs the following tasks:

  1. Renames duplicate buses.
  
     IPS uses a 12-character bus name, which includes the base kV. IPF uses only 8 
     characters, plus the real value of the base kV. To IPS, ``SAMPLE 230``. and
     ``SAMPLE 230`` are two different buses; to IPF they are the same bus.

     ``ips2ipf`` identifies duplicate names and generates a different name for one of them. It
     reports any changed names; if you don't like the name it generated, you can change it
     after the fact.
     
  2. Makes the system swing bus a ``BS`` bus, if given its name.
  3. Transfers non-zero shunt vars from ``BE`` and ``BQ`` records to ``+A`` records.

    In IPS, the shunt vars value entered for a bus which has variable var output is 
    considered to be a fixed component of the total vars. In order to retain this philosophy in IPF,
    it is necessary to put the shunt on a ``+A`` (continuation bus) record. Shunt vars entered
    on the ``BE`` or ``BQ`` record are considered by IPF to be continuously variable.
    
  4. Converts non-zero 'steps' on ``R`` records to ’taps’ (by adding one).
  
     IPS uses the number of steps available between TCUL taps; IPF uses the number of
     actual taps. If you run the conversion on an already converted file, another one will be
     added, which is probably not desirable.
     
  5. Converts IPS comments (``C`` in column 1) to IPF comments (``.`` in column 1).
  
     Unlike IPS, which prints the comments in the input data listing, IPF does not print
     them at all. But they can remain in the data file itself for information as long as they
     have a period in column 1 instead of a ``C``.
    
  6. Copies the controlled bus name from each ``X`` record to the corresponding ``BX`` record, to
     ensure that the proper bus is being controlled.
  7. Copies the voltage limits from a ``BX`` record controlling a remote bus, to the remote bus
     record.
  8. Corrects blank section id's in multi-section lines.
  
      Blank is acceptable to IPF as a section identification only on single-section lines.
      ``ips2ipf`` identifies multi-section lines, and changes blank to ``1``, ``1``to ``2`` etc. If there
      are actually 10 sections (IPS limit), then sections ``8`` and ``9`` will be combined and
      labeled ``9``.

  9. Gives bus ties a small impedance.
     
     IPF does not allow bus ties (0.0 impedance produces a fatal error.) ``ips2ipf`` changes
     this to (:math:`0.0 + j0.00001``), the same impedance IPF gives you when you sectionalize a
     bus and create a "bus tie" between the new bus and the old one. However, you should
     note that this may cause difficulties in getting a solution. (There are no zero impedance
     lines in standard WSCC study cases.)

  10. IPF has no ``RF`` model. Any ``RF`` records in your deck will be ignored.
  
Items which are not handled by ``ips2ipf``, which you need to look out for, are the following:

  11. In IPS, line and bridge current ratings on DC are not processed, but only passed on to
      the Stability program. IPF actually uses them. You may find that the bridge current
      rating on the Intermountain DC line is too low.

.. _ips-ipf-differences:

IPS IPF Differences
===================

  1. **Powerﬂow Command Differences**: All IPF commands are different from those in IPS. When you are using the GUI, you will not have to worry about any of these, but there are some things you will need to do to your input data deck, such as deleting all the IPS control records and COPE commands (``HDG``, ``BAS``, ``TITLE``, ``ATTACH``, ``$DONE``, ``END``, etc.).

  2. **Terminology**: The IPF Base Case (.bse) ﬁle is a binary ﬁle equivalent to the IPS History (.HIS) ﬁle. However, the Base Case ﬁle does not contain any mapping data, and only one case per ﬁle is permitted. The IPF Network (.net) ﬁle is an ASCII ﬁle equivalent to the IPS base case or base data ﬁle (.IPS).  However, this ﬁle must not contain any modiﬁcation records (’M’ or ’D’ in column 3).  Changes go in a different ﬁle, which must be loaded separately. All mapping data is saved (in PostScript format) in a Coordinate ﬁle (.cor).  Only buses which have a match in the currently loaded system data will be displayed. 

  3. **Case Title**: IPF builds the first line of a three-line IPS style title from the 10 character Caseid and the 20 character case description fields, and the other two lines from the two HEADER records. All of these are printed on standard BPA output listings, saved on the base case (history) file, and printed on hardcopy maps.

  4. **Structure**: The IPF Changes file (.chg) contains new and modification records you want to apply in bulk to your base case (e.g. your own local system representation). You will use the GUI to make individual touch-up or particular study changes. The system slack bus must be specified as a ’BS’ bus in the Network file; there is no GUI provision for selecting a slack bus (other than by changing the type of some bus to BS).

  5. **Data Differences**: IPF system data is very similar to that for IPS, but is *not* identical. If you try to read in a WSCC base case deck as an IPF network file, you can expect numerous data errors and no solution. If you charge ahead, fixing fatal errors as you stumble over them, you will still probably not get the answers to match, because of modeling differences. The data conversion program handles most of these. There are two categories of differences between BPA and WSCC power flow models:
  
    a. Modeling differences (including BPA extensions).
    b. Input data differences

  .. table:: IPS IPF Modeling Differences

  +----+----------------------------------------------------+---------------------------------------------------+
  |    | WSCC's IPS                                         | BPA's IPF                                         |
  +====+====================================================+===================================================+
  | 1  | The d-c line current rating is used only as a      | The minimum of the bridge current rating and      |
  |    | base by IPS. Both line current and bridge current  | the line current rating is used as a limit by the |
  |    | ratings are passed to the Stability program; they  | d-c system solution                               |
  |    | are not used as limits in the powerflow solution.  |                                                   |
  +----+----------------------------------------------------+---------------------------------------------------+
  | 2  | Type RM phase shifters (controlling :math: `P_{km}`| Type RM phase shifters (controlling :math:`P_km`  |
  |    | between :math:`P_{min}` and :math:`P_{max}`) will  | between :math:`P_min` and :math:`Pmax`) will bias |
  |    | bias the phase shift angle towards the original    | the phase shift angle to zero degrees to minimize |
  |    | phase shift angle.                                 | circulating real power flow.                      |
  |    |                                                    |                                                   |
  |    |                                                    | WSCC bias is available as a solution option on    |
  |    |                                                    | the GUI.                                          |
  +----+----------------------------------------------------+---------------------------------------------------+
  | 3  | A type ``BG`` generator may control only bus type  | A type ``BG`` generator may control bus types     |
  |    | ``BC.``                                            | ``BC``, ``B`` , ``BQ``, ``BV``, and ``BT.``       |
  +----+----------------------------------------------------+---------------------------------------------------+
  | 2  | An LTC may control only bus type ``BT``.           | An LTC may control bus types BC, B , BQ, BV,      |
  |    |                                                    | and BT.                                           |
  +----+----------------------------------------------------+---------------------------------------------------+
  |5   | Only one voltage control strategy per bus.         | A generator and an LTC may simultaneously         |
  |    |                                                    | control a common bus. If a degree of freedom      |
  |    |                                                    | exists, the LTC will control :math:`Q_km` directly|
  |    |                                                    | to minimize transformer var flow between terminal |
  |    |                                                    | buses.                                            |
  +----+----------------------------------------------------+---------------------------------------------------+
  | 6  | Type ``BX`` buses will bias the solution towards   | Type BX buses bias the solution to :math:`V_max`. |
  |    | the original :math:`X_shunt`.                      | WSCC bias is available as a solution option on    |
  |    |                                                    | the GUI.                                          |
  +----+----------------------------------------------------+---------------------------------------------------+
  | 7  | Infinite default limits are assigned to type ``BG``| Default global voltage limits are assigned to all |
  |    | buses.                                             | buses, including type ``BG`` buses, by base       |
  |    |                                                    | voltage level.                                    |
  +----+----------------------------------------------------+---------------------------------------------------+
  | 8  | The bus shunt reactive on type ``BQ`` buses is     | The bus shunt reactive on type ``BQ`` buses is    |
  |    | fixed.                                             | continuously adjustable (0 to 100%).              |
  |    |                                                    |                                                   |
  |    |                                                    | To make that quantity fixed, it must be entered   |
  |    |                                                    | on an accompanying ``+A`` continuation bus        |
  |    |                                                    | record.                                           |
  |    |                                                    |                                                   |
  |    |                                                    | The conversion program automates this.            |
  +----+----------------------------------------------------+---------------------------------------------------+
  | 9  | Inductance (G-jB) is applied to only one end of    | One half of G-jB is applied to each end of both   |
  |    | a transformer branch.                              | transformers and balanced pi lines.               |
Model RF phase shifter takes several iterations
to get from an initial angle to its final (fixed)
phase shift angle.
No such model. Problems in solving phase
shifters are handled internally.
11 Phase shifter must have same base kV at both
terminals.
Step up phase shifter. Tap2 field is off-nominal
tap2.
12 Phase shifter cannot be a section. Phase shifting transformer can be a section.
13 Bus ties (zero impedance lines) receive special
handling in solution and reporting.
No special bus tie model. A ’bus tie’ is defined
as a very low impedance line (0.0 + j0.00001).
14 Not available. “+” continuation bus records. Except for constant current load models, these records are
used mainly for accounting purposes to differentiate generation, load, and shunt with unique
ownerships.
15 Not available. “I” area intertie records. These records compute net area export on accompanying “A”
records.
16 Not available. “A” area record may be accompanied with
“A1”, “A2”, “A3”, and “A4” continuation records
to accept a maximum of 50 zones per area.
17 Not available. Branch records accept extended line current
ratings:
For types “L” and “E” records, thermal and bottleneck ratings.
For types “T” and “TP” records, thermal, bottleneck and emergency ratings.
18 Not available. Types “BM” and “LM” multi-terminal d-c data.
19 Not available. Type “RZ” RANI devices.
1
*
Base kv field interpreted as A4 for identification
purposes. SAMPLE 20.0 and SAMPLE 20
are different buses.
Base kV field interpreted as F4.0.
 SAMPLE 20.0 and SAMPLE 20 are the
same bus.
2
*
The number of steps on R records are interpreted as steps, where STEPS = TAPS - 1
The number of steps on R records are interpreted as number of taps, where TAPS =
STEPS + 1
3
*
A parallel branch consisting of sections will
accept section numbers in the set [0-9]. (Blank
is interpreted as a zero.)
A parallel branch consisting of sections will
accept section numbers in the set [1-9]. Zero
or blank can be used as a section number only
in a single section line (i.e. there are no ’sections’ in the line at all.) Zero or blank can also
be used on delete, to remove all sections of
one circuit .
4
*
Remotely controlled bus for a BX bus is specified on the X record.
Remotely controlled bus for a BX bus is specified on the BX record.
5
*
Voltage limits for a bus remotely controlled by
a BX bus are specified on the BC record.
Voltage limits for any bus, no matter how it is
controlled, are specified on the controlled bus
record.
6 Voltage limits (for reporting over and under
voltage buses) are specified on A records
Default voltage limits (for all purposes) are
specified by a default array which establishes
limits using base KV and zones.
7
*
Branches entered with both R and X equal to
zero receive special handling as ’bus ties’.
Zero impedance is not allowed – no bus tie
simulation.
8
*
The system slack bus can be designated as a
BS type bus, but very often is specified in the
SOLVE options instead.
System slack bus must be specified as a BS
bus.
9
*
IPS accepts various types of comment records
(’CB’, ’CL’, ’CR’) which annotate the data file,
and are printed in the (batch) input listing.
IPF uses a "." (period) in column 1 to designate
a comment. These annotate only the data file;
they are never printed.

* The conversion program will handle this item.