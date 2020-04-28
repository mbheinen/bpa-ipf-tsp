.. _ipfnet:
**********
``ipfnet``
**********
The GUI allows you to save a network data file describing the case you currently have loaded. The batch program, ``bpf``, has no command to do this. If you want a network data file produced in batch mode, use the netdata program. It loads a saved base case (.bse) file and writes it out in an ASCII network (.net) file.

Both programs generate a WSCC-formatted network data file in any of the following dialects: BPA, WSCC1, or PTI. "Dialects" means that although the file is still WSCC format, the data is generated with special processing or restrictions and is destined for use with other programs. In the case of the PTI dialect, that data is intended to be processed by the PTI-proprietary conversion program WSCFOR.

This program extracts network data from a Powerflow "old base" history file. Table F-1 below summarizes the effects of each dialect.

.. table:: Effect of Dialects on Network Data File

  Record or Field Dialect Effects
Header comments PTI Three header records: “<case_name> “ “<case_name> “ “<case_name> “
BPA, WSCC, WSCC1 “./CASE_ID = <case_name> “./CASE_DS = <case_description> “./H1 <header 1 text (auto-generated)> “./H2 <header 2 text (user input)> “./H3 <header 3 text (user input)> “./C001 <comment 1 text> ... “./Cnnn <comment nnn text>
Area “A” records BPA, PTI Encode zones 1-10 in “A” record, zones 11-20 in “A1” record, etc. Note: Voltage limits on “A” records are not encoded.They are specified by a default array that establishes limits using base kV and zones.
WSCC, WSCC1 Encode only “A” record (any zones 11-50 will be lost). Note; Voltage limits on “A” records are not encoded.They are specified by a default array that establishes limits using base kV and zones.
Intertie “I” records BPA, PTI Single entry (low alpha to high alpha) associated “I” records follow each “A record.
WSCC, WSCC1 No “I” records encoded.
Default percentages on type BG buses
BPA BG percentages are not changed.
PTI, WSCC, WSCC1 BG percentages are calculated if their default value is invalid.
Continuation “+” bus records
BPA “+” records are encoded.
PTI, WSCC, WSCC1 “+” records are consolidated with “B” records.
Reactive capability “Q” records
BPA “Q” records are encoded.
PTI, WSCC, WSCC1 “Q” records are not encoded.
Minimum branch impedance
BPA, PTI Branch impedances are not changed.
WSCC, WSCC1 Minimum branch impedances are set to 0.0003 p.u.
Branch ratings BPA Options: 1. Use extended ratings (120-character records). 2. Replace nominal rating with minimum (Emergency, Thermal, or Bottleneck). 3. Use nominal rating only.
PTI, WSCC, WSCC1 Options: 1. Replace nominal rating with minimum (Emergency, Thermal, or Bottleneck). 2. Use nominal rating only.
Branch sections BPA Encode as originally submitted.
PTI, WSCC Encode all branch sections in a consistent orientation.
WSCC1 Consolidate all sections into an equivalent branch
Regulating “R” records
BPA Encode as originally submitted.
PTI, WSCC 1. Encode as adjustable tap side-fixed tap side. 2. Consolidate parallel LTC transformers into a single, equivalent parallel LTC transformer.
WSCC1 1. Encode as adjustable tap side-fixed tap side. 2. Consolidate parallel LTC transformers into a single, equivalent parallel LTC transformer. 3. Convert taps into steps (STEPS = TAPS - 1).
D-C “LD” record BPA Encode as originally submitted.
PTI, WSCC, WSCC1 Encode as rectifier side-inverter side.

The resultant output is an ASCII file. Two formats are available for the resulting output. The BPA format retains all of the extra features that are available in the BPA Powerflow program without making any modifications to the data, while the WSCC format option consolidates and restricts the features in order to be used with WSCC’s IPS Powerflow program.

The CASEID of the power flow case data being extracted is used to create a file named CASEID.DAT. Any changes made to the data for WSCC (IPS) compatibility will be flagged on file CASEID.MES.

Input
=====
The ``ipfnet`` program prompts with the following requests:

  * File name of the Powerflow “old base” history filename.
  * Select output format desired: BPA, BPA1, BPA2, WSCC (IPS), or WSCC1 (IPS1).

Sample Run
==========
Type ``ipfnet`` at the system prompt and press the <RETURN> key. Answer the questions appropriately. An example is given below.

.. code::

  $ ipfnet
  > Enter OLD_BASE file name (or Q to quit): ../dat/43bus.bse
  > Enter name of network file (default is "../dat/43bus.net"): new.net
  > Enter dialect (BPA, WSCC, WSCC1 or PTI): WSCC
  > Enter record size (80 or 120): 80
  > Nominal rating replacement code
      T = Thermal  E = Emergency  B = Bottleneck
      T: Transformers = T, Lines = T
      E: Transformers = E, Lines = T
      B: Transformers = B, Lines = B
      ET: Transformers = E, Lines = T
      EB: Transformers = E, Lines = B
      M: Transformers = min(TEB), Lines = min(TB)
  > Enter rating replacement code: T
  * Options selected - dialect = WSCC
                         size  = 80
                        rating = T
  > Are above options correct (Y or N)? Y

Note that the codes for dialect and rating must be upper case. ``ipfnet`` formats commands which are sent to ``ipfsrv``. Some input checking is done, but invalid values may cause unexpected results.



