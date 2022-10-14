.. _ips-ipf-differences:

*******************
IPS IPF Differences
*******************

  1. **Powerﬂow Command Differences**: All IPF commands are different from those in IPS. When you are using the GUI, you will not have to worry about any of these, but there are some things you will need to do to your input data deck, such as deleting all the IPS control records and COPE commands (``HDG``, ``BAS``, ``TITLE``, ``ATTACH``, ``$DONE``, ``END``, etc.).

  2. **Terminology**: The IPF Base Case (.bse) ﬁle is a binary ﬁle equivalent to the IPS History (.HIS) ﬁle. However, the Base Case ﬁle does not contain any mapping data, and only one case per ﬁle is permitted. The IPF Network (.net) ﬁle is an ASCII ﬁle equivalent to the IPS base case or base data ﬁle (.IPS).  However, this ﬁle must not contain any modiﬁcation records (’M’ or ’D’ in column 3).  Changes go in a different ﬁle, which must be loaded separately. All mapping data is saved (in PostScript format) in a Coordinate ﬁle (.cor).  Only buses which have a match in the currently loaded system data will be displayed. 

  3. **Case Title**: IPF builds the first line of a three-line IPS style title from the 10 character Caseid and the 20 character case description fields, and the other two lines from the two HEADER records. All of these are printed on standard BPA output listings, saved on the base case (history) file, and printed on hardcopy maps.

  4. **Structure**: The IPF Changes file (.chg) contains new and modification records you want to apply in bulk to your base case (e.g. your own local system representation). You will use the GUI to make individual touch-up or particular study changes. The system slack bus must be specified as a ’BS’ bus in the Network file; there is no GUI provision for selecting a slack bus (other than by changing the type of some bus to BS).

  5. **Data Differences**: IPF system data is very similar to that for IPS, but is not identical. If you try to read in a WSCC base case deck as an IPF network file, you can expect numerous data errors and no solution. If you charge ahead, fixing fatal errors as you stumble over them, you will still probably not get the answers to match, because of modeling differences. The data conversion program handles most of these. There are two categories of differences between BPA and WSCC power flow models:
  
    a. Modeling differences (including BPA extensions).
    b. Input data differences

  .. table:: IPS IPF Modeling Differences

  +----------------------------------------------------+---------------------------------------------------+
  | WSCC's IPS                                         | BPA's IPF                                         |
  +====================================================+===================================================+
  | The d-c line current rating is used only as a      | The minimum of the bridge current rating and      |
  | base by IPS. Both line current and bridge current  | the line current rating is used as a limit by the |
  | ratings are passed to the Stability program; they  | d-c system solution                               |
  | are not used as limits in the powerflow solution.  |                                                   |
  +----------------------------------------------------+---------------------------------------------------+
  | Type RM phase shifters (controlling :math: `P_{km}`|
  | between :math: `P_{min}` and :math: `P_{max}`) will|
  | bias the phase shift angle towards the original    |
  | phase shift angle.                                 |
