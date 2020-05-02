.. _ips-ipf-differences:

*******************
IPS IPF Differences
*******************

  1. Powerﬂow Command Differences: All IPF commands are different from those in IPS. When you are using the GUI, you will not have to worry about any of these, but there are some things you will need to do to your input data deck, such as deleting all the IPS control records and COPE commands (``HDG``, ``BAS``, ``TITLE``, ``ATTACH``, ``$DONE``, ``END``, etc.).

  2. Terminology: The IPF Base Case (.bse) ﬁle is a binary ﬁle equivalent to the IPS History (.HIS) ﬁle. However, the Base Case ﬁle does not contain any mapping data, and only one case per ﬁle is permitted. The IPF Network (.net) ﬁle is an ASCII ﬁle equivalent to the IPS base case or base data ﬁle (.IPS).  However, this ﬁle must not contain any modiﬁcation records (’M’ or ’D’ in column 3).  Changes go in a different ﬁle, which must be loaded separately. All mapping data is saved (in PostScript format) in a Coordinate ﬁle (.cor).  Only buses which have a match in the currently loaded system data will be displayed. 


  The PFC case identification, project ID, and two header records corresponds to the IPS case title.