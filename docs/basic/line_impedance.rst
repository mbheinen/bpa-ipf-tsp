.. _calculating-line-impedance:

**************************
Calculating Line Impedance
**************************
This line impedance subroutine has been extracted from BPA's EMTP (Electromagnetic
Transients Program) and modified for computing a balanced pi-equivalent line section. 
You can read more about the BPA EMTP program here https://github.com/ahmadabdullah/BPA_EMTP.
The line impedance calculation feature is invoked from the branch data dialog. See Table C-1
and . It calculates the pi-equivalent quantities from the conductor type, conductor bundling, and
tower geometry. The following applies to all calculations:

   * There is continuous transposition of phases.
   * Resistivity is calculated at 25 degrees Celsius.
   * Earth resistivity is constant at 100 ohm-meters.

The Line Impedance dialog box consists of three panels for data.

   * Conductor Values. This is the scrollable list of conductor data, which is usually imported
     from a line constants data file.
   * Edit Conductor. This is the only means to edit data.
   * Calculate Impedance. This output panel displays the pi-equivalent data.
