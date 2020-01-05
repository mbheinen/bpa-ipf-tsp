C
      common /ltcsln/ ltcsln(MAXLTC)
      integer ltcsln
C
C     LTCSLN flags automatic/manual status:
C
C       0 = control is either automatic or manual.
C       1 = control is manual (limit truncation).
C       2 = control is manual (open reactive circuit).
C       3 = control is manual (low transformer impedance).
C       4 = control is manual (radial circuit).
C
