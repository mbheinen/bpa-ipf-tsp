C
      common /tbxsrt/ tbxsrt(MAXTBX), tbxrat(MAXTBX), tbxslp(6,MAXTBX)
      integer tbxsrt, ltbxsl(6,MAXTBX)
      equivalence (tbxslp, ltbxsl)
 
C     TBXSLP(1,*) = ITER
C           (2,*) = ITYP (0 indicates entity is reinitialized).
C           (3,*) = Vk.
C           (4,*) = QK + Q_shunt.
C           (5,*) = SUM dV/dQ.
C           (6,*) = Number of terms in (5,*).
C
