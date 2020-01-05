 
      common /bxlock/ numlck, nxlock(MAXXDT), vxlock(2,MAXXDT),
     1                bxlock(MAXXDT)
C
C     NUMLCK = number of BX buses with deadband locking.
C     NXLOCK() = array of external numbers of flagged BX buses.
C     VXLOCK() = array of Vmin, Vmax limits for flagged BX buses.
C     BXLOCK() = array of initial B shunt for flagged BX buses.
C
