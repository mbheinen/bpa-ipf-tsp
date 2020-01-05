C
      common /lndpcp/ numldc, lndpcp(2,20), drppct(20), vmax_ldc(20),
     &                vmin_ldc(20), lndp_type(20), xc_ldc(20)
      integer numldc, lndpcp, lndp_type
      real drppct, vmax_ldc, vmin_ldc, xc_ldc
C
C     NUMLDC = number of line drop compensators.
C     LNDPCP(1,*) = bus number.
C     LNDPCP(2,*) = remote bus number.
C     LNDP_TYPE(*) = type 1 : LINE_DROP_COMPENSATION
C                         2 : REACTIVE_COMPENSATION
C     DRPPCT(*) = percent compensation.
C     VMAX_LDC(*) = Vmax limit
C     VMIN_LDC(*) = Vmin limit
C     XC_LDC(*) = Vmin limit
C
