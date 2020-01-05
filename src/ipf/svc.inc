c
      common /svc/ svc(14,MAXXDT), numsvc
c
      double precision svc
      integer numsvc
C
C     NUMSVC = number of SVC buses.
C     KSVC(1,*) = external SVC bus number.
C     KSVC(2,*) = internal state:
C                 1 I < I_min
C                 2 I_min < I < I_max
C                 3 I > I_max
C     KSVC(3,*) = BCTBL index.
C     KSVC(4,*) = TBX index.
C     SVC(5,*) = B_min (p.u.)         ! Shunt reactance
C     SVC(6,*) = B_max (p.u.)         ! Shunt capacitance
C     SVC(7,*) = I_min (p.u.)         ! (gen)
C     SVC(8,*) = I_max (p.u.)         ! (gen)
C     SVC(9,*) = V_min (p.u.)
C     SVC(10,*) = V_max (p.u.)
C     SVC(11,*) = B_ref (p.u.)        ! Norton equivalent shunt
C     SVC(12,*) = I_ref (p.u.)        ! Norton equivalent current (gen)
C     SVC(13,*) = Non-SVC B (p.u.)    ! Non-SVC bus shunt
C     SVC(14,*) = Non-SVC I (p.u.)    ! Non-SVC constant current (gen)
C
