
      integer MAXAGC
      parameter (MAXAGC = 24)
      common /agc/ agcmtx(21*MAXAGC), agc(14,MAXAGC), numagc

      double precision agcmtx
      real agc
      integer kagc(14,MAXAGC), numagc
      equivalence (agc, kagc)
 
C     Definitions:

C     NUMAGC = counter of generators participating.
C     AGC    = array of generators participating:

C      (1,*) = generator number,
C      (2,*) = storage location of dF/dX,
C      (3,*) = counter of storage location,
C      (4,*) = percentage,
C      (5,*) = P_max (p.u.),
C      (6,*) = P_min (p.u.),
C      (7,*) = P_base (p.u.),
C      (8,*) = P_load (p.u.),
C      (9,*) = 0/N: normal/area N slack bus,
C     (10,*) = status: 0 - initial condition;
C                      1 - active AGC control;
C                      2 - P_max limit constrained.
C                      3 - P_min limit constrained.
C     (11,*) = 0/N; normal/AGC is temporary slack node controlling
C              P_min or P_max for unit N.
C     (12,*) = bus type.
C     (13,*) = Pk at current iteration.
C     (14,*) = Excess Pk at area slack bus which must be reallocated.

