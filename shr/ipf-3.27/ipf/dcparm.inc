C FORTELT DCPARM
C---------------------------------------------------------------------
C
C        RELEASE    3        VERSION    1         JULY 15, 1985
C
C---------------------------------------------------------------------
C
C     DC PARAMETERS.
C
C        DEFINITION OF PARAMETERS:
C                  JMAX   -  jacobian size.
C                  NCC    -  maximum number of central controllers.
C                  NCC5   -  maximum storage for central controller.
C                  NDCB   -  maximum storage for breakers.
C                  NDCC   -  maximum number of dc converters.
C                  NDCL   -  maximum number of dc lines.
C                  NDCN   -  maximum number of dc nodes.
C                  NDCN5  -  nodal conductance matrix storage space.
C
C
      integer    JMAX, NDCB, ndcc, ndcl, ndcn, ndcn5, NCC, ncc5
C
      parameter (NDCB=50,ndcl=50,ndcc=50,ndcn=50,ndcn5=5*ndcn)
      parameter (NCC=50,ncc5=NCC*5)
      parameter (JMAX=500)
C
C---------------------------------------------------------------------
C
C     DEGREE/RADIAN CONSTANTS.
C
C        DEFINITION OF CONSTANTS:
C                  DGRRAD -  convert from degrees to radians.
C                  PI     -  value of pi to ten decimals.
C                  RADDGR -  converts from radians to degrees.
C
C
      real          DGRRAD, PI, raddgr
C
      parameter (PI=3.1415926535)
      parameter (DGRRAD=PI/180.0, raddgr=1/DGRRAD)
C
C---------------------------------------------------------------------
C
C     BREAKER STATUS CONSTANTS
C
C        DEFINITION OF CONSTANTS:
C                  OPEND  -  value of DBSTAT for an OPEN breaker.
C                  OPENNG -  value of DBSTAT for an OPENING breaker.
C                  CLOSED -  value of DBSTAT for a CLOSED breaker.
C
      integer       CLOSED, OPEND, OPENNG
C
      parameter (CLOSED = 32)
      parameter (OPEND  = 33)
      parameter (OPENNG = 34)
C
C---------------------------------------------------------------------
C     END DCPARM
