C FORTELT GRLF01
C---------------------------------------------------------------------
C
C        RELEASE    3        VERSION    1         JULY 15, 1985
C
C---------------------------------------------------------------------
C
C                        DC input common areas
C
C     DC converter data
C
C        DEFINITION OF COMMON VARIABLES:
C                  AQFLAG -  flag for specification of Q flow.
C                  AQVAL  -  value of Q to be held.
C                  BIASR  -  voltage bias factor. (p.u.)
C                  CNAREA -  internal area number for converter.
C                  CONIMX -  line current rating.
C                  NDCCON -  number of dc converters.
C                  PQDCAC -  power per unit factor.
C                  PRUNIT -  flag indicating that values are per
C                            unitized.
C                  SET    -  set point 1 and 2 (adjusted).
C                  VACDC  -  voltage per unit factor.
C                  XCCONV -  commutating reactance in per unit.
C
C
C---------------------------------------------------------------------
C
      real          aqval(ndcc),  biasr(ndcc),  conimx(ndcc),
     1              pqdcac(ndcc), set(2,ndcc),  vacdc(ndcc),
     2              xcconv(ndcc)
C
      integer       cnarea(ndcc), ndccon
C
      logical       aqflag(ndcc), prunit
C
      character*8   ground
C
C
      common/lfdata/    aqflag,        aqval,         biasr,
     1   cnarea,        conimx,        ndccon,        pqdcac,
     2   prunit,        set,           vacdc,         xcconv
C
      common/lfdatc/    ground
C
C
C---------------------------------------------------------------------
C
C     DC line data
C
C        DEFINITION OF COMMON VARIABLES:
C                  DCAREA -  internal area number for line.
C                  NDCBRK -  number of dc breakers.
C                  NDCLIN -  number of dc lines.
C                  RDCLIN -  line resistance (p.u.)
C
C
C---------------------------------------------------------------------
C
      real          rdclin(ndcl)
C
      integer       dcarea(ndcl), ndcbrk, ndclin
C
C
      common/lfline/    dcarea,        ndcbrk,        ndclin,
     1    rdclin
C
C
C---------------------------------------------------------------------
C
C     Data from the miscellaneous data card.
C
C        DEFINITION OF COMMON VARIABLES:
C                  FITLIM -  full convergence iteration limit.
C                  FULTOL -  full convergence tolerance values for
C                            each equation type.
C                  LDCONV -  count of number of iterations required
C                            for convergence.
C                  MDF    -  indicates full tolerance MD card.
C                  MDP    -  indicates partial tolerance MD card.
C                  PARTOL -  partial convergence tolerance values.
C                  PITLIM -  partial convergence iteration limit.
C                  QCENTR -  indicator that central controller should
C                            be activated.
C                  QDCLIM -  indicator that iteration limit was
C                            reached.
C                  QDCONV -  indicator that convergence has been
C                            obtained.
C                  QFINAL -  indicator that full convergence should be
C                            obtained.
C                  QFIRST -  indicator of first call to DC power flow.
C                  QLIMIT -  indicator that local limits should be
C                            enforced.
C
C
C---------------------------------------------------------------------
C
      real          fultol(9),    partol(9)
C
      integer       fitlim,       icarea(ndcc), ldconv,
     1              pitlim
C
      logical       mdf,          mdp,          qcentr,
     1              qdclim,       qdconv,       qfinal,
     2              qfirst,       qlimit
C
C
      common/lfmisc/    fitlim,        fultol,        icarea,
     1   ldconv,        mdf,           mdp,           partol,
     2   pitlim,        qcentr,        qdclim,        qdconv,
     3   qfinal,        qfirst,        qlimit
C
C---------------------------------------------------------------------
C     END GRLF01
