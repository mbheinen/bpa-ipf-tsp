
      integer MAXCRV
      parameter (MAXCRV=300)
      common /pqcurves/ numcurv, pg_sched(MAXCRV),pqpgen(-1:15,MAXCRV), 
     &                  pqqmin(-1:15,MAXCRV), pqqmax(-1:15,MAXCRV), 
     &                  pqbusptr(MAXCRV), pqid(MAXCRV),
     &                  pqactive(MAXCRV), pqnumunit(MAXCRV), 
     &                  buspqptr(MAXBUS), pq_flag
      logical pq_flag, pqactive
      integer numcurv, pqnumunit, pqbusptr, buspqptr
      character pqid*2
      real pg_sched, pqpgen, pqqmin, pqqmax

c     numcurv     = The number of p-q curves
c     pg_sched()  = Scheduled pgen
c     pqpgen()    = Pgen values for each point on the curve
c                   Notes: "-1" subscript refers to constant MVA
c                               model (Pmax, +PF, -PF)
C                          "0"  subscript refers to Pmax
c     pqpmax()    = Optional Pmax limit 
c     pqqmin(), pqqmax() = corresponding Q limits
c     pqid()      = Machine ID
c     pqnumunit() = Number of units 
c     pqbusptr()  = Cross index pointer from pq to bus
c     pqactive()  = TRUE when the curve is active, and
c                   FALSE when the curve is inactive.
c     buspqptr    = Cross-index pointer from bus to pq 
c     pq_flag     = TRUE if ptrpqdta is up to date

