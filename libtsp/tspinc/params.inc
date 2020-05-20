C    @(#)params.inc	4.3 9/21/93
C     -                       PARAMS.INC
C  Revs:
c    Apr/07/93 - DEM: Added MAXMAC for number of gen cards in input file
c    May/04/92 - DEM: Added MXPOOL for size of memory pool
c    Mar/10/92 - DEM: Added MAXKV for number of different nominal KV's
c    Mar/09/92 - DEM: Added MXLSHD for number of load shed input cards
c    Jan/13/92 - DEM: Raised MAXBRN for budget cases with lots
c      of series sections.
c     -
      integer MAXBUS, MAXGEN, MAXBRN, MAXWRK, MXLSFR, MXITFR,
     &        MAXPTS, MAXECN, MAXECD, MAXSTP, MXLSHD, MAXKV,
     &        MXPOOL, MAXMAC, GOVSLOTS, TURBSLOTS, MAXCV, CVSLOTS,
     &        HCCVS
      parameter (MAXBUS    = 13000)
      parameter (MAXGEN    = MAXBUS/10)
      parameter (MAXBRN    = MAXBUS*5/2)
      parameter (MAXWRK    = MAXBUS/5)
      parameter (MXLSFR    = 50)
      parameter (MXITFR    = MAXBUS/2)
      parameter (MAXPTS    = 13000)
      parameter (MAXECN    = 80000)
      parameter (MAXECD    = 32000)
      parameter (MAXSTP    = 800)
      parameter (MXLSHD    = 1200)         !
      parameter (MAXKV     = MAXBUS*2/100) !
      parameter (MXPOOL    = 4000)         !
      parameter (MAXMAC    = MAXBUS*2/3)   !
      parameter (MAXCV     = 100)          ! max nonlinear curves 
      parameter (GOVSLOTS  = 25)           ! avg space per gov in govdat
      parameter (TURBSLOTS = 10)           ! avg space per turb in turbdat
      parameter (CVSLOTS   = 10)           ! avg space per cv in cvdat
      parameter (HCCVS     = 2)            ! # of hardcoded nonlinear curves
C     -                 end of PARAMS.INC
