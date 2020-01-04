C     added by Yu Wang for BPA RANI and GE TCSC control test.
      integer VYMNMAX
      parameter (VYMNMAX = 25)
      common /vymn1/ iranityp(VYMNMAX), xtcscma(VYMNMAX),
     1               xtcscmi(VYMNMAX), xtcscge(VYMNMAX), 
     2               rtcscyw(VYMNMAX), xtcscyw(VYMNMAX),
     3               dangl(VYMNMAX), dangl0,xfcyw(VYMNMAX),
     4               iparl(VYMNMAX)
      common /vymn2/ vymcf(VYMNMAX), vymarf0(VYMNMAX), vymca0(VYMNMAX),
     1               vymarge(VYMNMAX), zsyn1ge(VYMNMAX), 
     2               zsyn2ge(VYMNMAX), igeon(VYMNMAX),
     3               ptie0(VYMNMAX), ptie1(VYMNMAX),
     4               ptie(VYMNMAX),iptie
      common /vymn3/ tnxyw0, blok0(VYMNMAX), p4blok0(VYMNMAX),
     1               iblok(VYMNMAX), blocker(VYMNMAX), ibtyp(VYMNMAX),
     2               roota1, roota2, rootb1, rootb2, togflag
      complex roota1, roota2, rootb1, rootb2
      real togflag
