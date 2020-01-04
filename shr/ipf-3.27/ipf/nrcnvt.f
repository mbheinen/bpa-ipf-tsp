C    @(#)nrcnvt.f	20.4 7/18/96
        subroutine nrcnvt

      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/alpha2.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/tran.inc'
      include 'ipfinc/xtran.inc'
 
      save

        icount = 1
        do 130 i = 1,ntot
        if (ikk(1,i+ntota).eq.1) then
           kvolt(i) = i
        else
           kvolt(i) = 0
        endif
        iflag(i) = icount
        i5 = ikk(5,i+ntota)
        if (i5.gt.0) then
  120      if (jndx(1,i5).eq.i+ntota) then
              jflag(1,icount) = jndx(2,i5)
              jflag(2,icount) = jndx(3,i5)
              icount = icount + 1
              i5 = i5 + 1
              go to 120
           endif
        endif
  130   continue
        iflag(ntot+1) = icount
        icount = 1
C                         Recalculate Ykm pointers
        xtrflg = 1
        do 180 jt = 1,ntota
        kt = ltran(1,jt)
        mt = ltran(9,jt)
        xtran(1,jt) = ltran(3,jt)
        xtran(2,jt) = ltran(12,jt)
        ltran(3,jt) = 0
        ltran(12,jt) = 0
        do 160 l = km(kt), km(kt)-1+kmlen(kt) 
           if (ikmu(l).eq.mt) then                    
              ltran(3,jt) = l
           endif
  160   continue
        do 170 l = km(mt), km(mt)-1+kmlen(mt) 
           if (ikmu(l).eq.kt) then                    
              ltran(12,jt) = l
           endif
  170   continue
        lt = jckikk(jt,7)
        ityp = mod (ltran(10,jt), 100)
        ltran(10,jt) = ityp + 100*lt
  180   continue
        return
 
        entry nrrstr   ! converts data from IKK to alpha tables
C                      ! Reset Ykm pointers
        xtrflg = 2
        do 210 jt = 1,ntota
           itemp = xtran(1,jt)
           xtran(1,jt) = ltran(3,jt)
           ltran(3,jt) = itemp
           itemp = xtran(2,jt)
           xtran(2,jt) = ltran(12,jt)
           ltran(12,jt) = itemp
           ltran(10,jt) = mod(ltran(10,jt),100)
  210   continue
        return
        end
