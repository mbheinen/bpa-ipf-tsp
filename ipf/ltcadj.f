C    @(#)ltcadj.f	20.4 7/18/96
        subroutine ltcadj(jt, dt, origin)   

C       This subrouine makes LTC adjustments into the Y-matrix, the 
c       TXTIE array, and the area interchange tie branches.   

        character origin*(*)
        double precision dt 
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/beta.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/optim1.inc'
      include 'ipfinc/tran.inc'
 
        double precision dx, tnew, c, s, xh

        ityp = mod(ltran(10,jt),100)  
        lt = ltran(10,jt)/100 
        k=ltran(1,jt)
        m=ltran(9,jt)
        lt=jckikk(jt,7)
        lx1=jckikk(jt,11)
        lx2=jckikk(jt,12)
        la1=ltran(3,jt)
        la2=ltran(12,jt)
        tnew=tap(jt)+dt 
        dx = dt - ddim(dble(tnew),dble(tran(7,jt)))
     &          + ddim(dble(tran(8,jt)),dble(tnew)) 
        gkm = gkmu(la1)                            
        bkm = bkmu(la1)                            
        if (ityp .eq. 3) then   

C          LTC phase shifter adjustment 

           c=dcos(dx)
           s=dsin(dx)
           gmk=gkm*c-bkm*s  
           bmk=bkm*c+gkm*s  
           gkmu(la1) = gmk 
           bkmu(la1) = bmk 
           if (lt.gt.0) then
              if (tie(1,lt) .eq. k .and. tie(7,lt) .eq. m) then   
                 tie(5,lt) = gmk
                 tie(6,lt) = bmk
              endif 
           endif
           if (lx1.gt.0) then   
              txtie(4,lx1) = tnew   
              txtie(5,lx1) = gmk
              txtie(6,lx1) = bmk
           endif
           gkm = gkmu(la2)                         
           bkm = bkmu(la2)                         
           gmk=gkm*c+bkm*s  
           bmk=bkm*c-gkm*s  
           gkmu(la2) = gmk 
           bkmu(la2) = bmk 
           if (lt.gt.0) then
              if (tie(1,lt) .eq. m .and. tie(7,lt) .eq. k) then   
                 tie(5,lt) = gmk
                 tie(6,lt) = bmk
              endif 
           endif
           if (lx2.gt.0) then   
              txtie(4,lx2) = -tnew  
              txtie(5,lx2) = gmk
              txtie(6,lx2) = bmk
           endif
        else

C          LTC transformer adjustment

           xh=(tap(jt)+dx)/tap(jt)  
           gmk=gkm*xh   
           bmk=bkm*xh   
           gkmu(la1) = gmk 
           bkmu(la1) = bmk 
           gkmu(la2) = gmk 
           bkmu(la2) = bmk 
           if(lt.gt.0) then 
              tie(5,lt)=gmk 
              tie(6,lt)=bmk 
           endif
           if (lx1.gt.0) then   
              txtie(5,lx1) = gmk
              txtie(6,lx1) = bmk
           endif
           xh=(xh**2-1.0)*tap(jt)   
           gkku(m) = gkku(m) -gkm* xh 
           bkku(m) = bkku(m) -bkm * xh
           if (lt.gt.0) then
              if (tie(1,lt) .ne. ltran(1,jt)) then 
                 tie(3,lt)=tie(3,lt)-gkm*xh 
                 tie(4,lt)=tie(4,lt)-bkm*xh 
              endif 
           endif
           if (lx2.gt.0) then   
              txtie(5,lx2) = gmk
              txtie(6,lx2) = bmk
              txtie(9,lx2) = txtie(9,lx2) - gkm * xh
              txtie(10,lx2) = txtie(10,lx2) - bkm * xh  
           endif
        endif   
        if (idswa .ne. 0) then  
           write (dbug,120) origin, jt, k, m, lt, tap(jt), dt, dx   
  120      format(' LTC ADJUSTMENT - ',a,4i5,3e11.4)
        endif   
        tap(jt) = tap(jt) + dx  
        return  
        end 
