C    @(#)nrtxaj.f	20.3 2/13/96
        subroutine nrtxaj (jt,dt,origin)

C       Makes LTC adjustments into the Y-matrix and the  
C       area interchange tie branches

        character origin*(*)
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc' 
c	Global variables used:
c		gkmu(r*8), bkmu(r*8)
      include 'ipfinc/alpha2.inc'
c	Global variables used:
c		None
      include 'ipfinc/area.inc'
c	Global variables used:
c		tie(r*8)
      include 'ipfinc/blank.inc'
c	Global variables used:
c		None
      include 'ipfinc/ecvar.inc'
c	Global variables used:
c		idswa
      include 'ipfinc/lfiles.inc'
c	Global variables used:
c		dbug
      include 'ipfinc/tran.inc'
c	Global variables used:
c		tap(r*8), tran(r*4), ltran(i*4)
 
C
C       This subroutine makes LTC adjustments into the Y-matrix and the
C       area interchange tie branches
C
        if (abs(dt).lt.1.0e-10) go to 900
        ityp = mod(ltran(10,jt),100)
        k=ltran(1,jt)   
        m=ltran(9,jt)   

C       Check for LTC TIE line

        lt = ltran(10,jt) / 100 
        la1=ltran(3,jt) 
        la2=ltran(12,jt)
        tnew=tap(jt)+dt 
        dx = dt - ddim(dble(tnew),dble(tran(7,jt)))
     &          + ddim(dble(tran(8,jt)),dble(tnew)) 
        if (mod (ityp,10) .eq. 3) then  

C          LTC phase shifter adjustment   

           c=cos(dx)
           s=sin(dx)
           gkm = gkmu(la1)                         
           bkm = bkmu(la1)                         
           gkmu(la1) = gkm*c-bkm*s 
           bkmu(la1) = bkm*c+gkm*s 
           gkm = gkmu(la2)                         
           bkm = bkmu(la2)                         
           gkmu(la2) = gkm*c+bkm*s 
           bkmu(la2) = bkm*c-gkm*s 
           if (lt.gt.0) then
              if (tie(1,lt).eq.ltran(1,jt)) then   
                 tie(5,lt)=gkmu(la1)               
                 tie(6,lt)=bkmu(la2)               
              else  
                 tie(5,lt)=gkmu(la2)               
                 tie(6,lt)=bkmu(la2)               
              endif 
           endif
        else

C          LTC transformer adjustment 

           xh=(tap(jt)+dx)/tap(jt)  
           gkm = gkmu(la1)                         
           bkm = bkmu(la1)                         
           gkmu(la1) = gkm*xh  
           bkmu(la1) = bkm*xh  
           gkmu(la2) = gkmu(la1)  
           bkmu(la2) = bkmu(la1)  
           xh=(xh**2-1.0)*tap(jt)   
           gkku(m) = gkku(m) -gkm*xh  
           bkku(m) = bkku(m) -bkm*xh  
           if(lt.gt.0) then 
              tie(5,lt)=gkmu(la1)                  
              tie(6,lt)=bkmu(la1)                  
              if (ltran(1,jt).ne.tie(1,lt)) then   
                 tie(3,lt) = tie(3,lt) - gkm*xh 
                 tie(4,lt) = tie(4,lt) - bkm*xh 
              endif 
           endif
        endif   
        if(idswa.ne.0) write(dbug,120)origin,jt,k,m,lt,dt,tap(jt),dx
  120   format(' LTC adjustment - ',a,4i5,3f12.5)   
        tap(jt)=tap(jt) + dx
  900   continue
        return  
        end 
