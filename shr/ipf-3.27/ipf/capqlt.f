C    @(#)capqlt.f	20.3 2/13/96
        subroutine capqlt
 
        include 'ipfinc/parametr.inc'

        include 'ipfinc/alpha.inc'
c	Global variables used:
c		bkku (r*8), qnetu(r*8), ntypu
        include 'ipfinc/apcom.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
c	Global variables used:
c		e(r*8), f(r*8), inp2opt
        include 'ipfinc/cont.inc'
        include 'ipfinc/ikk.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/tbx.inc'
c	Global variables used:
c		tbx(r*8)
        include 'ipfinc/time1.inc'
C
c	Local variables:
c
	double precision vsq, qsq
c
        igenq = 0
        if (irect .ne. 0) go to 190

C       Search special bus table and adjust types

        do 180 jt=1,ntotb   
           ltyp = tbx(1,jt)   
           ityp = tbx(7,jt)   
           kt = tbx(2,jt) 
           kt = inp2opt(kt)  
           if (ikk(1,kt).eq.0) go to 180   
           ntyp = ntypu(kt)                            
           if (ityp .gt. 0 .and. ityp .le. 6) then 
C       
C             Normal   
C       
              go to (100,110,120,110,130) ltyp 
  100         go to (140,140,150,150) ityp 
  110         go to (160,160,160,160,160,160) ityp 
  120         go to (155,140,140,155,155) ityp 
  130         go to (160,140,140,140) ityp 
           else
C       
C             Error
C       
              go to (140,160,160,160,140) ltyp 
           endif   
C       
C          Type  "BG", "BX", or "BO" bus in state PQmin or PQmax   
C       
  140      ntyp = 1
           go to 170   
C       
C          Type "BV" bus in state PVmin or PVmax   
C       
  150      ntyp = 2
           go to 170   
C       
C          Type "BG" in state PV; ignore any shunt reactance.  
C       
  155      ntyp = 7
           igenq = igenq + 1   
           iqlim(igenq) = kt   
           shunt(igenq) = 0.0  
           vsq = e(kt)**2 + f(kt)**2   
           vhold(igenq) = sqrt (vsq)   
           qstate(igenq) = 1   
C          
C          Bias Q-limits with Qnet prior to network reduction. 
C          Afterwards, Qnet will be reinstated, but with possible 
C          disributed injections added.  
C       
           qlow(igenq) = tbx(4,jt) - qnetu(kt)         
           qhi(igenq) = tbx(3,jt) - qnetu(kt)          
           go to 170   
C       
C          Type "BQ", "BO", or "BX" in state PV, or
C          Type "BQ" in state PQmin or PQmax   
C       
  160      ntyp = 7
           igenq = igenq + 1   
           iqlim(igenq) = kt   
           shunt(igenq) = tbx(6,jt)
           vsq = e(kt)**2 + f(kt)**2   
           vhold(igenq) = sqrt (vsq)   
           qstate(igenq) = 1   
           qsq = tbx(6,jt) * vsq   
C       
C          BQ buses in state Qmax with capacitance or in state Qmin 
C          with reactance are an aberration (because of switching 
C          tolerances), and are frozen into this state. 
C       
           if ((ltyp .eq. 2 .and. ityp .eq. 3 .and. qsq .lt. 0.0) .or. 
     1         (ltyp .eq. 2 .and. ityp .eq. 4 .and. qsq .gt. 0.0)) then
              igenq = igenq - 1
              ntyp = 1 
              go to 170
C       
C          BQ buses in state Qmin with capacitance or in state Qmax 
C          with reactance already have susceptance removed from 
C          Y-matrix.   
C       
           else if ((ltyp .eq. 2 .and. ityp .eq. 3 .and. qsq .gt. 0.0)
     &         .or. (ltyp .eq. 2 .and. ityp .eq. 4 .and. qsq .lt. 0.0))
     &        then 
           else
              bkku(kt) = bkku(kt) -tbx(6,jt) 
              qnetu(kt) = qnetu(kt) +qsq 
        
           endif   
           if (ltyp .eq. 2 .or. ltyp .eq. 4) then  
              qlow(igenq) = tbx(4,jt) + qsq - qnetu(kt)    
              qhi(igenq) = tbx(3,jt) + qsq - qnetu(kt) 
           else
              qlow(igenq) = tbx(4,jt) - qnetu(kt)      
              qhi(igenq) = tbx(3,jt) - qnetu(kt)       
           endif   
           if (ltyp .eq. 2 .and. ityp .eq. 3) then 
              qstate(igenq) = 3
              vhold(igenq) = tbx(5,jt) 
           else if (ltyp .eq. 2 .and. ityp .eq. 4 ) then   
              qstate(igenq) = 4
              vhold(igenq) = tbx(5,jt) 
           endif   
  170      ntypu(kt) = ntyp   
           if (ntyp .ne. 1 .and. idebug .gt. 0) then
              write (dbug,172) kt, ltyp, ityp, qsq, tbx(3,jt), 
     &         tbx(4,jt), qhi(igenq), qlow(igenq), qnetu(kt), 
     &         shunt(igenq)
  172         format(' CAPQLT ',3i5, 7e12.5)
           endif
  180   continue
  190   return  
        end 
