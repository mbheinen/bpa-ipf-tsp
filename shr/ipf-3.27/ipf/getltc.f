C    @(#)getltc.f	20.7 10/13/99
        subroutine getltc
C
C       This subroutine determines the feasibility of LTC control
C       by three criteria:
C
C       1. Controlled bus has local reactive supply.
C
C       2. Location of a reactive supply from the controlling terminal
C          side either with the discovery of a loop to the controlled
C          terminal, or the presence of a PV bus in the controlling
C          circuit.
C
C       3. Compute LTC voltage/tap sensitivities:
C
C          SEN = DELTA_V_CONTROLLED / DELTA_TAP
C
C       If tests (1) and (2) are negative, (3) is performed.
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/beta.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/dcsln.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnphs.inc'
      include 'ipfinc/tran.inc'

        save
        dimension level(100)
        
	double precision ek,fk,ak

        entry ltcint        ! Convert voltages to polar form
        do 410 kt=1, ntot
           ek=e(kt)
           fk=f(kt)
           vk=dsqrt(ek**2+fk**2)
           e(kt)=vk
           if (vk .gt. 0) f(kt)=datan2(fk,ek)
  410   continue
        msw = 0
C
C       Factor and store C-matrix
C
        do 100 i = 1,ntot+ntota+1
           ikkind(1,i) = 1
  100   ikkind(2,i) = 1
 
        ikec=1
        msw = 1
        call dcfact (ikkind)
        return  

c       ===============================================================
        entry ltcsen (jt,sen)   
C       
C       Compute LTC sensitivities   
C       
        if (ltran(10,jt) .eq. 1) then   
           kt = ltran(1,jt) 
           mt = ltran(9,jt) 
           nt = 0   
           lt = 0   
           if (ltran(2,jt) .eq. -1) then
              nt = kt   
              lt = mt   
           else if (ltran(2,jt) .eq. -2) then   
              nt = mt   
              lt = kt   
           else if (ltran(2,jt) .gt. 0) then
              nt = ltran(2,jt)  
              lt = 0
           endif
           kta = kt + ntota 
           mta = mt + ntota 
           nta = nt + ntota 
           la1 = ltran(3,jt) 
           gkm = gkmu(la1)                         
           bkm = bkmu(la1)                         
       
           if (gkm ** 2 + bkm ** 2 .gt. 0.0) then
C
C             High-impedance Tx's automatically excluded
C       
              sen = 0.0
              if (idswa .ne. 0) write (dbug,220) jt,kt,mt,nt,sen   
              return   
           endif
C       
C          1. Check topology for PV node or non-controlled node 
C       
           if (ikk(1,kta) .eq. 1 .or. ikk(1,mta) .eq. 1) go to 210  
           if (lt .ne. 0) then  
              if (ikk(1,lt+ntota) .eq. 1) go to 210 
              ilevel = 0
              last = 1  
              next = 2  
              level(1) = nt 
              level(2) = lt 
C       
C             Select kernel node
C       
  120         i1 = last+1   
              i2 = next 
              last = next   
              ilevel = ilevel + 1   
              if (ilevel .gt. 4) go to 200  
              do 150 ix = i1,i2 
              i = level(ix) 
              do 140 l = km(i), km(i)-1+kmlen(i)  
              j = ikmu(l)                          
              do 130 k = 1,next 
                 if (level(k) .eq. j) go to 140
                 if (ikk(1,j+ntota) .eq. 1) go to 210  
  130         continue  
              if (next .lt. 100) then   
                 next = next + 1
                 level(next) = j
              else  
                 go to 200  
              endif 
  140         continue  
  150         continue  
              if (last+1 .le. next) go to 120   
           endif
  200      continue 
           go to 300
C       
C          Feasibility established topologically
C       
  210      sen = 1.0
           if (idswa .ne. 0) write (dbug,220) jt,kt,mt,nt,sen   
  220      format (' LTC SENSITIVITY ',4i5,' VALUE: ',e11.3)
           return   
C       
  300      do 310 i = nbslck+ntota,ntotx
  310      dpt(1,i) = 0.0d0
           if (ikk(1,kta) .eq. 2 .or. ikk(1,mta) .eq. 2) then   
              la1 = ltran(3,jt) 
              if (ikk(1,kta) .eq. 2) then   
                 dpt(1,kta) = e(mt) * bkmu(la1) / tap(jt)  
              endif 
              if (ikk(1,mta) .eq. 2) then   
                dpt(1,mta)=e(kt)*bkmu(la1)/tap(jt)-2.0*e(mt)*bkmu(la1)
              endif 
              if (nt .eq. 0) then   
                 sen = 0.0  
              else if (ikk(1,nta) .eq. 1) then  
                 sen = 1.0  
              else  
                 call dwnbak (ikkind)   
                 if (idswa .ne. 0) write (dbug,320) jt,kt,mt,nt,
     1              dpt(1,kta),dpt(1,mta),e(mt),-e(mt)**2/e(kt) 
  320            format (' LTC SENSITIVITY ',4i5,' VALUE: ',4e11.3) 
C       
C                Test for insufficient reactive control -   
C                Actual sensitivities are less than no-load 
C                sensitivities, or are infeasible, i.e.,
C       
C                dVk/dt > Vm or dVk/dt < 0 or   
C                dVm/dt > 0 or  dVm/dt < -Vk/Vm**2  
C       
                 sen = sngl(dpt(1,nta))
                 if (nt .eq. kt) then   
                     if (sen .gt. e(mt) .or. sen .lt. 0.0) sen = 0.0
                 else if (nt .eq. mt) then  
                     if (sen .lt. -e(kt)/e(mt)**2 .or. sen .gt. 0.0)
     1                  sen = 0.0   
                 endif  
              endif 
           else 
              sen = 0.0 
           endif
        else
           sen = 0.0
        endif   
        if (idswa .ne. 0) write (dbug,680) jt,kt,mt,nt,sen  
  680   format (' LTC SENSITIVITY ',4i5,' VALUE: ',e11.3)   
        return  



        entry ltcfin
C       
C       Restore voltages to rectangular form
C       
        do 690 kt=1,ntot
           vk=e(kt)
           ak=f(kt)
           e(kt) = vk * dcos(ak)
           f(kt) = vk * dsin(ak)
  690   continue
        end 
