C    @(#)senltc.f	20.3 2/13/96
      integer function senltc (jt,inquir)
C
C     compute Jacobian elements for LTC's.
C     Elements are stored in KOLUM, ROWH, ROWN, ROWJ, ROWL,
C     KOLUM, and DPT.
C
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha2.inc'
      include 'ipfinc/beta2.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/gamma.inc'
      include 'ipfinc/ordsta.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/slnphs.inc'
      include 'ipfinc/tran.inc'
 
        common /txstat/ txstat(MAXLTC)
        integer txstat
C
C     Compute Jacobian elements for LTC
C
C     SENLTC assignments - 0: 1*DT = DX
C                          1: 1*DT + 100000.0*DV = 0.0
C                          2: 1*DT + H*DA + N*DV = DP or
C                             1*DT + H*DA + N*DV = DQ
C
      ityp = mod(ltran(10,jt),100)
      lt = ltran(10,jt)/100
      kt=ltran(1,jt)
      mt=ltran(9,jt)
      if (ordltc .eq. 1) then   
         kt = inp2opt(kt) 
         mt = inp2opt(mt) 
      endif 
      senltc = txstat(jt)   
      dx = 0.0  
      istat = 1000*ityp 
C                If "LTC" option is not V-control, disable V-control
      if (senltc .eq. 0 .or. (senltc .eq. 1 .and. kspare(19) .ne. 2))   
     1   then   
C               Manual/inactive LTC's have identity elements in Jacobian.   
         call senctl (jt,ityp,senltc,0,dx,dy)   
      else if (senltc.eq.1) then
C               Auto NR adjustment in Jacobian is invoked by the
C               following contraint:
C                             0.000001*Dt + 1.0*Dv   = Dv(error)
        if (ityp .eq. 3) then   
           call senctl (jt,ityp,senltc,0,dx,dy) 
        else
           nt = ltran(2,jt) 
           kc = 0   
           if (nt .eq. -1) then 
              kc = kt   
           else if (nt .eq. -2) then
              kc=mt 
           else if (nt .gt. 0) then 
              if (ordltc .eq. 1) then   
                 kc = inp2opt(nt) 
              else  
                 kc = nt
              endif 
           endif
           call senctl (jt,ityp,senltc,kc,dx,dy)
        endif   
      else  
C                   Auto NR adjustment in Jacobian is invoked by the
C                   following contraint:
C                               1.0*Dt + RH*Da + RN*Dv = Dq(error)  
        call senctl (jt,ityp,senltc,0,dx,dy)
      endif 
      return
      end   
