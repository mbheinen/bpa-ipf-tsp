C    @(#)nragc.f	20.3 2/13/96
        subroutine nragc
C                 establishes / AGC schemes and stores their
C                 Jacobian elements.
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/agc.inc'
c	Global variables used:
c		kagc, agc, agcmtx(r*8), numagc
      include 'ipfinc/alpha.inc'
c	Global variables used:
c		pnetu(r*8)
      include 'ipfinc/blank.inc'
c	Global variables used:
c		ntota
      include 'ipfinc/bus.inc'
c	Global variables used:
c		inp2opt
      include 'ipfinc/ecvar.inc'
c	Global variables used:
c		idswb
      include 'ipfinc/gamma.inc'
c	Global variables used:
c		kolum, korder, rowh(r*8), rown(r*8)
      include 'ipfinc/lfiles.inc'
c	Global variables used:
c		dbug
      include 'ipfinc/slnopt.inc'
c	Global variables used:
c		option
C
        common /agcxxx/ dpgen(MAXAGC), dperr(MAXAGC),
     1                  pviol(MAXAGC), status(MAXAGC)
        integer status
 
        double precision ptot, pct
        
        if (numagc .eq. 0) go to 900
C                     Initialize AGC control
        do  i=1,MAXAGC 
           status(i)=0
        end do
        ikec = 1
C                     Establish status of each participating generator. 
C                     KAGC(10,*) = STATUS   
C                     0 : Initial condition (AGC initially is not   
C                         implemented). 
C                     1 : generator constraint is AGC.  
C                     2 : generator constraint is P_max.
C                     3 : generator constraint is P_min.
        dlptot = 0.0
        dlxtot = 0.0
        pcttot = 0.0
        pcxtot = 0.0
        numvio = 0  
        do 60 jt = 1,numagc 
           kagc(11,jt) = 0  
           nb = kagc(1,jt)  
           kt = inp2opt(nb)   
           call nrpqv (kt, pk, dpk, qk, dqk, vk)
           agc(13,jt) = pk + agc(8,jt)  
           dpgen(jt) = agc(13,jt) - agc(7,jt)   
           pviol(jt) = dim (agc(13,jt), agc(5,jt))  
     1               - dim (agc(6,jt), agc(13,jt))  
           if (abs(pviol(jt)) .lt. option(5)) pviol(jt) = 0.0   
           dlptot = dlptot + dpgen(jt)  
           pcxtot = pcxtot + agc(4,jt)  
           status(jt) = kagc(10,jt) 
           if (status(jt) .eq. 0) then  
C                   Initial conditions. Set up AGC for all candidates.  
              kagc(10,jt) = 1   
              pcttot = pcttot + agc(4,jt)   
              dlxtot = dlxtot + dpgen(jt)   
              pnetu(kt) = pk   
           else if (status(jt) .eq. 1) then 
              dlxtot = dlxtot + dpgen(jt)   
C                  Determine whether any AGC units have violated limits.  
              if (pviol(jt) .eq. 0.0) then  
                 pcttot = pcttot + agc(4,jt)
                 pnetu(kt) = pk
              else if (pviol(jt) .gt. 0.0) then 
                 kagc(10,jt) = 2
                 pnetu(kt) =agc(5,jt) - agc(8,jt)  
                 numvio = numvio + 1
              else  
                 kagc(10,jt) = 3
                 pnetu(kt) =agc(6,jt) - agc(8,jt)  
                 numvio = numvio + 1
              endif 
           else 
           endif
   60   continue
C           Determine whether any AGC units locked at P_min or P_max
C           can be reactivated to AGC control. This is contingent upon  
C           no generators violating their P_limits. We look at STATUS(*)
C           instead of KAGC(10,*) because we are interested in their
C           prior state.
        if (numvio .eq. 0) then 
           do 70 jt = 1, numagc 
              if (status(jt) .eq. 2) then   
                 pct = agc(4,jt)
                 dp = pct / (pcttot + pct) * (dlxtot + dpgen(jt))   
                 if (dp .lt. dpgen(jt)) then
                    pcttot = pcttot + agc(4,jt) 
                    kagc(10,jt) = 1 
                 endif  
              else if (status(jt) .eq. 3) then  
                 pct = agc(4,jt)
                 dp = pct / (pcttot + pct) * (dlxtot + dpgen(jt))   
                 if (dp .gt. dpgen(jt)) then
                    pcttot = pcttot + agc(4,jt) 
                    kagc(10,jt) = 1 
                 endif  
              endif 
   70      continue 
        endif   
C       
C       Using the new AGC status, determine whether under the AGC   
C       system has constrained its slack bus at its P_limits. If so,
C       assign a new temporary slack bus to the AGC system. 
C       
        islk = 0
        ibig = 0
        
        do 80 jt = 1, numagc
           if (kagc(9,jt) .gt. 0) islk = jt 
   80   continue
C       
C       If KAGC(10,ISLK) >< 1, a temporary slack bus must be selected   
C       from the set of active AGC units.   
C       
        if (kagc(10,islk) .ne. 1) then  
        
           do 90 jt = 1, numagc 
              if (kagc(9,jt) .eq. 0) then   
                 if (kagc(10,jt) .eq. 1) then   
                    if (ibig .eq. 0) then   
                       ibig = jt
                    else if (agc(13,jt) .gt. agc(13,ibig)) then 
                       ibig = jt
                    endif   
                 endif  
              endif 
   90      continue 
        
           if (ibig .eq. 0) then
C       
C             No available slack bus. AGC defaults to the slack bus 
C             regardless of its state.  
C       
              nb = kagc(1,islk) 
              kt = inp2opt(nb)
              pnetu(kt) = agc(13,islk) - agc(8,islk)   
              pcttot = pcttot + agc(4,islk) 
              kagc(10,islk) = 1 
        
           else 
        
              kagc(11,ibig) = islk  
        
           endif
        
        endif   
C       
C       Now assemble the controls.  
C       
        ptot = 0.0  
        do 110 jt = 1, numagc   
        
           nb = kagc(1,jt)  
           kt = inp2opt(nb)   
           if ((kagc(10,jt) .eq. 1) .or.
     1         (ibig. ne. 0 .and. islk .eq. jt)) then   
        
C             Store DJ/DX elements for either of the two sets of
C             constraints.  
C       
C             1. The set of AGC constraints (all active generators and  
C                the area/system slack bus. 
C       
C             2. P_min or P_max of the area/system slack bus.   
C       
              kagc(2,jt) = ikec 
              agcmtx(ikec) = kt+ntota   
              if (kagc(10,jt) .eq. 1) then  
                 ptot = ptot + agc(13,jt) - agc(7,jt)   
                 agcmtx(ikec+1) = agc(13,jt)
                 agcmtx(ikec+2) = agc(4,jt) / pcttot
              else  
                 agcmtx(ikec+1) = agc(13,jt)
                 agcmtx(ikec+2) = 0.0   
              endif 
              ikec = ikec+3 
              call jacbus (kt,1)
              lp = 0
  100         lp = korder(lp)   
              if (lp.gt.0) then 
                 agcmtx(ikec) = kolum(lp)   
                 agcmtx(ikec+1) = rowh(lp)  
                 agcmtx(ikec+2) = rown(lp)  
                 ikec=ikec+3
                 go to 100  
              endif 
              kagc(3,jt) = ikec - 1 
           else 
              kagc(2,jt) = 0
           endif
  110   continue
        
        if (idswb.gt.0) then
        
           do 130 jt = 1,numagc 
              if (ptot .ne. 0.0) then   
                 pctdel = dpgen(jt) / ptot  
              else  
                 pctdel = agc(4,jt) 
              endif 
              nb = kagc(1,jt)   
              kt = inp2opt(nb)
              write (dbug, 120) kt, kagc(10,jt), kagc(11,jt),   
     1           agc(13,jt), agc(7,jt), dpgen(jt), agc(4,jt) * 100.0,   
     2           pctdel * 100.0 
  120         format(' NRAGC ',i4, ' STATUS ',2i4, ' PGEN ', 3f12.3,
     1           ' % ', 2f7.1)  
  130      continue 
        
        endif   
  900   continue
        return  
        end 
