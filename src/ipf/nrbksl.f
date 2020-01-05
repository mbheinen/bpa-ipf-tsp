C    @(#)nrbksl.f	20.9 8/19/99
        integer function nrbksl()
C
        include 'ipfinc/parametr.inc'
 
        include 'ipfinc/alpha.inc' 
        include 'ipfinc/alpha2.inc'
        include 'ipfinc/amtrx.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/ecvar.inc'
        include 'ipfinc/intbus.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/ltcsln.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/slnopt.inc'
        include 'ipfinc/tbx.inc'
        include 'ipfinc/tran.inc'
C
C       Double precision conversion
C
        double precision dv,dt,dp,dq,xn,de,ek,fk,s,c
C
        common /txsens/ ltxsen(MAXLTC), txsens(2,MAXLTC)
        
      nrbksl = 0             ! default return status = successful
      dvmax=0.0 
      dtmax=0.0 
      do 210 kt = ntotx-1,1,-1  
      ik=jndex(2,kt)
      ikstop=jndex(1,kt+1)-1
      xn = amtrx(ik)
      ik=ik+1   
      dt = dpt(1,kt)
      dv = dpt(2,kt)
  130 if (ik.ge.ikstop) go to 150
      mt=amtrx(ik)
      dp=dpt(1,mt)
      dq=dpt(2,mt)
      dt=dt-dp*amtrx(ik+1)-dq*amtrx(ik+3)
      dv=dv-dp*amtrx(ik+2)-dq*amtrx(ik+4)
      ik=ik+5
      go to 130
 
  150 dpt(2,kt)=dv
      dt=dt-xn*dv
      dpt(1,kt)=dt
      dvv = dv
      dvmax = amax1(dvmax,abs(dvv))
      dtt = dt
      dtmax = amax1(dtmax,abs(dtt))
      if (amax1(abs(dtt),abs(dvv)).gt.option(4)) call buscor(kt,dtt,dvv)
  210 continue  
C       
C     Begin NR correction loop with possible under-corrections  
C       
      cx = 1.0  
      if (dtmax.ne.0) cx = amin1(cx,option(10)*1.001/dtmax) 
      if (dvmax.ne.0) cx = amin1(cx,option(11)*1.001/dvmax) 
      if (cx.lt.1.0) then   
         write (outbuf,220) dtmax,dvmax,cx  
  220    format('0Relaxation',f14.5,' D THETA',f19.5,' D V MAX',
     1    f18.5,' Relaxation factor' )  
         call prtout(1) 
         call space(1)  
      endif 
      if (dtmax .gt. 10.0 .or. dvmax .gt. 10.0) then
         nrbksl = 1        ! set return state divergence
         go to 900
      endif
C       
C     LTC transformers adjustments  
C       
      do 250 kt = 1,ntota   
      dt=dpt(1,kt)  
      dtt = dt  
      ityp = mod(ltran(10,kt),100)  
      jtyp = ityp 
      told = tap(kt)  
      k = ltran(1,kt)   
      m = ltran(9,kt)   
      if (ityp.eq.3) then   
         if (abs(dtt).gt.option(10)) then   
            tt = 2.0*option(10) - option(10)**2/abs(dtt)
            dt = sign(tt,dtt)   
            if (idswa.ne.0) write (dbug,230) kt,dpt(1,kt),dtt,dt
  230       format (' Truncated LTC adjustment ',i5,3e12.5) 
            kownta = kownta + 1000  
            jtyp = ityp + 10   
         endif  
         tnew=told+dt   
      else if (ityp .lt. 10) then
         if (abs(dtt).gt.option(11)) then   
            tt = 2.0*option(11) - option(11)**2/abs(dtt)
            dt = sign(tt,dtt)   
            if (idswa.ne.0) write (dbug,230) kt,dpt(1,kt),dtt,dt
            kownta = kownta + 1000  
            jtyp = ityp + 10
         endif  
         tnew=told*(1.0+dt) 
      else  
         if (abs(dtt) .gt. 1.0e-6) then 
            write (dbug,231) kt,dpt(1,kt),dpt(2,kt) 
  231       format (' LTC error: Non-zero residuals for inactive TX ',
     1        i5,2f12.5)
         endif  
         go to 250  
      endif 
      dx = tnew - told  
      dy = -dim(tnew, tran(7,kt)) + dim(tran(8,kt), tnew) 
      tnew = tnew + dy  
      if (idswa .ne. 0 .and. dt .ne. 0.0) then  
         write (dbug,240) kt,k,m,ityp,dt,told,tnew,dx,dy
  240    format (' LTC Adjustment ',8x,4i5,5f12.5)  
      endif 
      if (kownta .le. 10000 .and. abs (dy) .gt. 0.10) then  
         write (errbuf(1), 232) intbus(k), intbas(k), intbus(m),
     1      intbas(m), dtt, dy  
  232    format (' Possible pathological LTC control scheme ',  
     1      a8, f6.1, 1x, a8, f6.1, ' d_tap ',f6.3, 
     2      ' truncation ', f6.3)   
         call prterx ('W', 1)   
         ltcsln(kt) = 1 
      endif 
C       
C     LTXSEN(1,KT) = 1 : Da input, Dpkm output (calculated in JACLTC)
C                    2 : Dpkm input, Da output (calculated in NRBKSL)
C       
      if (ltxsen(kt) .eq. 2) then   
         txsens(1,kt) = dpt(1,kt)   
      endif 
      if (abs(dx).ge.1.0e-6.and.abs(dy).ge.1.0e-6) then 
C       
C        Truncated LTC adjustment : attenuate terminal adjustments  
C        accordingly.   
C       
         kownta=kownta+1000 
         jtyp = ityp + 10
         dx = (dx+dy)/dx  
         kx = k + ntota 
         mx = m + ntota 

         if (ityp .eq. 3) then
            dpt(1,kx) = dpt(1,kx) * 0.5 * (1.0 + dx)
            dpt(1,mx) = dpt(1,mx) * 0.5 * (1.0 + dx)
         else
            dpt(2,kx) = dpt(2,kx) * 0.5 * (1.0 + dx)
            dpt(2,mx) = dpt(2,mx) * 0.5 * (1.0 + dx)
         endif
      endif 
      call nrtxaj (kt,tnew-told,'NRBKSL')   

C     "JTYP" is temporarily ITYP + 10 which flags an  
C     LTC with a limit adjustment.   

      lt = ltran(10,kt)/100 
      ltran(10,kt) = 100*lt + jtyp
  250 continue  

C     Nodal voltage and angle adjustments  

      do 300 jt = nbslck+1,ntot 
      kt = jt + ntota   
      dt = dpt(1,kt)
      dtt = dt  
      if (abs(dtt).gt.option(10)) then  
         tt = 2.0*option(10) - option(10)**2/abs(dtt)   
         dt = sign(tt,dtt)  
         if (idswa.ne.0) write (dbug,260) jt,dpt(1,kt),dtt,dt   
  260    format (' Truncated Bus angle adjustment ',i5,3e12.5)  
         kownta=kownta+1000 
      endif 
      dv = dpt(2,kt)
      dvv = dv  
      if (abs(dvv).gt.option(11)) then  
         tt = 2.0*option(11) - option(11)**2/abs(dvv)   
         dv = sign(tt,dvv)  

         if (idswa.ne.0) write (dbug,262) jt,dpt(2,kt),dvv,dv   
  262    format (' Truncated Bus voltage adjustment ',i5,3e12.5)
         kownta=kownta+1000 
      endif 

C     Check voltage against Vlimits.  Note that 
C     remote control may be in effect.  

      if (kvolt(jt).gt.0.and.kvolt(jt).ne.jt) then  
         vold=dsqrt(e(jt)**2+f(jt)**2)   
         vnew=vold*(1.0+dv) 
         dx = vnew - vold   
         dy = -dim(vnew,vlimx(jt)) + dim(vlimn(jt),vnew)
         dv = (dx+dy)/vold   
         if (abs(dy).gt.1.0e-6) then
            kownta=kownta+1000  
            kvolt(jt) = jt  

C           Obtain generator index; temporarily disable one iteration.

            i1 = iflag(jt)  
            i2 = iflag(jt+1) - 1
            do 286 i = i1,i2
            if (jflag(1,i) .eq. 8) then 
               j = jflag(2,i)   
               ityp = tbx(7,j) 
               tbx(7,j) = -ityp
               if (kownta .le. 10000 .and. abs (dy) .gt. 0.10) then 
                  write (errbuf(1), 285) intbus(jt), intbas(jt),
     1               dtt, dy
  285             format (' Possible pathological generator control ',
     &                    'scheme ', a8, f6.1, ' d_volt ',f6.3,
     &                    ' truncation ', f6.3)
                  call prterx ('W', 1)
               endif
               go to 288
            endif   
  286       continue
  288       continue
c
c           Truncate adjacent bus voltage adjustment also
c
            if (kmlen(jt) .eq. 1) then
               mt = ikmu(km(jt))
               if (mt .gt. jt) then
                  dpt(2,mt+ntota) = dpt(2,mt+ntota) 
     &                            * 0.5 * (1.0 + (dx + dy)/dx)
               endif
            endif
         endif  
         if (idswb.ne.0) write (dbug,290) jt,kvolt(jt),volt(jt),vold,
     1                                    vnew,dx,dy   
  290    format (' GEN Adjustment ',8x,2i5,10x,5f12.5)  
      endif 
      de=1.0+dv 
      s=dsin(dt)
      c=dcos(dt)
      ek=e(jt)  
      fk=f(jt)  
      e(jt)=de*(ek*c-fk*s)  
      f(jt)=de*(fk*c+ek*s)  
  300 continue  
  900 return
      end   
