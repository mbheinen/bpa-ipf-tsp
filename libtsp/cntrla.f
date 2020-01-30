C    %W% %G%
      subroutine cntrla(itrnfr)
C      
c     This subroutine solves the differential equations
c     for the PSS models and old WSCC exciter models.
c     It calls subroutines TSLSOL and SOLFA thru SOLFL.
c     It is called by CNTRL and processes only one machine per call.
C      
      include 'tspinc/params.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/blkcom2.inc'
      include 'tspinc/cntrl2.inc'
      include 'tspinc/toler.inc'
      include 'tspinc/param.inc'
      include 'tspinc/contrl.inc'
      include 'tspinc/search.inc'
      include 'tspinc/lnk12.inc'
      include 'tspinc/link2.inc'
      include 'tspinc/lnk1a.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/vrgov.inc'
      include 'tspinc/ecsind.inc'
      include 'tspinc/gentbla.inc'
      include 'tspinc/gentblb.inc'
      include 'tspinc/gentblc.inc'
      include 'tspinc/dateq.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/relays.inc'
      include 'tspinc/busvolt.inc'
      include 'tspinc/tsldat.inc'
      include 'tspinc/bname.inc'
      include 'tspinc/buskv.inc'
      include 'tspinc/nameid.inc'
      include 'tspinc/outaux.inc'
      include 'tspinc/machd1.inc'
      include 'tspinc/machd2.inc'
      include 'tspinc/igentn.inc'
      include 'tspinc/citer.inc'
      include 'tspinc/spare1.inc' 
      include 'tspinc/demfix.inc' 

      equivalence (aeps,ar),(aes,brs,af1),(bes,bf1),(ba1s,br),
     &            (baps,a1),(bfs,a2)
      equivalence (ae, vfdo), (aep, bfx), (be, ba1x), (bf1, bapx)
      dimension istem(10)
      equivalence (istem,isorti)
      equivalence(csupp(26),x5o)
      common/vregl/em,vmqt,vmdt
      dimension nregt(21)
      common/prate/ idisw
C 
C     The following common statement is due to the split IN CNTRL and 
c     CNTRLA
C 
      include 'tspinc/comvar.inc'
c
      dimension temps1(195),temps3(900)
      dimension tempn(2, 13)
      common /ckpnt/ dnxmin,d13
      dimension drv(14)
      equivalence (drv,tmpdy)
      equivalence (creg(9), dr), (creg(21), rcex), (creg(23),
     1    xcex), (creg(3), vref), (creg(27), vco), (creg(7), cckp,
     2    ckpr), (creg(28), theta, ckpi), (creg(20), xl), (creg(10),
     3    ckc), (creg(11), v1max), (creg(13), va), (creg(30), vi),
     4    (creg(15), hcb), (creg(16), ckj), (creg(17), db),
     5    (creg(18), dc), (creg(19), vgmax), (creg(1), vrmax),
     6    (creg(2), vrmin), (creg(22), vr), (creg(8), cka), (creg(24),
     7    ckg), (creg(25), da), (creg(26), hba), (creg(4), efdmax),
     8    (creg(5), cki), (creg(29), efd), (creg(6), hbr),
     9    (creg(12), v1min)
      equivalence (pgo,pgor)
      equivalence(sat,rt)
      equivalence(creg(14),vfldo)
      equivalence (sater,dtc),(isg2,istp)
      equivalence(csupp(27),ivcsw)
      dimension ecsn2(2), iecsn(6)
      character*1  id, id1
      character*8 name1, name2
      character ch16*16                                                 !dem
      logical debug,dbghere                                             !dem

      data nregt /7,8,6,7,4,5,5,5,5,7,0,0,0,0,0,0,0,0,0,0,0/
      data twopi / 6.2831853 /

c     begin     begin     begin     begin     begin     begin

      debug = dbghere ('CNTRLA  ')                                      !dem
      if (debug) then                                                   !dem
        call dbgeko ('CNTRLA - at start')                               !dem
        call dbgwrf ('  TO /time/ = ',to)                               !dem
        call dbgwri ('  LPPWR /iteratn cnt/ = ',lppwr)                  !dem
        call dbgwri ('  IDSW /discont stat/ = ',idsw)                   !dem
      endif                                                             !dem
C     
C     START OF EXCITER AND SUPPLEMENTARY SIGNAL LOGIC
C 
 4060 mex=igndta(5,ispf)
      idm = igentc(ispf)
      igbn =igentn(1,ispf)
      iecs = igentn(2,ispf)
C      
C     Get bus name and base kv
C      
      name = bname(igbn)
      bkv = buskv(igbn)
      mexecs=igndta(6,ispf)
      call redecs (creg,mexecs,iex)
      nreg=nregt(mex)
      if (keybrd(3) .ne. 0) then
         write (outbuf, 6050) dtsc, dtsj
         call prtout (1)
 6050    format(1x, 'CNTRL ---DTSC, DTSJ = ', 2e18.8)
      endif
      if (mex.eq.5) go to 4080
      if (mex.eq.8) go to 4080
 4065 if (igndta(7,ispf).ne.0) go to 4100
 4080 msupp=0
      x7=0.0
      x5o=0.0
      x7o = 0.0
      go to 4240
 4100 msupp = igndta(7, ispf)
      msupec = igndta(8, ispf)
      call redecs (csupp,msupec,isup)
      isup1=isup
      call redecs (csupp,msupec,isup1)
c     if (debug) then                                                   !dem
c       call dbgwri ('  JBUSS /bus # for freq input/ = ',JBUSS)         !dem
c       if (jbuss .ne. 0) then                                          !dem
c         igb = jbuss                                                   !dem
c         dtf = edt                                                     !dem
c         eyrn = eyr(igb)                                               !dem
c         eyin = eyi(igb)                                               !dem
c         eysqn = eyrn * eyrn + eyin * eyin                             !dem
c         rb = sqrt (eysqn)                                             !dem
c         call dbgwrf ('  /volt mag @ jbuss/ = ',rb)                    !dem
c       endif                                                           !dem
c       eysqo = eyro * eyro + eyio * eyio                               !dem
c       rb = sqrt (eysqo)                                               !dem
c       call dbgwrf ('  EYO /from csupp[]/ = ',rb)                      !dem
c     endif                                                             !dem
      if (msupp.ne.2 .and. msupp.ne. 5) go to 4240
C      
C     COMPUTE BUS FREQENCY FOR SF TYPE PSS
C      
      igb=jbuss
      dtf=edt
      if (lppwr .eq. 0) dtf = ddt2
      eyrn = eyr(igb)
      eyin = eyi(igb)

c     Calculate old & new voltage magnitudes

      eysqo = eyro * eyro + eyio * eyio                                 !dem
      eysqn = eyrn * eyrn + eyin * eyin                                 !dem
c     EYSQ=EYRN*EYRN+EYIN*EYIN
      if ((lppwr.eq.0) .and. (idsw.eq.3 .or. idsw.eq.5) ) go to 4160
      if (eysqn .ne. 0.0) bfreq = (eyin*eyro-eyrn*eyio) / (eysqn * dtf) !dem
c     if ( EYSQ.NE.0.0) BFREQ=(EYIN*EYRO-EYRN*EYIO) / (EYSQ*DTF)

c     Debug to determine units of frequency

c     if (debug) then                                                   !dem
c       call dbgeko ('CNTRLA - freq input to PSS')                      !dem
c       write (13,'(a,f12.6,a,f12.6)') '   time: ',to,'  bfreq = ',bfreq!dem
c     endif                                                             !dem
 4160 if (lppwr .eq. 0)then
         eyro=eyrn
         eyio=eyin
      endif
      if (keybrd(9) .ne. 0) then
         name1 = bname(igb)
         name2 = bname(igbn)
         write (outbuf,4200) name1,name2
         call prtout (1)
         write(outbuf,4201)bfreq,angp1(ispf),angp2(ispf),eyro,eyio,eyr,
     1                     eyin
         call prtout (1)
 4200    format ('  FREQ. SIG. FROM BUS ',a8,' TO BUS ',a8)
 4201    format (' BUS FREQ,SPDOLD,SPDNEW ',3e15.6,4f8.4)
      endif
 4240 if (lvref.ne.0) go to 4260
      vamgt = vmagt(ispf)
      em=vamgt
c     if (debug) then                                                   !dem
c       call dbgwri ('  ISPF /gen num/ = ',ispf)                        !dem
c       call dbgwrf ('  VMAGT[] /term? volt/ = ',VMAGT(ISPF))           !dem
c     endif                                                             !dem
      if (idm.ne.'H'.and.idm.ne.'L') go to 4280
C      
C     Reactance compensation of terminal voltage for cross compound
c     machines
C      
      vhq = eyii(ispf)
      vhd = eyri(ispf)
      em = em+(0.05/em)*(vhq*oid-vhd*oiq)
      go to 4280
 4260 eyrm = eyr(lvref)
      eyim = eyi(lvref)
      em=sqrt(eyrm*eyrm+eyim*eyim)
 4280 if (msupp.eq.0) go to 4460
C      
C     Start of supplementary signal logic
C      
c     Process voltage input

      if (jbusv .ne. 0) then
        eyrm = eyr(jbusv)
        eyim = eyi(jbusv)
        emm=sqrt(eyrm**2+eyim**2)
c       if (debug) then                                                 !dem
c         call dbgwri ('  JBUSV /bus num for PSS v_in/ = ',JBUSV)       !dem
c         call dbgwrf ('  EMM /volt mag @ jbusv/ = ',EMM)               !dem
c       endif                                                           !dem
      else
         emm=em
      endif
c     if (debug) then                                                   !dem
c       call dbgwrf ('  EMM /volt used for PSS v_in/ = ',EMM)           !dem
c     endif                                                             !dem
      delv=vto-emm

C     Jump if this is the 2nd or later iteration at this time.

      if (lppwr.ne.0) go to 4410
      ivcsw = 0
      delv = vto - vf1
c     if (debug) then                                                   !dem
c       call dbgeko ('  for LPPWR == 0:')                               !dem
c       call dbgwrf ('  VF1 /from csupp[]/ = ',VF1)                     !dem
c     endif                                                             !dem
      x1a=(bqv+ckqv*delv)/aqv
c     if (debug) then                                                   !dem
c       call dbgeko ('CNTRLA - Calculation of PSS voltage transducer')  !dem
c       call dbgwrf ('  TO /time/ = ',TO)                               !dem
c       call dbgwrf ('  VTO /orig term volt/ = ',VTO)                   !dem
c       call dbgwrf ('  VF1 /now term volt/ = ',VF1)                    !dem
c       call dbgwrf ('  -DELV /now - old/ = ',-DELV)                    !dem
c       call dbgwrf ('  X1A /volt xduc out/ = ',X1A)                    !dem
c     endif                                                             !dem

c     Update past values of new temp PSS (inactive)

      if (lppwr .eq. 0)  then                                           !dem
        call ssupdt                                                     !dem
      endif                                                             !dem

c     End of demfix sectn

c     if (debug) then                                                   !dem
c       call dbgeko ('CNTRLA - display of PSS parms')                   !dem
c       call genname (ispf,ch16)                                        !dem
c       call dbgwrc ('  At gen = ',ch16)                                !dem
c       call dbgwrf ('  CKQV /Volt xducer gain/ = ',CKQV)               !dem
c     endif                                                             !dem
C      
C     Shaft slip input
C      
      if (msupp.eq.1) slp=wnow
C      
C     Bus frequency input
C      
      if (msupp.eq.2) slp=bfreq
C      
C     Accelerating power input
C      
      if (msupp.eq.3) slp=pacc(ispf)
      if  ((msupp.eq.3).and.(idsw.eq.3 .or. idsw.eq.5)) slp=paccm1(ispf)
      xefun = 0.0
C      
C     Shaft slip with transient stabilizer
C      
      if (msupp .eq. 4) then
         slp = wnow
         itsl = itsl+1
         tslfir(itsl) = 0.0
         call tslsol(slp)
C      
C        Output of transient stabilizer becomes input to PSS
C      
         xefun = efun(itsl)
      endif
C      
C     Bus frequency with transient stabilizer
C      
      if (msupp .eq. 5)then
         slp = bfreq
         itsl = itsl+1
         tslfir(itsl) = 0.0
         call tslsol(slp)
C      
C        Output of transient stabilizer becomes input to PSS
C      
         xefun = efun(itsl)
      endif
      x1b = (bqs+ckqs*slp)/aqs
      x1 = x1b - x1a + ckqs*xefun
      x1c = ckqs * xefun                                                !dem
c     if (debug) then                                                   !dem
c       call dbgwrf ('  X1B /freq xducr out/ = ',X1B)                   !dem
c       call dbgwrf ('  X1C /teb out/ = ',X1C)                          !dem
c       call dbgwrf ('  X1 /xducr out sum/ = ',X1)                      !dem
c     endif                                                             !dem
      x2=(bq+x1*(atq-1.))/atq
      x3=(bq1+x2*atq1p)/atq1
      if (atq1.eq.1.) x3=x2
      x4=(bq2+x3*atq2p)/atq2
      if (atq2.eq.1.0) x4=x3
C      
C     If we have PSS notch filter, store the old values of X5O
C      
      if (dnf .gt. 0.)then
         call redecs(csupp(34),msupec+isup,9)
         x5oo = x5o
      endif
      x5o=(bq3+x4*atq3p)/atq3
      if (atq3.eq.1.) x5o=x4
      if (keybrd(3) .ne. 0) then
         write (outbuf, 4300)
         call prtout (1)
         write (outbuf, 4301) slp, x1a, x1b, x1, x2, x3, x4, x5o
         call prtout (1)
         write (outbuf, 4301) bqv, bqs, bq, bq1, bq2, bq3, emm
         call prtout (1)
 4300    format ('   SLP,X1A,X1B,X1,X2,X3,X4,X5O,BQV,BQS,BQ,BQ1,BQ2,',
     1            'BQ3,EM   ')
 4301    format (8(2x,e12.5))
      endif
C      
C     Calculate notch filter state variables
C      
      if (csupp(33) .gt. 0.0)then
         x6oo = x6o
         a4s = (1. + yp2 + yp1 ) / ( 1. + xp2 + xp1 )
         x6o = x5o*a4s + bq4/(1.+xp2+xp1)
         x56oo = x56o
         x56o=x56oo+(ddt2/2.)*(bnf*(x5oo+x5o)-dnf*(x6oo+x6o))
         if (keybrd(3) .ne. 0) then
            write (outbuf, 4328) x6o, x5o, bq4
            call prtout (1)
 4328       format('0   notch filter   x6o,x5o,bq4 =', 3e14.5)
         endif
      endif
C      
C     If there has been a step change, modify all time factors
C      
      if (al.eq.0.0) go to 4380
      aqv=aqv*tfac-tfac+1.
      do 4340 k=6,13
 4340 csupp(k)=csupp(k)*tfac-tfac+1.
      cn=atq*atq1*atq2
      denom=1./(cn*atq3)
      cn=atq1p*atq2p*atq3p
      cn5=cn*(atq-1.)
      a2s=(cn5*ckqs*denom)/aqs
      a3s=(cn5*ckqv*denom)/aqv
C      
C     Modify time factors for PSS notch filter
C      
      if (csupp(33) .gt. 0.) then
         xp1 = xp1 / ( tfac * tfac )
         yp1 = yp1 / ( tfac * tfac )
         xp2 = xp2 / tfac
         yp2 = yp2 / tfac
         a4s = (1. + yp2 + yp1 ) / ( 1. + xp2 + xp1 )
         a2s = a2s * a4s
         a3s = a3s * a4s
         if (keybrd(5) .ne. 0) then
            write (outbuf, 4360)
            call prtout (1)
            do 4362 jjj = 1,isup,7
            kkk = min0 (jjj+6,isup)
            write (outbuf, 4361) (k, csupp(k), k=jjj,kkk)
            call prtout (1)
 4362       continue
 4360       format ('0',5x,' SUPPLEMENTAL SIGNAL TABLE ')
 4361       format (1x,7(i3,e15.7))
         endif
      endif
 4380 bqv=(aqv-2.)*x1a+ckqv*delv
      bqs=(aqs-2.)*x1b+ckqs*slp
      bq=x2*(atq-2.)-(atq-1.)*x1
      bq1=x3*(atq1-2.)-x2*(atq1p-2.)
      bq2=x4*(atq2-2.)-x3*(atq2p-2.)
      bq3=x5o*(atq3-2.)-x4*(atq3p-2.)
C      
C     Bypass limiting of X5O if PSS notch filter exists
C      
      if (csupp(33) .le. 0.0)then
         if ((abs(delv).gt.vc).and.(vc.ne.0.0)) x5o=0.0
         if (x5o.gt.vslim) x5o=vslim
         if (x5o.lt.-vslim) x5o=-vslim
C      
C        Keep old value of X7 for output purposes
C      
         x7o = amin1(vslim,amax1(-vslim,vf2))
      endif
      if (atq3.eq.1.) bq3=0.0
      if (atq2.eq.1.) bq2=0.0
      if (atq1.eq.1.) bq1=0.0
      cn=atq*atq1*atq2
      denom=1./(cn*atq3)
      cn1=cn*bq3
      cn=atq3p*atq
      cn2=cn*atq1*bq2
      cn3=cn*atq2p*bq1
      cn=atq1p*atq2p*atq3p
      cn4=cn*(bq+(atq-1.)*(bqs/aqs-bqv/aqv))
      a1s=(cn1+cn2+cn3+cn4)*denom
C      
C     Process PSS notch filter if it exists
C      
      if (csupp(33) .gt. 0.0)then
         if ((abs(delv).gt.vc).and.(vc.ne.0.0)) x6o = 0.0
         if (x6o.gt. vslim) x6o =  vslim
         if (x6o.lt.-vslim) x6o = -vslim
         bq4 = x6o * (1.-xp2-xp1) - x5o * (1.-yp2-yp1) + edt * x56o
         a1s = a1s * a4s + bq4/(1.+xp2+xp1)
      endif
 4410 if (msupp .eq. 1) slp = wnew
      if (msupp.eq.2) slp=bfreq
      if (msupp.eq.3) slp=paccex(ispf)
      xefun = 0.0
C      
C     Shaft slip with transient stabilizer
C      
      if (msupp .eq. 4)then
         slp = wnew
         if (lppwr .ne. 0)itsl = itsl+1
         tslfir(itsl) = 1.0
         call tslsol(slp)
C      
C        Output of transient stabilizer becomes input to PSS
C      
         xefun =  efun(itsl)
      endif
C      
C     Bus frequency with transient stabilizer
C      
      if (msupp .eq. 5)then
         slp = bfreq
         if (lppwr .ne. 0)itsl = itsl+1
         tslfir(itsl) = 1.0
         call tslsol(slp)
C      
C        Output of transient stabilizer becomes input to PSS
C      
         xefun = efun(itsl)
      endif
      if (ivcsw .ge. 3) then
         x7 = 0.0
         x7o = 0.0
         go to 4460
      endif
      vf1 = emm
c *** csw addition 10/93
      delv = vto - vf1
c *** csw end addition
      a5s = a2s*aqs
      x7 = a1s+a2s*slp-a3s*delv+a5s*xefun
      vf2 = x7

c     Calculate new values for new temp PSS (inactive)

      call sssolv (slp, em, edt/frqbse, x7)

c     vf2 = x7                                                          !dem
c     if (debug) then                                                   !dem
c       call debgeko ('CNTRLA - after calcing demfix vars')             !dem
c       write (13,'(a,f10.4,a,f10.6)')                                  !dem
c    +     '   time: ',to,'  V_ps: ',x7                                 !dem
c     endif
c     -  End of demfix sectn
      if (keybrd(3) .ne. 0) then
         write (outbuf, 4420)
         call prtout (1)
         write (outbuf, 4421) slp, delv, a1s, a2s, a3s, x7, bqv, bqs
         call prtout (1)
         write (outbuf, 4421) bq, bq1, bq2, bq3
         call prtout (1)
 4420    format ('   SLP,DELV,A1S,A2S,A3S,X7,BQV,BQS,BQ,BQ1,BQ2,BQ3  ')
 4421    format (8(2x,e12.5))
      endif
 4440 if (x7.gt.vslim) x7=vslim
      if (x7.lt.-vslim) x7=-vslim
      if (abs(delv) .gt. vc .and. vc .ne. 0.0) then
         x7 = 0.0
         ivcsw = ivcsw + 1
      endif
C      
C     Start of exciter logic
C      
 4460 if (mgen .eq. 9) go to 7000
C      
C     Jump to SN 8000 for IEEE exciter models
C      
      if (mex .gt. 10) go to 8000
      if (lppwr .ne. 0) then
         if (mex.eq.5) go to 4500
         go to 5620
      endif
      cvt1=cvt2
      cvt2=em
      efdo=vfd
      if (mex.eq.9) efdo=cvt1*amax1(efdmin,amin1(vfd,efdmax))
      if (mex .eq. 3) go to 5861
      if (mex.eq.10) efdo=amax1(efdmn,vfd)
 4500 go to (4520,4520,4540,4520,4540,4540,4540,4540,4520,4540), mex
 4520 x1=(br+em)/ar
      if (idsw.eq.3 .or. idsw.eq.5) x1=(br+cvt1) / ar
      if (lppwr.ne.0) go to 4560
      br=x1*(ar-2.)+cvt2
      if (ar .eq. 1.) br = 0.0
      go to 4560
 4540 x1=cvt2
 4560 go to(4580,4580,4580,4680,5620,4600,4660,4620,4640,4580),mex
 4580 x4=aep*vfd-be
      x3=aa1*x4-ba1
      if (mex.eq.2) go to 4700
      go to 4680
 4600 x3=aep*efdo-be
      go to 4680
 4620 x3=efdo/cke1
      go to 4720
 4640 x4=vfd
      x3 = aa1 * x4 - ba1
      x6=(bf+akf*x4)/atf
      go to 4760
 4660 x3=aa1*efdo-ba1
      x4 = efdo
 4680 x6=(bf+akf*efdo)/atf
      if (mex.eq.3) x6=(bf+akf*efdo*(cke+aep-ae))/atf
      if (mex .eq. 6 )  x6 = (bf + akf * efdo * (cke +aep-ae))/atf
      go to 4760
 4700 x5=(x4+bf1)/af1
      x6=(akf*x5+bf)/atf
      go to 4780
 4720 x5=(bf1+ckf1*efdo)/af1
 4740 x6=(bf+akf*x3)/atf
 4760 go to(4780,4780,4780,5760,4780,4780,4810,4810,4810,4780),mex
 4780 ck1=0.0
      ck2=0.0
      ck1 = csatx*exp(esatx*abs(vfd))*(1. + vfd*esatx)
      ck2 = vfd*vfd*esatx*csatx*exp(esatx*abs(vfd))
 4800 aep=ae+ck1
 4810 if (keybrd(3) .ne. 0) then
          write (outbuf, 4820)
 4820     format ('   X1,X3,X4,X6,X5O,AE,AEP,CK1,CK2,EFDO,BE,BA1,BF ')
          call prtout (1)
          write (outbuf, 4821) x1, x3, x4, x6, x5o, ae, aep, ck1
 4821     format (8(2x,e12.5))
          call prtout (1)
          write (outbuf, 4821) ck2, efdo, be, ba1, bf
          call prtout (1)
      endif
      if (al.eq.0.0) go to 4980
      go to (4860,4860,4880,4860,4900,4880,4880,4880,4860,4880),mex
 4860 ar=ar*tfac-tfac+1.
      br=x1*(ar-2.)+cvt2
 4880 atf=atf*tfac-tfac+1.
      if (ar .eq. 1.) br = 0.0
      akf = akf*tfac
      if (mex.eq.2) af1=af1*tfac-tfac+1.
      if (mex .eq. 8) af1=af1*tfac-tfac+1.
      if (mex.eq.6) go to 4920
      aa1=aa1*tfac-tfac+1.
 4900 aa=aa*tfac-tfac+1.
      if (mex .gt. 6 .and. mex .ne. 10) go to 4940
 4920 ae=ae*tfac-cke*tfac+cke
      aep=ae+ck1
      if (mex.ne.6) go to 4940
      aa=aa-ddt2/2.+edt/2.
 4940 if (keybrd(5) .ne. 0) then
         write (outbuf, 4960)
 4960    format ('0',5x,' EXCITER TABLE ')
         call prtout (1)
         do 4962 jjj = 1,iex,7
            kkk = min0 (jjj+6,iex)
            write (outbuf, 4961) (k, creg(k), k=jjj,kkk)
 4961       format (1x,7(i3,e15.7))
            call prtout (1)
 4962    continue
      endif
 4980 go to (5000,5000,5000,5000,5020,5020,5060,5040,5060,5000), mex
 5000 be=vfd*(ae-ck1-2.*cke)+x4+2.*ck2
      ba1=x4*(aa1-2.)+x3
      go to 5060
 5020 be=efdo*(ae-ck1-2.*cke)+x3+2.*ck2
      if (mex.eq.5) go to 5050
      bap=(edt*cka/2.)*(2.*vref-em-x6+x5o)+creg(14)*(em+x6-x5o)+x3
      go to 5100
 5040 bap=x3*(aa-2.)+cka*(2.*vref-x6-x5-x1)
      go to 5080
 5050 if (cka.gt.10.) bap=x3p*(aa-2.)+cka*(2.*vref-x1)
      if (cka.lt.10.) bap=x3p*(2./edt)+cka*(2.*vref-x1)
      go to 5080
 5060 bap=x3*(aa-2.)+cka*(2.*vref-x6-x1+x5o)
      ba1 = x4 * ( aa1 - 2.) + x3
      go to (5080,5080,5080,5360,5080,5100,5080,5080,5080,5080), mex
 5080 ckap=cka
      if (mex.eq.5) go to 5090
      slope=ckap*(vref-x6-x1+x5o)-x3
      if (mex.eq.8) slope=slope-ckap*x5
      go to 5120
 5090 if (cka.gt.10.) slope=ckap*(vref-x1)-x3p
      if (cka.lt.10.) slope=ckap*(vref-x1)
      go to 5120
 5100 slopn=vref-x6-em
      ta=aa-edt/2.
      slope=slopn+(ta/edt)*(slopn-slopo)
      slopo=slopn
 5120 go to(5140,5140,5140,5360,5160,5160,5180,5200,5180,5140),mex
 5140 vfdmax=(be+(ba1+vrmax)/aa1)/aep
      vfdmin=(be+(ba1+vrmin)/aa1)/aep
      go to 5240
 5160 vfdmax=(be+vrmax)/aep
      vfdmin=(be+vrmin)/aep
      if (mex.ne.6) go to 5240
      aa=ta+edt/2.
      aap=aa*cka
      go to 5240
 5180 vfdmax=(ba1+vrmax)/aa1
      vfdmin=(ba1+vrmin)/aa1
      go to 5240
 5200 vfdmax=cke1*vrmax
      vfdmin=cke1*vrmin
 5240 go to(5260,5320,5340,5360,5360,5340,5260,5280,5350,5260),mex
 5260 bf=(atf-2.)*x6-akf*efdo
      go to 5360
 5280 bf1=(af1-2.)*x5+efdo*ckf1
      bf=(atf-2.)*x6-akf*x3
      go to 5360
 5320 bf1=(af1-2.)*x5+x4
      bf=(atf-2.)*x6-akf*x5
      go to 5360
 5340 bf=(atf-2.)*x6-akf*efdo*(ck1+cke)
      go to 5360
 5350 bf=(atf-2.)*x6-akf*x4
 5360 maxsw=0
      go to 5420

c     -  no path to this code

c5370 if ((X3 .GT. VRMAX-.001) .AND. (SLOPE .GT. 0.0)) MAXSW =1
c     IF ((X3.LT.VRMIN+.001).AND.(SLOPE.LT.0.0)) MAXSW=-1
c     IF (MAXSW) 5380,5420,5400
c5380 CKAP=0.0
c     BAP=VRMIN*AA
c     IF (MEX.NE.6) GO TO 5420
c     AAP=0.
c     BAP=VRMIN
c     GO TO 5420
c5400 CKAP=0.0
c     BAP=VRMAX*AA
c     IF (MEX.NE.6) GO TO 5420
c     AAP=0.
c     BAP=VRMAX

 5420 go to (5440,5460,5480,5500,5520,5540,5600,5560,5580,5440), mex
 5440 denom=1./(aa1*aep*aa+(ckap*akf)/atf)
      if ( mex.eq.10) denom = 1./ ( aa*aa1*aep)
      a1=-(ckap/ar)*denom
      a2=(aa1*aa*be+aa*ba1+bap-ckap*(br/ar+bf/atf))*denom
      go to 5620
 5460 denom=1./(aep*aa1*aa+(ckap*akf*aep)/(af1*atf))
      a1=-(ckap/ar)*denom
      a2=(aa1*aa*be+ba1*aa+bap-ckap*(br/ar-akf*(be-bf1)/(af1*atf)+bf/atf
     1))*denom
      go to 5620
 5480 denom=1./(aa1*aa*aep+ckap*akf*(cke+ck1)/atf)
      a1=-ckap*denom
      a2=(be*aa1*aa+ba1*aa+bap-(ckap*bf)/atf)*denom
      ar=1.
      go to 5620
 5500 denom=1./(ae*aa1*aa+ckap*akf/atf)
      a1=(-ckap/ar)*denom
      a2=(aa1*aa*be+ba1*aa+bap-ckap*(br/ar+bf/atf))*denom
      a3p= aa1*aa*denom
      vfd = a1*em+a2 +a3p*vbp -a1*ar*x7
      go to 5660
 5520 vfd=(be+x3)/aep
      go to 5660
 5540 denom=1./(aep+aap*akf*(cke+ck1)/atf)
      a1=-aap*denom
      a2=(be+bap-bf*aap/atf)*denom
      go to 5620
 5560 denom=(aa+ckap*(akf/atf+cke1*ckf1/af1))/cke1
      a1=-ckap/denom
      a2=(bap-ckap*(bf/atf+bf1/af1))/denom
      ar=1.
      go to 5620
 5580 denom=1./(aa*aa1+ckap*akf/atf)
      a1 = -(ckap/ar)*denom
      a2 = (aa*ba1 + bap-ckap*(br/ar+bf/atf))*denom
      go to 5620
 5600 denom=1./(aa*aa1+(ckap*akf)/atf)
      a1=-ckap*denom
      a2=(aa*ba1+bap-(ckap*bf)/atf)*denom
      ar=1.
 5620 emp = em
      em = .5*em + .5*vo
      vo = emp
      if (lppwr .eq. 0) em = emp
      if (mex.eq.3) go to 5861
      if (mex.eq.4) go to 5780
      if (mex.eq.5) go to 5820
 5630 vfd = a1*em + a2-a1*ar*x7
      if (mex.lt.10) go to 5635
      vfldn=0.5*vfldtn(1,ispf)+0.5*vfldo
      if (lppwr.eq.0) vfldn=vfldtn(1,ispf)
      vfd=vfd-vfldn*(ckap*(akf/atf)/(aa*aa1*aep))
 5635 if (keybrd(3) .ne. 0) then
         write (outbuf, 5640)
         call prtout (1)
         write (outbuf, 5641) vfd, vfdmax, vfdmin, em, x7, a1, a2,
     1                        denom
         call prtout (1)
         write (outbuf, 5641) bap, ckap, be, ba1, bf, slope
         call prtout (1)
 5640    format ('0  VFD,VFDMAX,VFDMIN,EM,X7,A1,A2,DENOM,BAP,CKAP,BE,',
     1         'BA1,BF,SLOPE, ')
 5641    format (8(2x,e12.5))
         if (lppwr .eq. 0) then
             write (outbuf, 5650)
             call prtout (1)
             write (outbuf, 5651) x1, x3, x4, x6, x5o, ae, aep, ck1
             call prtout (1)
             write (outbuf, 5651) ck2, efdo, be, ba1, bf
             call prtout (1)
 5650        format('0  X1,X3,X4,X6,X5O,AE,AEP,CK1,CK2,EFDO,BE,BA1,',
     1              'BF ')
 5651        format(8(2x,e12.5))
         endif
      endif
 5660 if (vfd.gt.vfdmax) vfd=vfdmax
      if (vfd.lt.vfdmin) vfd=vfdmin
      if (mex.eq.8) go to 5680
      if (mex.eq.9) go to 5700
      vfdp=amax1(efdmn,vfd)
      go to 5720
 5680 efdmxt=efdmax*em
      efdmnt=efdmin*em
      vfdp = vfd
      if (vfdp.gt.efdmxt) vfdp = efdmxt
      if (vfdp.lt.efdmnt) vfdp = efdmnt
      go to 5720
 5700 vfdp=vfd
      if (vfdp.gt.efdmax) vfdp = efdmax
      if (vfdp.lt.efdmin) vfdp = efdmin
      vfdp=vfdp*em
 5720 if (mgen .gt. 5) go to 5730
      fq(ispf) = fq(ispf) 
     &         + (edt / (2.0*ad+edt*(1.0+satd)))*(vfdp-vfldtn(1,ispf))
      go to 5735
C      
C     MGEN .gt. 5 for generators with damper windings
C      
 5730 vfl=vfldtn(1,ispf)
      fq(ispf) = fq(ispf) + adxdr * (vfdp - vfl )
      if (mex .eq .10) vfldo = vfl
      vfldtn(1,ispf)=vfdp
      go to 5740
 5735 if (mex .eq. 10) vfldo = vfldtn(1,ispf)
      vfldtn(1,ispf)=vfdp
C      
C     Add regulator voltage to output table
C      
 5740 call target  

c     Most exciters calclulate X3 <- V_reg only on first pass.

      if (lppwr .eq. 0) regout(ispf) = x3

c     REGOUT(ISPF) = X3

      if ((lppwr .eq. 0) .and. debug) then                              !dem
        call dbgeko ('CNTRLA - Storing gen v_reg output in REGOUT[]')   !dem
        call dbgwri ('  ISPF     /gen num/ = ',ispf)                    !dem
        call dbgwrf ('  X3   /v_reg value/ = ',x3)                      !dem
      endif                                                             !dem
C      
C     IF A TRANSIENT STABILIZER IS PRESENT REGOUT IS EFUN
C      
      if (msupp.eq.4 .or. msupp.eq. 5)then
         regout(ispf) = efun(itsl)
      endif
C      
C     Add supplemental signal to output table
C      
      supout(ispf) = x7
      go to 6200
 5760 x3=(bap/aa)-(ckap/aa)*(x1+x6-x5o)
      if (x3.gt.vrmax) x3=vrmax
      if (x3.lt.vrmin) x3=vrmin
      x4=(ba1+x3)/aa1
 5780 vhq = eyii(ispf)
      veq = vhq+ra*oiq+((xd-xp)/(1.+satd)+xp)*oid
      vhd = eyri(ispf)
      if (xt.gt.0.) go to 5786
      vtd = vhd
      vtq = vhq
      go to 5792
 5786 vtd = vhd + rt * oid - xt * oiq
      vtq = vhq + rt * oiq + xt * oid
 5792 vthd = ckp * vtd - cki * oiq
      vthq = ckp * vtq + cki * oid
      vthevx = sqrt(vthd * vthd + vthq * vthq)
      xlifd=0.78*xl*veq
      vbp=0.0
      vbpsq=vthevx*vthevx-xlifd*xlifd
      if (vbpsq.lt.0. ) go to 5800
      vbp=sqrt(vbpsq)
 5800 if (lppwr.ne.0) go to 5500
      vb=vbp+x4
      if (vb.gt.vbmax) vb=vbmax
      if (vb.lt.0.0) vb=0.0
      if ( xlifd.gt.vthevx) vb= 0.0
      bf=x6*(atf-2.)-akf*efdo
      ba1=x4*(aa1-2.)+x3
      be=efdo*(ae-2.*cke)+vb
      ckap=cka
      bap=x3*(aa-2.)+ckap*(2.*vref-x6-x1+x5o)
      vfdmax=(be+vbmax)/ae
      vfdmin=be/ae
      go to 5500
 5820 if (cka.gt.10.) x3p=(bap-cka*em)/aa
      if (cka.lt.10.) x3p=(bap-cka*em)*(edt/2.)
      if (x3p.gt.vrmax) x3p=vrmax
      if (x3p .lt.vrmin) x3p=vrmin
      x3=x3p
      dvt=vbias-em
      if (dvt.ge.ckv) x3=vrmax
      if (dvt.le.-ckv) x3=vrmin
 5860 if (lppwr.ne.0) go to 5520
      go to 4780

c     -  no path to this code
c5851 EMP = EM
c     EM = .5*EM + .5*VO
c     VO = EMP
c     if (LPPWR .EQ. 0) EM = EMP
c     SLP1 = (EM-CVT1)/EDT
c     SLP2 = (X7-X5O)/EDT
c     if (LPPWR .NE. 0) GO TO 5852
c     BR = BRS
c     BAP = BAPS
c     BF = BFS
c5852 X1 = (BR +CVT1)/AR
c     X7A = X5O
c     CVT = CVT1
c     X3 = EFDO
c     X6 = (BF + AKF*X3)/ATF
c     VFD = EFDO
c     IED = EDT*10.
c     DENOM = AA + CKA*AKF/ATF
c     SA1 = -(CKA/AR)/DENOM
c     DO 5855 IN=1,IED
c     TDT = .1*IN
c     BAPS = X3*(AA-2.) + CKA*(2.*VREF-X6-X1 + X7A)
c     BRS = X1*(AR-2.) + CVT
c     BFS = (ATF-2.)*X6 - AKF*X3
c     CVT = TDT*SLP1 +CVT1
c     X7A = TDT*SLP2 + X5O
c     SA2 = (BAPS - CKA*(BRS/AR+ BFS/ATF))/DENOM
c     VFD = SA1*(CVT -AR*X7A) + SA2
c     if (VFD .GT. VRMAX) VFD = VRMAX
c     if (VFD .LT. VRMIN) VFD = VRMIN
c     X3 = VFD
c     X1 = (BRS + CVT)/AR
c     X6 = (BFS + AKF*X3)/ATF
c5855 CONTINUE
c     GO TO 5700

 5861 slp1 = (em-cvt1)/edt
      slp2 = (x7-x5o)/edt
      if (lppwr .ne. 0) go to 5862
      aep = aeps
      ae = aes
      be = bes
      bap = baps
      ba1 = ba1s
      bf = bfs
      if (dtsc .lt. 1.0) go to 5862
      aa1s = aa1
      akfs = akf
      aess = ae
      atfs = atf
 5862 x1 = cvt1
      x7a = x5o
      x4 = aep*efdo - be
      x3 = aa1*x4-ba1
      x6 = (bf+akf*efdo* (cke + aep-ae))/atf
      if (dtsc .lt. 1.0) go to 5863
      x3 = aa1s*x4 - ba1
      x6 = (bf + akfs*efdo*(cke + aep - aess))/atfs
5863  ck1 = 0.0
      ck2 = 0.0
      ck1 = csatx*exp(esatx*abs(efdo))* (1.+efdo*esatx)
      ck2 = (efdo**2)*esatx*csatx*exp(esatx*abs(efdo))
      vfd = efdo
      if (dtsc .lt. 1.0) go to 5868
      if (lppwr .ne. 0 .or. al .eq. 0.0) go to 5865
C 
C 
C     Change coeff due to EDT change
C 
      aa1s = aa1
      akfs = akf
      aess = ae
      atfs = atf
      ae = ae*tfac - tfac*cke + cke
      aes = ae
      aa1 = aa1*tfac - tfac + 1.0
      aa = aa*tfac - tfac + 1.0
      atf = atf*tfac - tfac + 1.0
      akf = akf*tfac
 5865 continue
      ied = dtsc
      dts1 = edt/dtsc
      go to 5869
5868  ied = edt*10.
      dts1 = 0.1
5869  do 5875 in=1,ied
      tdt = in*dts1
      bes = vfd*(ae-ck1-2.*cke)+x4 +2.*ck2
      ba1s = x4*(aa1-2.) + x3
      baps = x3*(aa-2.) + cka*(2.*vref -x6 -x1 + x7a)
      bfs = (atf-2.)*x6 - akf*vfd*(ck1+cke)
      aeps = aes + ck1
      vfdmax = (bes + (ba1s + vrmax)/aa1)/aeps
      vfdmin = (bes + (ba1s + vrmin)/aa1)/aeps
      x1 = tdt*slp1+cvt1
      x7a= tdt*slp2 + x5o
      denom = 1./(aa1*aa*aeps + cka*akf*(cke+ck1)/atf)
      sa1 = -cka * denom
      sa2 = (bes*aa1*aa + ba1s*aa+baps -(cka*bfs)/atf)*denom
      vfd = sa1*(x1-x7a)+ sa2
      if (vfd .gt. vfdmax) vfd = vfdmax
      if (vfd .lt. vfdmin) vfd = vfdmin
      vfdp = amax1(efdmn,vfd)
      x4 = aeps*vfd-bes
      x3 = aa1*x4 -ba1s
      x6 = (bfs+akf*vfd*(cke+aeps-aes))/atf
      ck1=0.0
      ck2=0.0
      ck1 = csatx*exp(esatx*abs(vfd))*(1.+vfd*esatx)
      ck2 = (vfd**2)*esatx*csatx*exp(esatx*abs(vfd))
 5875 continue
      if (keybrd(3) .ne. 0) then
         write (outbuf, 5878)
         call prtout (1)
         write (outbuf, 5879) x1, x3, x4, x6, vfd, x7a
         call prtout (1)
 5878    format('0',5x,'X1,X3,X4,X6,VFD,X7A')
 5879    format(2x,6e12.5)
      endif
      go to 5660
C 
C     Process new IEEE exciter models
C
 8000 vhq = eyii(ispf)
      vhd = eyri(ispf)
C 
C     Calculate transformer voltages
C 
      if (xt .eq. 0.) then
          vtd = vhd
          vtq = vhq
      else
          vtd = vhd + rt*oid - xt*oiq
          vtq = vhq + rt*oiq + xt*oid
      endif
C 
C        Model FA
C 
      if (mex .eq. 11)then
        call solfa(citer(1,ispf))
        vfdp = citer(29,ispf)
        if (debug) then                                                 !dem
          call dbgeko ('CNTRLA - just after call to SOLFA')             !dem
          call dbgwrf ('  X3 /v_reg value/ = ',x3)                      !dem
        endif                                                           !dem
        go to 5720
C 
C        Model FB
C 
      else if (mex .eq. 12)then
         call solfb(citer(1,ispf))
         vfdp = citer(29,ispf)
         go to 5720
C 
C        Model FC
C 
      else if (mex .eq. 13)then
         call solfc(citer(1,ispf))
         vfdp = citer(29,ispf)
         go to 5720
C 
C        Model FD
C 
      else if (mex .eq. 14)then
         call solfd(citer(1,ispf))
         vfdp = citer(29,ispf)
         go to 5720
C 
C        Model FE
C 
      else if (mex .eq. 15)then
         call solfe(citer(1,ispf))
         vfdp = citer(29,ispf)
         go to 5720
C 
C        Model FF
C 
      else if (mex .eq. 16)then
         call solff(citer(1,ispf))
         vfdp = citer(29,ispf)
         go to 5720
C 
C        Model FG
C 
      else if (mex .eq. 17)then
         call solfg(citer(1,ispf))
         vfdp = citer(29,ispf)
         go to 5720
C 
C        Model FH
C 
      else if (mex .eq. 18)then
         call solfh(citer(1,ispf))
         vfdp = citer(29,ispf)
         go to 5720
C 
C        Model FJ
C 
      else if (mex .eq. 19)then
         call solfj(citer(1,ispf))
         vfdp = citer(29,ispf)
         go to 5720
C 
C        Model FK
C 
      else if (mex .eq. 20)then
         call solfk(citer(1,ispf))
         vfdp = citer(29,ispf)
         go to 5720
C 
C        Model FL
C 
      else if (mex .eq. 21)then
         call solfl(citer(1,ispf))
         vfdp = citer(29,ispf)
         go to 5720
      else
         return
      endif

 6200 itrnfr = 1
      go to 7010
 7000 itrnfr = 2
 7010 return
      end
