C    @(#)cutlis.f	20.4 11/12/98
        subroutine cutlis (idummy)
 
C       Routine formats an input listing of the
C       powerflow data
 
        include 'ipfinc/parametr.inc'
c		
        include 'ipfinc/bus.inc'
        include 'ipfinc/cbus.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/xdata.inc'
        include 'ipfinc/pqcurves.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/com007.inc'
        include 'ipfinc/bstype.inc'
        include 'ipfinc/brtype.inc'

        character  cntrlb*8, mo*1, type*1, angle*6, id*1, buscnt*8, 
     &             jown*3, cc*1, pqstatus*8, t1c*7, t2c*7, trtyp*1
        integer ptr, q
        logical found

C       ****************************************************************
        entry lisbus (nb)
        ktype = kbsdta(1,nb)
        qmax = busdta(9,nb)
        qmin = busdta(10,nb)
        call typno(type,ktype)
        if (ktype.eq.8.or.ktype.eq.11) then
           kb=kbsdta(13,nb)
           if (kb.ne.0) then
              buscnt=bus(kb)
              bascnt=base(kb)
           else
              buscnt = '        '
              bascnt = 0.0
           endif
          if (ktype .eq. 8) then
             pct = busdta(14,nb)
          else
             pct = 0.0
          endif
          vmax = busdta(11,nb)
          vmin = busdta(12,nb)
          ipct = pct
          write (outbuf,690) type,owner(nb),bus(nb),base(nb),zone(nb),
     &       (busdta(k,nb),k=3,8),qmax,qmin,vmax,vmin,buscnt,bascnt,
     &       ipct
  690     format('0B',a1,1x,a3,1x,a8,f6.1,1x,a2,f8.1,'PL',f7.1,'QL',
     &       f6.1,'PS',f6.1,'QS',f6.1,'PM',f7.1,'PG',f7.1,'QH',f7.1,
     &       'QL',f6.3,'VH',f6.3,'VL ',a8,f6.1,i4,'%')
 
          call prtout(1)
          go to 870
 
        else if (ktype.eq.5) then
C
C          "BD" BUS
C
           k1=kbsdta(9,nb)
           write (outbuf,700) type,owner(nb),bus(nb),base(nb),
     1        zone(nb),(busdta(k,nb),k=3,8),bus(k1),base(k1)
  700      format('0B',a1,1x,a3,1x,a8,f6.1,1x,a2,f3.0,' BRIDGES',f7.1,
     &        ' MH',f7.1,' AMIN',f7.1,' ASTOP',f7.1,' VALVE DROP',f7.1,
     &        ' IMAX ',a8,f6.1,' COM BUS'  )
           call prtout(1)
           go to 870
 
        else if (ktype.eq.12) then
C
C          "BM" BUS
C
           k1 = kbsdta(9,nb)
           bn = busdta(3,nb)
           bd = busdta(7,nb)
           amin = busdta(5,nb)
           amax = busdta(6,nb)
           gnom = busdta(10,nb)
           gmin = busdta(11,nb)
           pd = busdta(13,nb)
           pv = busdta(14,nb)
           if (k1 .gt. 0) then
              buscnt = bus(k1)
              bascnt = base(k1)
           else
              buscnt = '         '
              bascnt = 0.0
           endif
           write (outbuf,700) type,owner(nb),bus(nb),base(nb),
     1        zone(nb),(busdta(k,nb),k=3,8),buscnt,bascnt
           call prtout(1)
           write (outbuf,820) gnom,gmin,pd,pv,busdta(7,nb)
  820      format(46x, f7.1,' GNOM ',f7.1,' GMIN ',f9.1,' PDC ',f9.1,
     1        ' VDC ',a1,' CONVERTER TYPE ')
           call prtout(1)
           go to 870
 
        else if (ktype.eq.3) then
           write (angle,750) busdta(12,nb)
  750      format(f6.1)
        else
           write (angle,730) busdta(12,nb)
  730      format(f6.3)
        endif
        write (outbuf,770) type,owner(nb),bus(nb),base(nb),zone(nb),
     &     (busdta(k,nb),k=3,8),qmax,qmin,busdta(11,nb),angle
  770   format('0B',a1,1x,a3,1x,a8,f6.1,1x,a2,f8.1,'PL',f7.1,'QL',
     1     f6.1,'PS',f6.1,'QS',f6.1,'PM',f7.1,'PG',f7.1,'QH',f7.1,
     2     'QL',f6.3,'VH',a6,'VL ',a8,f6.1,f5.0,'%')
        call prtout(1)
  870   continue
        return
 
        entry liscbs (ncb)
 
        k1=kbctbl(1,ncb)
        kdin=kbctbl(7,ncb)
        kyr=kdin/100
        mon = mod(kdin,100) + 1
        mo = month(mon)
        write (outbuf,920) (bctbl(k,ncb),k=8,10),(bctbl(k,ncb),k=2,6),
     1     bctbl(11,ncb),mo,kyr
920     format(5x,'+',a1,a3,1x,a3,10x,2(f9.1),2(f8.1),10x,f7.1,2x,f7.1,
     1     43x,a1,i2,'IN'   )
        call prtout(1)
        return
 
        entry lisxdt (lmx)
 
        nbx = xdata(1,lmx)
        k1 = xdata(2,lmx)
        if (k1.eq.0) k1 = nbx
        write (outbuf,960) bus(k1), base(k1), (xdata(k,lmx),k=7,22)
  960   format(7x, 'X', 9x, a8, f6.1, 8(f2.0, 'N', f7.1, ' Q') )
        call prtout(1)
        return
 
c       Print the P/Q curve data

        entry lispqd(lpq)
        pqstatus = 'Active  '
        if (.not. pqactive(lpq)) pqstatus = 'Inactive'

        write (outbuf,970) pqid(lpq), pqstatus, pqnumunit(lpq), 
     &    (pqpgen(i,lpq),i=-1,7)
  970   format(t5, 'QP Pgen ', a2, 1x, a8, 1x, i2, 'UN', f8.2, 
     &     'MVA', f8.2, 'PMx', f8.2, 'P01', f8.2, 'P02', f8.2, 'P03', 
     &     f8.2, 'P04', f8.2, 'P05', f8.2, 'P06', f8.2, 'P07')
        call prtout(1)
        write (outbuf,971) (pqqmax(i,lpq),i=-1,7)
  971   format(t5, 'QX Qmax      ', 11x, f8.3, '+PF', 9(f8.2, 3x) )
        call prtout(1)
        write (outbuf,972) (pqqmin(i,lpq),i=-1,7)
  972   format(t5, 'QN Qmin      ', 11x, f8.3, '-PF', 9(f8.2, 3x) )
        call prtout(1)
        found = .false.
        do i = 8, 15
          if (pqpgen(i,lpq) .ne. 0.0 .or.
     &        pqqmax(i,lpq) .ne. 0.0 .or.
     &        pqqmin(i,lpq) .ne. 0.0) found = .true.
        enddo
        if (found) then
           write (outbuf,980) (pqpgen(i,lpq),i=8,15)
  980      format(t5, 'QP Pgen', t30, f8.2, 'P08', f8.2, 'P09', f8.2, 
     &       'P10', f8.2, 'P11', f8.2, 'P12', f8.2, 'P13', f8.2, 'P14', 
     &       f8.2, 'P15')
           call prtout(1)
           write (outbuf,981) (pqqmax(i,lpq),i=8,15)
  981      format(t5,'QX ', t30, 8(f8.2, 3x) )
           call prtout(1)
           write (outbuf,982) (pqqmin(i,lpq),i=8,15)
  982      format(t5,'QN ', t30, 8(f8.2, 3x) )
           call prtout(1)
        endif

        return

        entry lisbrn (ptr)
 
        ltype = brtype(ptr)
        k1 = kx(ptr)
        k2 = ky(ptr)
        id = brid(ptr)
        nsect=brsect(ptr)
        q  = brnch_ptr(ptr)
        nbr = iabs(q)
        call getchr(3,jown,kbrnch(3,nbr))
        intovr = kbrnch(15,nbr)

c       Transpose interchange override if needed

        if (q .lt. 0 .and. intovr .ne. 0 ) intovr = 3 - intovr
        rating = brnch(4,nbr)
        alines = brnch(16,nbr)
        lines=alines
        kdin = kbrnch(11,nbr)
        if(ltype .eq. BRTYP_PEQ) kdin=0
        kyr=kdin/100
        mon = mod(kdin,100) + 1
        mo = month(mon)

        if (ltype .eq. BRTYP_L) then
c                 "L " branch
           irate1 = rateln(1,nbr)
           irate2 = rateln(2,nbr)
           irate = rating
           write (outbuf,1090) jown,intovr,bus(k2),base(k2),id,
     1                         nsect,irate,lines,
     2                       (brnch(k,nbr),k=5,9),irate1,irate2,mo,kyr
 1090      format(t5,'L',2x,a3,1x,i1,1x,a8,f6.1,1x,a1,i2,i5,
     1           ' AMP ',i1,' C',f8.5,' R',f9.5,' X',f8.5,' G',
     2           f9.5,' B',f8.1,' MI',t109,i6,' T',i6,' B',
     3           t128,a1,i2,'IN'  )
           call prtout(1)

        else if (ltype .eq. BRTYP_T) then
c                 "T" branch
           irate1 = rateln(1,nbr)
           irate2 = rateln(2,nbr)
           irate3 = rateln(3,nbr)
           if ( q.gt.0 ) then       ! taps as input
              t1 = brnch(9,nbr)
              t2 = brnch(10,nbr)
           else                     ! transpose taps 
              t2 = brnch(9,nbr)
              t1 = brnch(10,nbr)
           endif

           if (t1 .ge. 1000.0 .or. t1 .le. -100.0) then
              write (t1c,1102) t1
 1102         format(f7.1)
           else
              write (t1c,1104) t1
 1104         format(f7.2)
           endif

           if (t2 .ge. 1000.0 .or. t2 .le. -100.0) then
              write (t2c,1102) t2
           else
              write (t2c,1104) t2
           endif

           irate = rating
           write (outbuf,1110) jown,intovr,bus(k2),base(k2),
     1           id,nsect,irate,lines,(brnch(k,nbr),k=5,8),
     2           t1c,t2c,irate1,irate2,irate3,mo,kyr
 1110      format(t5,'T',2x,a3,1x,i1,1x,a8,f6.1,1x,a1,i2,i5,
     1           ' MVA ',i1,' C',f8.5,' R',f9.5,' X',f8.5,' G',
     2           f9.5,' B',2a7,i6,' T',i6,' E',i6,' B',
     3           t128,a1,i2,'IN'  )
           call prtout(1)

        else if (ltype .eq. BRTYP_R) then
c                 "R " branch
           call getchr(1,trtyp,kbrnch(3,nbr))
           if (trtyp.ne.' ' .and. 
     &        trtyp.ne.'V' .and. trtyp .ne. 'O') then
              cntrlb = ' '
              ctbkv = 0.0
           else
              k49=kbrnch(4,nbr)
              cntrlb=bus(k49)
              ctbkv = base(k49)
           endif
           write (outbuf,1070) trtyp,intovr,bus(k2),
     1           base(k2),id,cntrlb,ctbkv,(brnch(k,nbr),k=6,10)
 1070      format(t5,'R',a1,5x,i1,1x,a8,f6.1,1x,a1,3x,a8,f6.1,
     1            f8.2,' MAX ',f7.2,' MIN',f4.0,' TAPS ',f9.1,
     2           ' MAX SCHED',f9.1,' MIN SCHED')
           call prtout(1)

        else if (ltype .eq. BRTYP_TP) then
c                 "TP" branch
           irate1 = rateln(1,nbr)
           irate2 = rateln(2,nbr)
           irate3 = rateln(3,nbr)
           t1 = brnch(9,nbr)
           if (q.lt.0) t1 = - t1        ! transpose phase shift
           if (t1 .ge. 1000.0 .or. t1 .le. -100.0) then
              write (t1c,1102) t1
           else
              write (t1c,1104) t1
           endif
           t2 = brnch(10,nbr)
           if (t2 .ge. 1000.0 .or. t2 .le. -100.0) then
              write (t2c,1102) t2
           else
              write (t2c,1104) t2
           endif
           irate = rating
           write (outbuf,1130) jown,intovr,bus(k2),base(k2),id,
     1            nsect,irate,lines,(brnch(k,nbr),k=5,8),t1c,
     2            t2c,irate1,irate2,irate3,mo,kyr
 1130      format(t5,'TP',1x,a3,1x,i1,1x,a8,f6.1,1x,a1,i2,i5,
     1            ' MVA ',i1,' C',f8.5,' R',f9.5,' X',f8.5,' G',
     2            f9.5,' B',2a7,i6,' T',i6,' E',i6,' B',t128,
     3            a1,i2,'IN'  )
           call prtout(1)

        else if (ltype .eq. BRTYP_E) then
c                 "E " branch
           irate1 = rateln(1,nbr)
           irate2 = rateln(2,nbr)
           irate = rating
           r = brnch(5,nbr)
           x = brnch(6,nbr)
           if (q.gt.0) then      ! data in input order
              g1 = brnch(7,nbr)
              b1 = brnch(8,nbr)
              g2 = brnch(9,nbr)
              b2 = brnch(10,nbr)
           else                  ! data is transposed
              g2 = brnch(7,nbr)
              b2 = brnch(8,nbr)
              g1 = brnch(9,nbr)
              b1 = brnch(10,nbr)
           endif
           write (outbuf,1150) jown,intovr,bus(k2),base(k2),id,
     1            nsect,irate,lines,r,x,g1,b1,g2,b2,
     2            irate1,irate2,mo,kyr
 1150      format(t5,'E',2x,a3,1x,i1,1x,a8,f6.1,1x,a1,i2,i5,
     1            ' AMP ',i1,' C',f8.5,' R',f9.5,' X',f8.5,' G1',
     2            f8.5,' B1',f8.5,' G2',f8.5,' B2',t110,i5,' T',
     3            i6,' B',t128,a1,i2,'IN'  )
           call prtout(1)

        else if (ltype .eq. BRTYP_RZ) then
c                 "RZ" branch
           write (outbuf,1034) jown,bus(k2),base(k2),id,nsect,
     1            (brnch(k,nbr),k=4,10),brnch(18,nbr),mo,kyr
 1034      format(t5,'RZ',1x,a3,3x,a8,f6.1,1x,a1,i2,f4.0,' Type ',
     1            2f7.0,' P ',f6.0,' I Rate ',2f9.5,' X ',2f9.5,
     2           ' B1 ',t128,a1,i2,'IN'  )
           call prtout(1)

        else if (ltype .eq. BRTYP_LM .or. ltype .eq. BRTYP_LD) then
c                 "LM" and "LD" branches
           rate = brnch(4,nbr)
           amiles = brnch(16,nbr)
           kdin = kbrnch(11,nbr)
           kyr=kdin/100
           mo = ' '
           call getchr(3,jown,kbrnch(3,nbr))
           if (ltype .eq. BRTYP_LD) then
c              "ld" branch
              cc=' '
              if (nsect.eq.1) cc='R'
              if (nsect.eq.2) cc='I'
              irate = rate
              pd = brnch(8,nbr)
              vd = brnch(9,nbr)
              if ( q .gt. 0 ) then     ! data as input
                 ara = brnch(10,nbr)
                 aia = brnch(18,nbr)
              else                     ! transpose
                 pd = - pd
                 ara = brnch(18,nbr)
                 aia = brnch(10,nbr)
              endif
              write (outbuf,1170) jown,intovr,bus(k2),base(k2),
     1           irate,(brnch(k,nbr),k=5,7),cc,pd,vd,
     2           ara,aia,amiles,mo,kyr
 1170         format(t5,'LD',1x,a3,1x,i1,1x,a8,f6.1,4x,i5,' AMP',
     1               f8.2,' R',f8.2,' L',f8.2,' C ',a1,f7.1,' MW',
     2               f7.1,' KV',f6.1,' ALF',f6.1,' GAM', f7.1,
     3               ' MI ',a1,i2,'IN' )
           else
c                   "lm" branch
              irate = rate
              write (outbuf,1190) jown,intovr,bus(k2),base(k2),
     1                     irate,(brnch(k,nbr),k=5,7),amiles,mo,kyr
 1190         format (t5,'LM',1x,a3,1x,i1,1x,a8,f6.1,4x,i5,' AMP',
     1                f8.2,' R',f8.2,' L',f8.2,' C',42x,f7.1,' MI',
     2                1x,a1,i2,'IN')
           endif 
           call prtout(1)
        endif
        return
        end
