C    %W% %G%
        subroutine list_pfd
 
C       This routine formats an input listing of selected powerflow data
c       items
 
      include 'ipfinc/parametr.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/xdata.inc'
      include 'ipfinc/pqcurves.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/com007.inc'
      include 'ipfinc/bstype.inc'
      include 'ipfinc/brtype.inc'

        character  cntrlb*8, zn*2, mo*1, type*1, angle*6, id*1, 
     &             trtyp*1, buscnt*8, jown*3, cc*1, t1c*7, t2c*7,
     &             pqstatus*8 
        integer ptr, q, gtmetpnt
        logical newfrm, ifirst, found

C       ****************************************************************
        entry list_are (na)
        areamw = arcnet(na)*bmva
        write (outbuf,210) arcnam(na), areamw, arcbus(na), arcbas(na),
     1        (arczns(k,na),k=1,MAXCAZR)
  210   format(t10,a10,t46,f9.1,t58,a8,f7.1,t78,10(a2,1x))
        do j = 1, MAXCAZ/MAXCAZR-1
           j1 = j * MAXCAZR + 1
           j2 = (j+1) * MAXCAZR
           if (arczns(j1,na) .ne. ' ') then
              call prtout(1)
              write (outbuf,211) (arczns(k,na),k=j1,j2)
  211         format(t78,10(a2,1x))
           endif
        enddo
        ifirst = .true.
        do j=1,ntotic
           if (arcint(1,j) .eq. arcnam(na)) then
              if (ifirst) then
                 write (outbuf(22:45),212) arcint(2,j),arcinp(j)
  212            format(a10,2x,f9.1)
                 ifirst = .false.
              else
                 write (outbuf,214) arcint(2,j),arcinp(j)
  214            format(t22,a10,t34,f9.1)
              endif
              call prtout(1)
              outbuf = ' '
           endif
        enddo
        if (outbuf.ne.' ') call prtout(1)
        return

C       ****************************************************************
        entry list_bus (nb)
        zn = zone(nb)
        ktype = kbsdta(1,nb)
        qmax = busdta(9,nb)
        qmin = busdta(10,nb)
 
        call typno(type,ktype)

        if (ktype .eq. BSTYP_BG .or. ktype .eq. BSTYP_BX) then
           kb=kbsdta(13,nb)
           if (kb .eq.0) then
              buscnt = '        '
              bascnt = 0.0
           else
              buscnt=bus(kb)
              bascnt=base(kb)
           endif
 
           vmax = busdta(11,nb)
           vmin = busdta(12,nb)
           pct = 0.0
           if (ktype .eq. BSTYP_BG) pct = busdta(14,nb)
           ipct = pct
           write (outbuf,690) type,owner(nb),bus(nb),base(nb),zn,
     1                        (busdta(k,nb),k=3,8),qmax,qmin,vmax,
     2                        vmin,buscnt,bascnt,ipct
  690      format('0B',a1,1x,a3,1x,a8,f6.1,1x,a2,f8.1,'PL',f7.1,
     1            'QL',f6.1,'PS',f6.1,'QS',f6.1,'PM',f7.1,'PG',
     2            f7.1,'QH',f7.1,'QL',f6.3,'VH',f6.3,'VL ',
     3            a8,f6.1,i4,'%')
           call prtout(1)

        else if (ktype .eq. BSTYP_BD) then
           k1=kbsdta(9,nb)
           write (outbuf,700) type,owner(nb),bus(nb),base(nb),zn,
     2           (busdta(k,nb),k=3,8),bus(k1),base(k1)
  700      format('0B',a1,1x,a3,1x,a8,f6.1,1x,a2,f3.0,' BRIDGES',f7.1,
     1            ' MH',f7.1,' AMIN',f7.1,' ASTOP',f7.1,' VALVE DROP',
     2            f7.1,' IMAX ',a8,f6.1,' COM BUS'  )
           call prtout(1)

        else if (ktype .eq. BSTYP_BM) then
           k1 = kbsdta(9,nb)
           bn = busdta(3,nb)
           bd = busdta(7,nb)
           amin = busdta(5,nb)
           amax = busdta(6,nb)
           gnom = busdta(10,nb)
           gmin = busdta(11,nb)
           pd = busdta(13,nb)
           pv = busdta(14,nb)
           if (k1.ne.0) then
              buscnt = bus(k1)
              bascnt = base(k1)
           else
              buscnt = '         '
              bascnt = 0.0
           endif
 
           write (outbuf,700) type, owner(nb),bus(nb),base(nb),
     1                        zn,bn,busdta(4,nb),amin,amax,bd,
     2                        busdta(8,nb),buscnt,bascnt
           call prtout(1)
 
           write (outbuf,820) gnom,gmin,pd,pv,busdta(12,nb)
  820      format(46x,f7.1,' gnom ',f7.1,' gmin ',f9.1,' pdc ',f9.1
     1            ,' vdc ',a1,' converter type ')
           call prtout(1)

        else
           if (ktype .eq. BSTYP_BS) then
              write (angle,750) busdta(12,nb)
  750         format(f6.1)
           else
              write (angle,730) busdta(12,nb)
  730         format(f6.3)
           endif
 
c *** removed bus types JKL ***
c           if (ktype .eq. BSTYP_BJ .or. ktype .eq. BSTYP_BK .or.
c     &         ktype .eq. BSTYP_BL) then
c              write (outbuf,760) type,owner(nb),bus(nb),base(nb),zn,
c     1              (busdta(k,nb),k=3,8),qmax,qmin,busdta(11,nb),angle
c  760         format('0B',a1,1x,a3,1x,a8,f6.1,1x,a2,f8.1,'PL',f7.1,'QL'
c     1               ,f6.1,'PS',f6.1,'QS',f6.1,'PM',f7.1,'PG',f7.1,'QH'
c     2               ,f7.1,'QL',f6.3,'VH',a6,'VL ',a8,f6.1,f5.0,'%')
c              call prtout(1)
c           else
              write (outbuf,770) type,owner(nb),bus(nb),base(nb),zn,
     1              (busdta(k,nb),k=3,8),qmax,qmin,busdta(11,nb),angle
  770         format('0B',a1,1x,a3,1x,a8,f6.1,1x,a2,f8.1,'PL',f7.1,'QL'
     1               ,f6.1,'PS',f6.1,'QS',f6.1,'PM',f7.1,'PG',f7.1,'QH'
     2               ,f7.1,'QL',f6.3,'VH',a6,'VL ',a8,f6.1,f5.0,'%')
              call prtout(1)
c           endif
        endif
        if ( newfrm(iframe) ) then

c          label the microfiche index for 1st bus in a frame

           write (outbuf,880) bus(nb),base(nb),zn
  880      format( 5h$mfie , 5x, a8, f6.1,' KV ',a2 )
           call pfomf(3)
        endif
        return
 
        entry list_cbs (ncb)
 
        k1=kbctbl(1,ncb)
        kdin=kbctbl(7,ncb)
        kyr=kdin/100
        mon = mod(kdin,100) + 1
        mo = month(mon)
        write (outbuf,920) (bctbl(k,ncb),k=8,10),(bctbl(k,ncb),k=2,6),
     &                      bctbl(11,ncb),mo,kyr
920     format(t5,'+',a1,1x,a3,1x,a3,10x,2(f9.1),2(f8.1),10x,f7.1,
     1     2x,f7.1,43x,a1,i2,'IN'   )
        call prtout(1)
        return
 
        entry list_xdt (lxd)
 
        k1 = xdata(1,lxd)
        k2 = xdata(2,lxd)
        if (k2 .eq. 0) k2 = k1
        write (outbuf,960) bus(k2), base(k2), (xdata(k,lxd),k=7,22)
  960   format(7x, 'X', 9x, a8, f6.1, 8(f2.0, 'N', f7.1, ' Q') )
        call prtout(1)
        return
 
c       Print the P/Q curve data

        entry list_pqd(lpq)
        pqstatus = 'Active  '
        if (.not. pqactive(lpq)) pqstatus = 'Inactive'

        write (outbuf,970) pqid(lpq), pqstatus, pqnumunit(lpq), 
     &    (pqpgen(i,lpq),i=-1,7)
  970   format(t5, 'QP Pgen ', a2, 1x, a8, 1x, i2, 'UN', f8.2, 
     &     'MVA', f8.2, 'PMx', f8.2, 'P01', f8.2, 'P02', f8.2, 'P03', 
     &     f8.2, 'P04', f8.2, 'P05', f8.2, 'P06', f8.2, 'P07')
        call prtout(1)
        write (outbuf,971) (pqqmax(i,lpq),i=-1,7)
  971   format(t5, 'QX Qmax      ', 11x, f8.3, '+PF', 9(f8.2, 3X) )
        call prtout(1)
        write (outbuf,972) (pqqmin(i,lpq),i=-1,7)
  972   format(t5, 'QN Qmin      ', 11x, f8.3, '-PF', 9(f8.2, 3X) )
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

        entry list_brn (ptr)
 
        ltype = brtype(ptr)
        k1 = kx(ptr)
        k2 = ky(ptr)
        id = brid(ptr)
        nsect = brsect(ptr)
        q  = brnch_ptr(ptr)
        nbr = iabs(q)
        call getchr(3,jown,kbrnch(3,nbr))
        intovr = gtmetpnt(ptr)
        rating = brnch(4,nbr)
        alines = brnch(16,nbr)
        lines = alines
        kdin = kbrnch(11,nbr)
        if (ltype .eq. BRTYP_PEQ) kdin=0
        kyr = kdin/100
        mon = mod(kdin,100) + 1
        mo = month(mon)

        if (ltype .eq. BRTYP_L) then

c          "L " branch

           irate1 = rateln(1,nbr)
           irate2 = rateln(2,nbr)
           irate = rating
           write (outbuf,1090) jown,intovr,bus(k2),base(k2),id,nsect,
     &         irate,lines, (brnch(k,nbr),k=5,9),irate1,irate2,mo,kyr
 1090      format(t5,'L',2x,a3,1x,i1,1x,a8,f6.1,1x,a1,i2,i5,
     1        ' AMP ',i1,' C',f8.5,' R',f9.5,' X',f8.5,' G',
     2        f9.5,' B',f8.1,' MI',t109,i6,' T',i6,' B',
     3        t128,a1,i2,'IN'  )
           call prtout(1)

        else if (ltype .eq. BRTYP_T) then

c          "T" branch

           irate1 = rateln(1,nbr)
           irate2 = rateln(2,nbr)
           irate3 = rateln(3,nbr)
           if (q .gt. 0) then       ! taps as input
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
     2            f9.5,' B',2a7,i6,' T',i6,' E',i6,' B',
     3            t128,a1,i2,'IN'  )
           call prtout(1)

        else if (ltype .eq. BRTYP_R) then

c          "R " branch

           call getchr(1,trtyp,kbrnch(3,nbr))
           if (trtyp .ne. ' ' .and. 
     &         trtyp .ne. 'V' .and. trtyp .ne. 'O') then
              cntrlb = ' '
              ctbkv = 0.0
           else
              k49=kbrnch(4,nbr)
              if (k49 .eq. -1) k49 = k1
              if (k49 .eq. -2) k49 = k2
              cntrlb=bus(k49)
              ctbkv = base(k49)
           endif

           if (trtyp .eq. 'P' .or. trtyp .eq. 'M') then
              if (q .gt. 0) then
                 tmax = brnch(6,nbr)
                 tmin = brnch(7,nbr)
              else
                 tmax = -brnch(7,nbr)
                 tmin = -brnch(6,nbr)
              endif
           else
              tmax = brnch(6,nbr)
              tmin = brnch(7,nbr)
           endif

           if (trtyp .eq. 'P' .or. trtyp .eq. 'Q') then
              if (q .gt. 0) then
                 pmax = brnch(9,nbr)
                 pmin = brnch(10,nbr)
              else
                 pmax = -brnch(9,nbr)
                 pmin = -brnch(10,nbr)
              endif
           else if (trtyp .eq. 'M' .or. trtyp .eq. 'N') then
              if (q .gt. 0) then
                 pmax = brnch(9,nbr)
                 pmin = brnch(10,nbr)
              else
                 pmax = -brnch(10,nbr)
                 pmin = -brnch(9,nbr)
              endif
           else
              pmax = brnch(9,nbr)
              pmin = brnch(10,nbr)
           endif

           write (outbuf,1070) trtyp,intovr,bus(k2),
     1           base(k2),id,cntrlb,ctbkv,tmax,tmin,brnch(8,nbr),
     2           pmax,pmin
 1070      format(t5,'R',a1,5x,i1,1x,a8,f6.1,1x,a1,3x,a8,f6.1,
     1           f8.2,' MAX ',f7.2,' MIN',f4.0,' TAPS ',f9.1,
     2           ' MAX SCHED',f9.1,' MIN SCHED')
           call prtout(1)

        else if (ltype .eq. BRTYP_TP) then

c          "TP" branch

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
     1           nsect,irate,lines,(brnch(k,nbr),k=5,8),t1c,
     2           t2c,irate1,irate2,irate3,mo,kyr
 1130      format(t5,'TP',1x,a3,1x,i1,1x,a8,f6.1,1x,a1,i2,i5,
     1           ' MVA ',i1,' C',f8.5,' R',f9.5,' X',f8.5,' G',
     2           f9.5,' B',2a7,i6,' T',i6,' E',i6,' B',t128,
     3           a1,i2,'IN'  )
           call prtout(1)

        else if (ltype .eq. BRTYP_E) then

c          "E " branch

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

c          "RZ" branch

           write (outbuf,1034) jown,bus(k2),base(k2),id,nsect,
     1           (brnch(k,nbr),k=4,10),brnch(18,nbr),mo,kyr
 1034      format(t5,'RZ',1x,a3,3x,a8,f6.1,1x,a1,i2,f4.0,' Type ',
     1            2f7.0,' P ',f6.0,' I Rate ',2f9.5,' X ',2f9.5,
     2            ' B1 ',t128,a1,i2,'IN'  )
           call prtout(1)

        else if (ltype .eq. BRTYP_LM .or. ltype .eq. BRTYP_LD) then

c          "LM" and "LD" branches

           rate = brnch(4,nbr)
           amiles = brnch(16,nbr)
           kdin = kbrnch(11,nbr)
           kyr=kdin/100
           mo = ' '
           call getchr(3,jown,kbrnch(3,nbr))
           if (ltype .eq. BRTYP_LD) then

c             "LD" branch

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
     1            irate,(brnch(k,nbr),k=5,7),cc,pd,vd,
     2            ara,aia,amiles,mo,kyr
 1170         format(t5,'LD',1x,a3,1x,i1,1x,a8,f6.1,4x,i5,' AMP',
     1              f8.2,' R',f8.2,' L',f8.2,' C ',a1,f7.1,' MW',
     2              f7.1,' KV',f6.1,' ALF',f6.1,' GAM', f7.1,
     3             ' MI ',a1,i2,'IN' )
              call prtout(1)
           else

c             "LM" branch

              irate = rate
              write (outbuf,1190) jown,intovr,bus(k2),base(k2),
     1              irate,(brnch(k,nbr),k=5,7),amiles,mo,kyr
 1190         format (t5,'LM',1x,a3,1x,i1,1x,a8,f6.1,4x,i5,' AMP',
     1               f8.2,' R',f8.2,' L',f8.2,' C',42x,f7.1,' MI',
     2                1x,a1,i2,'IN')
              call prtout(1)
           endif 
        endif
        return

        end
