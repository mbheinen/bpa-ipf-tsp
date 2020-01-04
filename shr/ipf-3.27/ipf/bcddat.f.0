C    %W% %G%
      subroutine bcddat (ind, xbuf)
c $$$
c $$$ Caution.  IPF conversion has revealed that RM and RN records
c $$$ are transposed differently.  The Pmin, Pmax and Qmin, Qmax 
c $$$ quantities were inconsistently transposed in the batch PF.
c $$$
C                                                                      *
C     This subroutine encodes bus,branch,+bus,or xdata information in  *
C     xbuf                                                             *
C                                                                      *
C       input data:                                                    *
C               common tables for bus,branch,+bus,xdata                *
C               pointer index to the tables 'ind'                      *
C                                                                      *
C       output data: xbuf*(*)                                          *
C                                                                      *
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/arcntl.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/com007.inc'
      include 'ipfinc/pqcurves.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/xdata.inc'

      common /bcdflg/ bldtbx, lowx, xlinmn, npctq, pctq(MAXBUS)
      logical bldtbx, lowx
 
      character xbuf * (*)
      character basbcd*4, pl*5, ql*5, gmw*5, gm*4, bm*4, vmin*4, vmax*4,
     &          pct*3 ,gpmx*5, qmin*5, qmax*5, bascnt*4, br*2, sr*5, 
     &          amin*5, amax*5,vlv*5, rate5*5, bs1bcd*4, bs2bcd*4, 
     &          tmax*5, tmin*5, vamax*5, vamin*5, cr*6, cx*6, cg*6, 
     &          cb6*6, amiles*4, ctap1*5, ctap2*5, cb1*6, cg1*6, cb2*6,
     &          cg2*6, cc*6, ca*4, cb*4, type*1, buscnt*8, gnom*3, 
     &          gmin*3, suscp(8)*5, id*1, cl*6, ir*1, pdc*6, vdc*5, 
     &          arnet*8, kyrc*2, secc*1, nocktc*1, step(8)*1, intovc*1, 
     &          rvtype*1, pmin*5, pmax*5, rate4*4, xijmin*6, xijmax*6,
     &          bismin*6, bismax*6, code*10, rate(3)*4, yy(8)*10,
     &          value(-1:15)*6, active*1

      integer bptr1, k1, k2, findex, iflag
      character switch*(*)                                            
      real xstep(8), xsuscp(8)
C     ******************************************************************
C                                                                      *
C     Build default percentage vars.                                   *
C                                                                      *
      entry bcdopt (switch, xval)
      if (findex (switch, 'CAL') .ne. 0) then
         write (*, 10)
   10    format (' BCDDAT option set to compute % VARs.')
         call bldpct (pctq)
         bldtbx = .true.
      else if (findex (switch, 'DEF') .ne. 0) then
         write (*, 20)
   20    format (' BCDDAT option set to default % VARs.')
         bldtbx = .false.
      else if (findex (switch, 'LOWX') .ne. 0) then
         xlinmn = xval
         write (*, 30) xlinmn
   30    format (' BCDDAT option set to minimum X = ', f8.5)
         lowx = .true.
      endif
      return

C     ******************************************************************
C                                                                      *
C     encode area interchange data                                     *
C                                                                      *
      entry bcdarc (jb,xbuf)
      bascnt = code(arcbas(jb),4,0)                                    

      arnet = code(bmva * arcnet(jb),8,0)
      write (xbuf,100) arcnam(jb), arcbus(jb), bascnt ,arnet,
     1   (arczns(j,jb),j=1,MAXCAZR)
  100 format( 'A  ', a10, a8, a4, 1x, a8, 10(1x, a2) )
      return
C                                                                      *
C     encode continuation area interchange data                        *
C                                                                      *
      entry bcdarc2 (jb, jc, xbuf)
      j1 = jc * MAXCAZR + 1
      j2 = (jc + 1) * MAXCAZR 
      write (xbuf,102) jc, arcnam(jb), (arczns(j,jb),j=j1,j2)
  102 format( 'A', i1, 1x, a10, t35, 10(1x, a2) )                   
      return

C     ******************************************************************
C                                                                      *
C     encode area intertie "I" data                                    *
C                                                                      *
      entry bcdari (int,xbuf)
      arnet = code(arcinp(int),8,0)
      write (xbuf,110) arcint(1,int), arcint(2,int), arnet
  110 format ('I', 2x, a10, 1x, a10, 2x, a8)
      return

C     ******************************************************************
C                                                                      *
C     encode bus data                                                  *
C                                                                      *
      entry bcdbus (nb,xbuf)
      basbcd = code(base(nb),4,0)
      ktyp = kbsdta(1,nb)
      type = bustyp(ktyp)
      if (ktyp .ne. 5 .and. ktyp .ne. 12) then
         buscnt = '       '
         bascnt = '    '
         pct = '   '
         if (ktyp .eq. 8 .or. ktyp .eq. 11) then
            k1 = kbsdta(13,nb)
            if (k1 .gt. 0) then
               buscnt = bus(k1)
               bascnt = code(base(k1),4,0)
            else
               buscnt = bus(nb)
               bascnt = basbcd
            endif
            if (ktyp .ne. 11) then
               if (.not. bldtbx) then
                  pct = code(busdta(14,nb),3,0)                        

               else
                  pct = code(pctq(nb), 3,0)                            

               endif
            endif
         endif
 
         vmax = code(busdta(11,nb),4,3)
         vmin = code(busdta(12,nb),4,3)
         pl = code(busdta(3,nb),5,0)
         ql = code(busdta(4,nb),5,0)
         gm = code(busdta(5,nb),4,0)
         bm = code(busdta(6,nb),4,0)
         gpmx = code(busdta(7,nb),4,0)
         gmw = code(busdta(8,nb),5,0)
         qmax = code(busdta(9,nb),5,0)
         qmin = code(busdta(10,nb),5,0)
         if (ktyp .eq. 3) vmin = code(busdta(12,nb),4,1)
         write (xbuf,120) type, owner(nb), bus(nb), basbcd, zone(nb)
     1      ,pl, ql, gm, bm, gpmx, gmw, qmax, qmin, vmax, vmin,
     2      buscnt, bascnt, pct
  120    format ('B', a1, 1x, a3, a8, a4, a2, 2a5, 3a4, 3a5, 2a4, a8,
     1           a4, a3)
C                                                                      *
      else
C                                                                      *
         k1 = kbsdta(9,nb)
         br = code(busdta(3,nb),2,0)
         sr = code(busdta(4,nb),5,1)
         amin = code(busdta(5,nb),5,1)
         amax = code(busdta(6,nb),5,1)
         vlv = code(busdta(7,nb),5,1)
         rate5 = code(busdta(8,nb),5,1)
         if (ktyp .eq. 5) then
            if (k1 .gt. 0) then
               buscnt = bus(k1)
               bascnt = code(base(k1),4,0)
            else
               buscnt = ' '                                      
               bascnt = ' '                                          
            endif
            write (xbuf,130) owner(nb), bus(nb), basbcd, zone(nb), br,
     1         sr, amin, amax, vlv, rate5, buscnt, bascnt
  130       format ('BD', 1x, a3, a8, a4, a2, 3x, a2, 5a5, a8, a4)
         else
            if (k1 .le. 0) then
               br = ' '                                                

               sr = ' '                                             
               amin = ' '                                            
               amax = ' '                                            
               vlv = ' '                                            
               gnom = ' '                                             
               gmin = ' '                                             
               rate5 = ' '                                          
               pdc = ' '                                           
               vdc = ' '                                            
               buscnt = ' '
               bascnt = ' '                                          
            else
               gnom = code(busdta(10,nb),3,1)
               gmin = code(busdta(11,nb),3,1)
               pdc = code(busdta(13,nb),6,1)
               vdc = code(busdta(14,nb),5,1)
               buscnt = bus(k1)
               bascnt = code(base(k1),4,0)
            endif
C                                                                      *
            write (xbuf,140) owner(nb), bus(nb), basbcd, zone(nb) ,br,
     1         sr, amin, amax, vlv, rate5, buscnt, bascnt,
     2         busdta(12,nb) ,gnom, gmin, pdc, vdc
  140       format ('BM ', a3, a8, a4, a2, 3x, a2, 5a5, a8, a4, a1, 2a3
     1               , a6, a5)
C                                                                      *
         endif
      endif
      return

C     ******************************************************************
C                                                                      *
C     encode "+" bus data                                              *
C                                                                      *
      entry bcdcbs (ncb,xbuf)
      kt = kbctbl(1,ncb)
      kyr = kbctbl(7,ncb)
      mo = mod(kyr,100)
      year = kyr / 100
      kyrc = code(year,2,0)                                            

      pl = code(bctbl(2,ncb),5,0)
      ql = code(bctbl(3,ncb),5,0)
      gm = code(bctbl(4,ncb),4,0)
      bm = code(bctbl(5,ncb),4,0)
      gmw = code(bctbl(6,ncb),5,0)
      qmax = code(bctbl(11,ncb),5,0)
      qmin = code(bctbl(12,ncb),5,0)
      basbcd = code(base(kt),4,0)
      write (xbuf,150) bctbl(8,ncb), bctbl(10,ncb), bus(kt), basbcd,
     1   bctbl(9,ncb), pl, ql, gm, bm, gmw, qmax, qmin, month(mo+1), 
     1   kyrc     
  150 format ('+', a1, 1x, a3, a8, a4, a2, 2a5, 2a4, 4x, a5, a5, a5, 
     1        17x, a1, a2) 
      return

C     ******************************************************************
C                                                                      *
C     encode "X" bus data                                              *
C                                                                      *
      entry bcdxdt (jxd,xbuf)
      k1 = xdata(1,jxd)
      basbcd = code(base(k1),4,0)
      k2 = xdata(2,jxd)
      if (k2 .gt. 0) then
         buscnt = bus(k2)
         bascnt = code(base(k2),4,0)
      else
         buscnt = bus(k1)
         bascnt = basbcd
      endif
c
c     Test if more than 10 switchable steps
c
      iflag = 0
      ilast = 0
      j = 0
      do i = 7, 21, 2
         j = j + 1
         xstep(j) = xdata(i,jxd)
         xsuscp(j) = xdata(i+1,jxd)
         if (xstep(j) .ge. 10.0) iflag = j
         if (xstep(j) .gt. 0.0) ilast = j
      enddo
c
c     Edit the original overflowed entity to contain 9 steps 
c     and create an overflow entity to receive steps 10, ....
c
      do while (iflag .ne. 0 .and. ilast .lt. 8)
         do j = 7, iflag, -1
            xstep(j+1) = xstep(j)
            xsuscp(j+1) = xsuscp(j)
         enddo
         xstep(iflag+1) = xstep(iflag) - 9.0
         xstep(iflag) = 9.0
         iflag = 0
         ilast = 0
         do j = 1, 8
            if (xstep(j) .ge. 10.0) iflag = j
            if (xstep(j) .gt. 0.0) ilast = j
         enddo
      enddo
      do j = 1, 8
         step(j) = code (xstep(j), 1, 0)
         suscp(j) = code (xsuscp(j), 5, 0)
      enddo

      write (xbuf,170) bus(k1), basbcd, buscnt, bascnt, (step(i),
     1   suscp(i),i=1,8)
  170 format ('X', 5x, a8, a4, 2x, a8, a4, 8(a1, a5))
      return

C     ******************************************************************
C                                                                      *
C     encode "QP" data                                                 *
C                                                                      *
      entry bcdqpd (jt, xbuf)
      k1 = pqbusptr(jt)
      basbcd = code(base(k1),4,0)
      if (pqactive(jt)) then
         active = ' '
      else
         active = '*'
      endif
      value(-1) = code(pqpgen(-1,jt), 5, 1)
      value(0) = code(pqpgen(0,jt), 5, 1)
      do i = 1, 15
         value(i) = code(pqpgen(i,jt), 6, 2)
      enddo
      write (xbuf, 172) 'QP', pqid(jt), active, bus(k1), basbcd, 
     &  pqnumunit(jt), value
  172 format (a2, t4, a2, t6, a1, a8, a4, t19, i2, 2a5, 15a6)
      return

C     ******************************************************************
C                                                                      *
C     encode "QX" data                                                 *
C                                                                      *
      entry bcdqxd (jt, xbuf)
      k1 = pqbusptr(jt)
      basbcd = code(base(k1),4,0)
      if (pqactive(jt)) then
         active = ' '
      else
         active = '*'
      endif
      write (value(-1), fmt='(f5.3)') pqqmax(-1,jt)
      value(0) = ' '
      do i = 1, 15
         value(i) = code(pqqmax(i,jt), 6, 2)
      enddo
      write (xbuf, 172) 'QX', pqid(jt), active, bus(k1), basbcd, 
     &  pqnumunit(jt), value
      return

C     ******************************************************************
C                                                                      *
C     encode "QN" data                                                 *
C                                                                      *
      entry bcdqnd (jt, xbuf)
      k1 = pqbusptr(jt)
      basbcd = code(base(k1),4,0)
      if (pqactive(jt)) then
         active = ' '
      else
         active = '*'
      endif
      write (value(-1), fmt='(f5.3)') pqqmin(-1,jt)
      value(0) = ' '
      do i = 1, 15
         value(i) = code(pqqmin(i,jt), 6, 2)
      enddo
      write (xbuf, 172) 'QN', pqid(jt), active, bus(k1), basbcd, 
     &   pqnumunit(jt), value
      return

C     ******************************************************************
C                                                                      *
C     encode branch data                                               *
C                                                                      *
      entry bcdbrn (bptr1,xbuf)
 
      jbrx = brnch_ptr(bptr1)
      jbr = iabs(jbrx)
      k1 = kx(bptr1)
      k2 = ky(bptr1)
      ntybr = brtype(bptr1)
      id = brid(bptr1)
      sect = brsect(bptr1)

      if (ntybr .eq. 2 .or. ntybr .eq. 7) then
         secc = ' '
         nocktc = ' '
      else
         secc = code(sect,1,0)                                         
         ckt = amin1 (9.0, brnch(16,jbr))
         nocktc = code(ckt,1,0)                                         
      endif
      if (ntybr .eq. 1) then
         mo = 0
         kyrc = ' '
      else
         mo = mod(kbrnch(11,jbr),100)
         year = kbrnch(11,jbr) / 100
         kyrc = code(year,2,0)                                         

      endif
      overrd = kbrnch(15,jbr)
      if (jbrx .lt. 0) then
         if (overrd. gt. 0.0) overrd = 3.0 - overrd
      endif
      intovc = code(overrd,1,0)                                        

      bs1bcd = code(base(k1),4,0)
      bs2bcd = code(base(k2),4,0)
      if (ntybr .eq. 4 .or. ntybr .eq. 9) then
         rate4 = ' '
      else
         rate4 = code(brnch(4,jbr),4,0)
      endif
      do i = 1, 3                                                
         if (rateln(i,jbr) .eq. 0.0) then                            
            rate(i) = ' '                                            
         else if (rateln(i,jbr) .lt. 9999.0) then
            rate(i) = code(rateln(i,jbr),4,0)                       
         else
            rate(i) = '9999'
         endif                                                       
      enddo
C                                                                      *
      if ( ntybr .eq. 3 ) then
C                                                                      *
C        encode "L" branch                                             *
C                                                                      *
         cr = code(brnch(5,jbr),6,5)
         if (lowx) then
            if (abs(brnch(6,jbr)) .lt. xlinmn) then
               cx = code(sign (xlinmn, brnch(6,jbr)), 6,5)
            else
               cx = code(brnch(6,jbr),6,5)
            endif
         else
            cx = code(brnch(6,jbr),6,5)
         endif
         cg = code(brnch(7,jbr),6,5)
         cb6 = code(brnch(8,jbr),6,5)
         amiles = code(brnch(9,jbr),4,1)
         write (xbuf,210) brnch(3,jbr), bus(k1), bs1bcd, intovc,
     1      bus(k2), bs2bcd, id, secc, rate4, nocktc, cr, cx, cg,
     2      cb6, amiles, month(mo+1), kyrc, rate(1), rate(2)
  210    format ('L', 2x, a3, a8, a4, a1, a8, a4, a1, a1, a4, a1, 4a6,
     1      a4, 8x, a1, a2, t81, 2a4)
      else if ( ntybr .eq. 5 .or. ntybr .eq. 6 ) then
C                                                                      *
C        encode "T" data                                               *
C                                                                      *
         cr = code(brnch(5,jbr),6,5)
         if (lowx) then
            if (abs(brnch(6,jbr)) .lt. xlinmn) then
               cx = code(sign (xlinmn, brnch(6,jbr)), 6,5)
            else
               cx = code(brnch(6,jbr),6,5)
            endif
         else
            cx = code(brnch(6,jbr),6,5)
         endif
         cg = code(brnch(7,jbr),6,5)
         cbabs = abs(brnch(8,jbr))
         cb6 = code(cbabs,6,5)                                   
         if (jbrx .gt. 0) then
            ctap1 = code(brnch(9,jbr),5,2)
            ctap2 = code(brnch(10,jbr),5,2)
         else if (ntybr .eq. 5) then
            ctap2 = code(brnch(9,jbr),5,2)
            ctap1 = code(brnch(10,jbr),5,2)
         else
            ctap1 = code(-brnch(9,jbr),5,2)
            ctap2 = code(brnch(10,jbr),5,2)
         endif
         type = ' '
         if (ntybr .eq. 6) type = 'P'
         write (xbuf,220) type, brnch(3,jbr), bus(k1), bs1bcd ,intovc,
     1      bus(k2), bs2bcd, id, secc, rate4, nocktc, cr, cx ,cg,
     2      cb6, ctap1, ctap2, month(mo+1), kyrc, rate(1), rate(2),
     3      rate(3)
  220    format ('T', a1, 1x, a3, a8, a4, a1, a8, a4, a1, a1, a4, a1,
     1      4a6, 2a5, 2x, a1, a2, t81, 3a4)
C                                                                      *
      else if ( ntybr .eq. 8 ) then
C                                                                      *
C        encode "E"                                                    *
C                                                                      *
         cr = code(brnch(5,jbr),6,5)
         if (lowx) then
            if (abs(brnch(6,jbr)) .lt. xlinmn) then
               cx = code(sign (xlinmn, brnch(6,jbr)), 6,5)
            else
               cx = code(brnch(6,jbr),6,5)
            endif
         else
            cx = code(brnch(6,jbr),6,5)
         endif
         if (jbrx .gt. 0) then
            cg1 = code(brnch(7,jbr),6,5)
            cb1 = code(brnch(8,jbr),6,5)
            cg2 = code(brnch(9,jbr),6,5)
            cb2 = code(brnch(10,jbr),6,5)
         else
            cg2 = code(brnch(7,jbr),6,5)
            cb2 = code(brnch(8,jbr),6,5)
            cg1 = code(brnch(9,jbr),6,5)
            cb1 = code(brnch(10,jbr),6,5)
         endif
         write (xbuf,230) brnch(3,jbr), bus(k1), bs1bcd, intovc,
     1      bus(k2), bs2bcd, id, secc, rate4, nocktc, cr, cx, cg1,
     2      cb1, cg2, cb2, month(mo+1), kyrc, rate(1), rate(2)
  230    format ('E', 2x, a3, a8, a4, a1, a8, a4, a1, a1, a4, a1, 6a6,
     1           a1, a2, t81, 2a4)  
C                                                                      *
      else if ( ntybr .eq. 2 .or. ntybr .eq. 7 ) then
C                                                                      *
C        encode "LD" and "LM" data                                     *
C                                                                      *
         mo = 0
         rate4 = code(brnch(4,jbr),4,0)
         amiles = code(brnch(16,jbr),4,0)
         cr = code(brnch(5,jbr),6,2)
         cl = code(brnch(6,jbr),6,2)
         cc = code(brnch(7,jbr),6,2)
         if (ntybr .eq. 2) then
            write (xbuf,240) brnch(3,jbr), bus(k1), bs1bcd, intovc,
     1         bus(k2), bs2bcd, rate4, cr, cl, cc, amiles,
     2         month(mo+1), kyrc
  240       format ('LM', 1x, a3, a8, a4, a1, a8, a4, 2x, a4, 3a6, 15x,
     1              a4, a1, a2)
         else
            vdc = code(brnch(9,jbr),5,1)
            ca = code(brnch(10,jbr),4,1)
            cb = code(brnch(18,jbr),4,1)
            ir = 'I'
            if (brsect(bptr1) .eq. 1) ir = 'R'                         

            xpdc = brnch(8,jbr)
            if (jbrx .lt. 0) then
               if (abs(xpdc) .lt. 1000.0) then
                  xpdc = -xpdc
               else if (ir .eq. 'I') then
                  ir = 'J'
               else if (ir. eq. 'R') then
                  ir = 'S'
               endif
            endif
            if (abs(xpdc) .lt. 1000.0) then                     
               pdc = code(xpdc,5,1)                            
            else
               pdc = code(abs(xpdc),5,1)                       
               if (ir .eq. 'I' .and. xpdc .lt. 0.0) ir = 'J'    
               if (ir .eq. 'R' .and. xpdc .lt. 0.0) ir = 'S'    
            endif
            write (xbuf,250) brnch(3,jbr), bus(k1), bs1bcd, bus(k2),
     1         bs2bcd, rate4 ,cr, cl, cc, ir, pdc, vdc, ca, cb,
     2         amiles, kyrc
  250       format ('LD', 1x, a3, a8, a4, 1x, a8, a4, 2x, a4, 3a6, a1,
     1              2a5, 2a4, a4, a2)
         endif
C                                                                      *
      else if ( ntybr .eq. 4 ) then
C                                                                      *
C        encode "R" data         
C                                                                      *
         call getchr (1, rvtype, kbrnch(3,jbr))
         if (index (' MNPQ', rvtype) .eq. 0) rvtype = ' '
         kb = kbrnch(4,jbr)
 
         if ( kb .gt. 0 ) then
            buscnt = bus( kb )
            bascnt = code( base(kb), 4, 0 )
         else if ((kb .eq. -1 .and. jbrx .gt. 0) .or.
     &            (kb .eq. -2 .and. jbrx .lt. 0)) then 
            buscnt = bus( k1 )
            bascnt = code( base(k1), 4, 0 )
         else if ((kb .eq. -2 .and. jbrx .gt. 0) .or.
     &            (kb .eq. -1 .and. jbrx .lt. 0)) then 
            buscnt = bus( k2 )
            bascnt = code( base(k2), 4, 0 )
         else
            buscnt = bus( k1 )
            bascnt = code( base(k1), 4, 0 )
         endif
C                                                                      *
         if (jbrx .gt. 0) then
            tmax = code(brnch(6,jbr),5,2)
            tmin = code(brnch(7,jbr),5,2)
            vamax = code(brnch(9,jbr),5,0)
            vamin = code(brnch(10,jbr),5,0)
         else
            if (rvtype .eq. 'M' .or. rvtype .eq. 'P') then
               tmin = code(-brnch(6,jbr),5,2)
               tmax = code(-brnch(7,jbr),5,2)
            else
               tmax = code(brnch(6,jbr),5,2)
               tmin = code(brnch(7,jbr),5,2)
            endif
            if (rvtype .eq. 'P' .or. rvtype .eq. 'Q') then 
               vamax = code(-brnch(9,jbr),5,0)
               vamin = code(brnch(10,jbr),5,0)
            else if (rvtype .eq. 'M' .or. rvtype .eq. 'N') then
               vamax = code(-brnch(10,jbr),5,0)
               vamin = code(-brnch(9,jbr),5,0)
            else
               vamax = code(brnch(9,jbr),5,0)
               vamin = code(brnch(10,jbr),5,0)
            endif
         endif
         ntap = brnch(8,jbr)
         write (xbuf,270) rvtype, bus(k1), bs1bcd, bus(k2),       
     1      bs2bcd, buscnt, bascnt, tmax, tmin, ntap, vamax, vamin
  270    format ('R', a1, 4x, a8, a4, 1x, a8, a4, 2x, a8, a4, 2a5, i2,
     1            2a5)
      else if ( ntybr .eq. 9 ) then
C                                                                      *
C        encode "RZ"                                                   *
C                                                                      *
         rvtype = code(brnch(4,jbr),1,0)
         if (jbrx .gt. 0) then
            pmax = code(brnch(5,jbr),5,0)
            pmin = code(brnch(6,jbr),5,0)
         else
            pmin = code(-brnch(5,jbr),5,0)
            pmax = code(-brnch(6,jbr),5,0)
         endif
         rate4 = code(brnch(7,jbr),4,0)
         xijmax = code(brnch(8,jbr),6,5)
         xijmin = code(brnch(9,jbr),6,5)
         bismax = code(brnch(10,jbr),6,5)
         bismin = code(brnch(18,jbr),6,5)
         write (xbuf,280) bus(k1), bs1bcd, bus(k2), bs2bcd, id, secc,
     1      rvtype, pmax, pmin, rate4, xijmax, xijmin, bismax, bismin
  280    format ('RZ', 4x, a8, a4, 1x, a8, a4, a1, a1, a1, 2a5, 
     1           a4, 4a6 ) 
C                                                                      *
C        Encode equivalent - pi                                        *
C                                                                      *
      else if (ntybr .eq. 1 ) then
         if (len (xbuf) .lt. 120) then
            write (xbuf,290) bus(k1), bs1bcd, bus(k2), bs2bcd ,id, secc
  290       format ('L*', 4x, a8, a4, 1x, a8, a4, a1, a1,
     1           ' *** equivalent pi ***')
         else
            do i = 1, 8
               yy(i) = code(brnch(i+3,jbr), 10, 5)
            enddo
            write (xbuf,300) bus(k1), bs1bcd, bus(k2), bs2bcd ,id, secc,
     &         yy
  300       format ('L*', 4x, a8, a4, 1x, a8, a4, a1, a1, t39, 8a10)
         endif
      endif
C                                                                      *
      return
      end
