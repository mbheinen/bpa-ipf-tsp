C    @(#)lodsol.f	20.3 2/13/96
      subroutine lodsol (error)
 
C     process /LOAD_SOLUTION commands.
C
C     Input parameter:
C
C     ERROR  = flag: 0/1 = no errors/errors encountered.
 
      include 'ipfinc/parametr.inc'
c		 
      include 'ipfinc/alpha.inc'
c	Global variables used:
c		vlimn(r*4), vlimx(r*4) 
      include 'ipfinc/blank.inc'
c	Global variables used:
c		 buf, card
      include 'ipfinc/branch.inc'
c	Global variables used:
c		 brnch, brtype, brnch_nxt, brnch_ptr
      include 'ipfinc/bus.inc'
c	Global variables used:
c		bus, opt2inp, inb2opt, kbsdta, base, e(r*8), f(r*8)
      include 'ipfinc/ecvar.inc'
c	Global variables used:
c		 None
      include 'ipfinc/filnam.inc'
c	Global variables used:
c		 inpnm
      include 'ipfinc/ikk.inc'
c	Global variables used:
c		 ikk
      include 'ipfinc/jobctl.inc'
c	Global variables used:
c		 None
      include 'ipfinc/lfiles.inc'
c	Global variables used:
c		 None
      include 'ipfinc/optim1.inc'
c	Global variables used:
c		 None
      include 'ipfinc/ordsta.inc'
c	Global variables used:
c		 ordtbx
      include 'ipfinc/prt.inc'
c	Global variables used:
c		 errbuf, outbuf
      include 'ipfinc/slnopt.inc'
c	Global variables used:
c		 ioption
      include 'ipfinc/snput.inc'
c	Global variables used:
c		 None
      include 'ipfinc/tbx.inc'
c	Global variables used:
c		 tbx(r*8)
      include 'ipfinc/tran.inc'
c	Global variables used:
c		 tap(r*8), ltran(i*4)
 
      integer find_bus, error, teminp, sect, ptr, ptrx, ptrltc

      character bus1 * 8, bus2 * 8, word(10) * 60, capital * 60,
     1          temfil * 60, id * 1, xbuf * 120, bigbuf * 512,
     2          comprs * 512, cfmt * 7, cfmtbuf(10) * 40

      logical found, polar, debug
      double precision told, tapnew
      external find_bus
 
      error = 0
      numrcd = 0
      polar = .false.   
      debug = .false.   

C     /LOAD_SOLUTION, VOLTAGES = POLAR,       FILE = <file_name>, -   
C                                RECTANGULAR  
C                     DEBUG = OFF, SOLUTION = BASE 
C                             ON              HOTSTART 

      if (index(buf,'LOAD_SOL') .ne. 0 .or. 
     1    index(buf,'LOADSOL') .ne. 0) then 

         write (outbuf, 90) buf(1:80)   
   90    format (' LOAD_SOL text (',a,')')  
         call prtout (1)
         bigbuf = comprs (buf)  

C        Check for and concatenate continuation records.

         last = lastch (bigbuf) 
         do while (bigbuf(last:last) .eq. '-') 
            read (inp, 94, end = 410) buf   
   94       format (a)  
            bigbuf(last:) = comprs(buf) 
            last = lastch (bigbuf) 
         end do  

         call scan(bigbuf, word, nwrd)  
         do i = 1, nwrd 
            word(i) = capital (word(i))  
         enddo

         do 120 iwrd = 2, nwrd, 2   
            if (word(iwrd)(1:4) .eq. 'FILE') then   
               temfil = inpnm   
               teminp = inp 
               inpnm = word(iwrd+1) 
               inp = 23 
               error = 0
               call opnfil (inp, inpnm, error)
               if (error .ne. 0) then
                  write (errbuf(1), 110) inpnm
  110             format ('0 File ', a,
     &                    ' cannot be opened. File ignored.')
                  call prterx ('W',1)
                  inpnm = temfil
                  inp = teminp  
               else 
                  incsw = 1 
               endif
        
            else if (word(iwrd)(1:4) .eq. 'VOLT') then  
               if (word(iwrd+1) .eq. 'POLAR') then  
                  polar = .true.
               else 
                  polar = .false.   
               endif
        
            else if (word(iwrd) .eq. 'DEBUG') then  
               if (word(iwrd+1) .eq. 'ON') then 
                  debug = .true.
               else 
                  debug = .false.   
               endif
        
            else if (word(iwrd)(1:4) .eq. 'SOLU') then  
               if (word(iwrd+1)(1:3) .eq. 'BAS') then   
                  iopton(20) = 2
               else if (word(iwrd+1)(1:3) .eq. 'HOT') then  
                  iopton(20) = 3
               endif
        
            endif   
        
  120    continue   
C       
C        Initialize IKK array:  
C       
C          (1,*) = 0 :  (bus is eligible for LOAD_SOLUTION values). 
C                  1 :  (bus voltage has been redefined).   
C          (2,*) =      (not used.) 
C          (3,*) = I    (cross index to TBX array)  
C          (4,*) = NTYP (bus type)  
C          (5,*) = 0:   (LTC TAP array not updated).
C                  1:   (LTC TPA array updated).
C       
         do 130 nb = 1, ntot
            ikk(1,nb) = 0   
            ikk(2,nb) = 1   
            ikk(3,nb) = 0   
            ikk(4,nb) = kbsdta(1,nb)
            ikk(5,nb) = 0   
  130    continue   
        
         do 140 i = 1, ntotb
            ltyp = tbx(1,i)
            if (ltyp .lt. 10) then  
               nb = tbx(2,i)   
               if (ordtbx .eq. 2) nb = opt2inp(nb)
               ikk(3,nb) = i
            endif   
  140    continue   
        
  160    found = .false.
         read (inp, 170, end = 410) buf 
  170    format (a) 
         card = buf(1:1)
        
         if (card .eq. '.') then
            go to 160   
         else if (card .eq. 'B') then   
            numrcd = numrcd + 1 
            found = .true.  
            read (buf, 180) bus1, base1 
  180       format (bz, t7, a8, f4.0) 
            nb = find_bus (bus1, base1)   
            if (nb .le. 0) then 
               write (errbuf(1), 190) bus1, base1   
  190          format ('LOAD_SOLUTION bus (', a8, f6.1, 
     1            ') is not in system.')
               write (errbuf(2), 192) buf(1:80) 
  192          format (t12, '(', a, ')')
               call prterx ('W', 2) 
               error = 1
            else
               if (ikk(1,nb) .eq. 1) then   
                  write (errbuf(1), 200) bus1, base1
  200             format ('Duplicate LOAD_SOLUTION buses (',a8,f6.1,
     1                    '). First record ignored.')
                  write (errbuf(2), 192) buf(1:80)  
                  call prterx ('W', 2)  
               else 
                  ikk(1,nb) = 1 
               endif
               call uscan( buf(20:), cfmtbuf, nfmt, '@', ', ' )
               write (cfmt, 102) lastch( cfmtbuf(1) )
  102          format ('(f', i2, '.0)')
               read (cfmtbuf(1), fmt=cfmt) ee
               write (cfmt, 102) lastch( cfmtbuf(2) )
               read (cfmtbuf(2), fmt=cfmt) ff

               kt = inp2opt(nb)   
               vold = dsqrt (e(kt) ** 2 + f(kt) ** 2)
               if (polar) then  
                  e(kt) = ee * cos(ff)  
                  f(kt) = ee * sin(ff)  
               else 
                  e(kt) = ee
                  f(kt) = ff
               endif
               vnew = dsqrt (e(kt) ** 2 + f(kt) ** 2)
               call glbvlt (nb, vmin, vmax) 
               if (vnew .lt. vmin - 0.001 .or. vnew .gt. vmax + 0.001)  
     1             then 
                  write (errbuf(1), 202) bus1, base1, vnew, vmin,   
     1               vmax   
  202             format (' Rescheduled voltage on bus ', a8, f6.1, 
     1                    ' violates global limits: VNEW, VMIN, VMAX ', 
     2                    3f8.3)
                  write (errbuf(2), 192) buf(1:80)  
                  call prterx ('W', 2)  
               endif
               if (vlimn(kt) .eq. vlimx(kt)) then   
                  vlimn(kt) = vnew
                  vlimx(kt) = vnew
               else 
                  vlimn(kt) = amin1 (vlimn(kt), vnew)   
                  vlimx(kt) = amax1 (vlimx(kt), vnew)
               endif
               jtbx = ikk(3,nb) 
               if (jtbx .gt. 0) then
                  if (tbx(1,jtbx) .eq. 2) then 
                     if (abs (vnew - tbx(5,jtbx)) .gt. 0.001) then  
                        write (errbuf(1), 204) bus1, base1, 
     1                     tbx(5,jtbx), vnew
  204                   format (' Rescheduled voltages on BQ bus ', a8, 
     1                     f6.1, ' VOLD, VNEW ', 2f8.3) 
                        write (errbuf(2), 192) buf(1:80)
                        call prterx ('I', 2)
                     endif  
                     tbx(5,jtbx) = vnew 
                  endif 
               endif
               if (debug) then  
                  write (dbug, 208) bus(nb), base(nb), vold, vnew   
  208             format (' B ', a8, f6.1, ' VOLD, VNEW ', 2f10.5)  
               endif
            endif   
        
         else if (buf(1:1) .eq. 'T') then   
        
            numrcd = numrcd + 1 
            found = .true.  
            read (buf, 210) bus1, base1, bus2, base2, id, sect  
  210       format (bz, t7, a8, f4.0, 1x, a8, f4.0, a1, 1x, i1) 
            k1 = find_bus (bus1, base1)   
            if (k1 .le. 0) then 
               write (errbuf(1), 220) bus1, base1   
  220          format ('LOAD_SOLUTION "T" record, bus1 (', a8, f6.1,
     1            ') is not in system.')
               write (errbuf(2), 192) buf(1:80) 
               call prterx ('W', 2) 
               error = 1
            endif   
            k2 = find_bus (bus2, base2)   
            if (k2 .le. 0) then 
               write (errbuf(1), 230) bus2, base2   
  230          format ('LOAD_SOLUTION "T" record, bus2 (', a8, f6.1,
     1            ') is not in system.')
               write (errbuf(2), 192) buf(1:80) 
               call prterx ('W', 2) 
               error = 1
            endif   
            ptrltc = 0  
            ptr = 0 
            if (k1 .gt. 0 .and. k2 .gt. 0) then 
               write (cfmt, 102) lastch( buf(33:) )
               read (buf(33:), fmt=cfmt) tapnew

               ptr = numbrn (k1, k2, id, sect)  
               if (ptr .le. 0) then 
                  write (errbuf(1), 232) bus(k1), base(k1), bus(k2),
     1               base(k2), id, sect 
  232             format (' Transformer ', a8, f6.1, 1x, a8, f6.1,  
     1                  1x, a1, 1x, i1, ' is not in system.')   
                  write (errbuf(2), 192) buf(1:80)  
                  call prterx ('W', 2)  
               else 
C       
C                 1. Check previous LTC for consistent taps.
C                 2. Find LTC.  
C       
                  nbr = brnch_ptr(ptr)
                  ptrx = numbrn (k1, k2, '*', 0)
                  do while (ptrx .gt. 0 .and. (ky(ptrx) .eq. k2))
                     nbrx = brnch_ptr(ptrx)
                     if (brtype(ptrx) .eq. 4) then
                        ptrltc = ptrx
                        nbrltc = brnch_ptr(ptrltc)
                     else if (nbr .eq. nbrx) then
                     else if (brnch(9,nbrx) .ne. brnch(9,nbr) .or.  
     1                        brnch(10,nbrx) .ne. brnch(10,nbr)) then   
                        write (errbuf(1), 234)   
  234                   format(' Parallel LTC''S have ',
     &                            'inconsistent taps.')  
                        errbuf(2) = ' '  
                        call bcdbrn(ptr,xbuf)  
                        write (errbuf(3), 192) xbuf(1:80)
                        call bcdbrn(ptrx,xbuf)
                        write (errbuf(4), 192) xbuf(1:80)
                        call prterx ('W',4)  
                     endif
                     ptrx = brnch_nxt(ptrx)
                  enddo

                  if (ptrltc .eq. 0) then
                     if (brtype(ptr) .eq. 5) then
                        told = (brnch(9,nbr) / base(k1)) / 
     1                         (brnch(10,nbr) / base(k2))   
                     else if (brtype(ptr) .eq. 6) then   
                        told = brnch(9,nbr) * 0.0174532
                     else  
                        write (errbuf(1), 238) bus(k1), base(k1),  
     1                     bus(k2), base(k2), id, sect 
  238                   format (' Input record ', a8, f6.1, 1x,
     &                     a8, f6.1, 1x, a1, 1x, i1, 
     &                     ' does not correspond with a "T" record.')  
                        write (errbuf(2), 192) buf(1:80)   
                        call prterx ('W', 2)   
                     endif 
                     if (abs (told - tapnew) .gt. 0.01) then   
                        write (errbuf(1), 240) bus(k1), base(k1),  
     1                     bus(k2), base(k2), id, sect, told, tapnew   
  240                   format (' Transformer ', a8, f6.1, 1x, 
     1                     a8, f6.1, 1x, a1, 1x, i1,   
     2                     ' cannot change taps ', 2f8.4)  
                        write (errbuf(2), 192) buf(1:80)   
                        call prterx ('W', 2)   
                     endif 
                  else
C       
C                    Find LTC index in TRAN array. 
C       
                     k1x = min0 (k1, k2)   
                     k2x = max0 (k1, k2)   
                     do 242 ltc = 1, ntota 
                        m1 = ltran(1,ltc)  
                        m2 = ltran(9,ltc)  
                        if (ordltc .eq. 2) then
                           m1 = opt2inp(m1)  
                           m2 = opt2inp(m2)  
                        endif  
                        m1x = min0 (m1, m2)
                        m2x = max0 (m1, m2)
                        if (k1x .eq. m1x .and. k2x .eq. m2x) then  
                           go to 248   
                        endif  
  242                continue
                     write (errbuf(1), 246) bus(k1), base(k1), bus(k2),
     1                  base(k2), id   
  246                format (' Transformer ', a8, f6.1, 1x, a8, f6.1,  
     1                  1x, a1, ' does not have an LTC index.')
                     call prterx ('W', 1)  
                     go to 260 
        
C       
C                    Update Tap1 or Tap2 for LTC transformers and  
C                    phase shifters.   
C       
  248                continue  
                     if (k1 .eq. m1) then  
                        ktrpos = 0 
                     else  
                        ktrpos = 1 
                     endif 
                     ityp = mod (ltran(10,ltc), 10)
                     if (brtype(ptrltc) .eq. 4 .and. ityp .eq. 3) then
                        tmax = brnch(6,nbrltc) + 0.05  
                        tmin = brnch(7,nbrltc) - 0.05  
                        if (ktrpos .eq. 1) tapnew = -tapnew
                        tap1 = 57.2957795 * tapnew 
                        if (tap1 .lt. tmin .or. tap1 .gt. tmax) then   
                           write (errbuf(1), 250) tap1, tmin, tmax 
  250                      format(' LTC tap ', f8.2,   
     1                         ' violates limit ', 2f8.2)   
                           errbuf(2) = ' ' 
                           call bcdbrn(ptrltc,xbuf)
                           write (errbuf(3), 192) xbuf(1:80)   
                           call bcdbrn(ptr,xbuf)   
                           write (errbuf(4), 192) xbuf(1:80)   
                           call prterx ('W',4) 
                        endif  
                        tap1 = amin1 (tmax,tap1)   
                        tap1 = amax1 (tmin,tap1)   
                     else if (brtype(ptrltc) .eq. 4) then
                        tmax = brnch(6,nbrltc) + 0.05  
                        tmin = brnch(7,nbrltc) - 0.05  
                        ck = tran(6,ltc)   
                        if (ktrpos .eq. 1) tapnew = 1.0 / tapnew   
                        tap1 = base(m2) * ck / tapnew  
                        if (tap1 .lt. tmin .or. tap1 .gt. tmax) then   
                           write (errbuf(1), 250) tap1, tmin, tmax 
                           errbuf(2) = ' ' 
                           call bcdbrn(ptrltc,xbuf)
                           write (errbuf(3), 192) xbuf(1:80)   
                           call bcdbrn(ptr,xbuf)   
                           write (errbuf(4), 192) xbuf(1:80)   
                           call prterx ('W',4) 
                        endif  
                        tap1 = amin1 (tmax,tap1)   
                        tap1 = amax1 (tmin,tap1)   
                     endif 
                     tapold = tap(ltc) 
                     tap(ltc) = tapnew 
C       
C                    Update Y-matrix.  
C       
                     if (ikk(5,ltc) .eq. 0) then   
                        call ltcadj (ltc,dble(tapnew-tapold),'LODSOL') 
                        ikk(5,ltc) = 1 
                     endif 
                     if (brtype(ptr) .eq. 5) then
                        tnew = tap1
                        if (ktrpos .eq. 0) then
                           told = brnch(10,nbr)
                           brnch(10,nbr) = tap1
                        else   
                           told = brnch(9,nbr) 
                           brnch(9,nbr) = tap1 
                        endif  
                     else if (brtype(ptr) .eq. 6) then   
                        if (ktrpos .eq. 0) then
                           told = brnch(9,nbr) 
                           tnew = tap1 
                           brnch(9,nbr) = tap1 
                        else   
                           told = -brnch(9,nbr)
                           tnew = -tap1
                           brnch(9,nbr) = -tap1
                        endif  
                     endif 
                     if (debug) then   
                        write (dbug, 252) bus(k1), base(k1),   
     1                     bus(k2), base(k2), id, sect, told, tnew 
  252                   format (' T ', a8, f6.1, 1x, a8, f6.1, 
     1                     1x, a1, 1x, i1, ' TOLD, TNEW ', 2f10.5) 
                     endif 
                  endif
               endif
  260       continue
            endif   
        
         else if (index ('(/HCS>', card) .ne. 0) then   
        
            go to 900   
        
         else   
        
            write (errbuf(1), 400) buf(1:80)
  400       format('Unrecognizable LOAD_SOLUTION text (', a, ')')   
            call prterx ('E', 1)
            error = 1   
            go to 900   
        
         endif  
         go to 160  
C       
C        Check for missing buses.   
C       
  410    continue
         do 430 i = 1, ntot 
            if (ikk(1,i) .eq. 0) then   
               write (errbuf(1), 420) bus(nb), base(nb) 
  420          format ('Missing LOAD_SOLUTION bus (', a8, f6.1, ').')   
               call prterx ('W', 1) 
            endif   
  430    continue   
        
      else  
        
         write (errbuf(1), 810) buf(1:80)   
  810    format('Unrecognizable LOAD_SOLUTION text (', a, ')')  
         call prterx ('E', 1)   
         error = 1  
        
      endif 
C       
C     Restore normal input file if alternate file invoked.  
C       
  900 if (incsw .eq. 1) then
         write (outbuf, 910) numrcd, inpnm  
  910    format (1x, i4, ' records read from /LOAD_SOLUTION file ', 
     1      a)  
         call prtout (1)
         close( unit = inp )
         inpnm = temfil 
         inp = teminp   
         incsw = 0  
         read (inp, 170, end = 930) buf 
         go to 940  

  930    buf = '( END ) LODSOL' 
  940    card = buf(1:1)
      endif 
      return
      end   
