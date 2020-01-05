C    @(#)getdrp.f	20.5 11/12/98
      subroutine getdrp (dropmw, error)
 
C     This subroutine processes /GEN_DROP commands.
 
C     Parameters: DROPMW = MW value of dropped generation.
C                 ERROR  = flag: 0/1 = no errors/errors encountered.
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/gendrp.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/optim1.inc'
      include 'ipfinc/ordsta.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/snput.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/tran.inc'
 
      common /is_batch / is_batch

      integer find_bus, error

      external find_bus, kmpgd1, swpgd1, kmpgd2, swpgd2

      character bus1*8, word(100)*30, capital*30, tempc*30,
     &          bigbuf*512, comprs*512

      logical found, chgbrn
 
      numdrp = 0
      numgen = 0
      itdpmx = 0
      dropmw = 0.0  
      drptot = 0.0d0
      drptol = 10.0d0
      error = 0 
      gensum_flag = 1
      chgbrn = .false.  
C                             Initialize IKK array: 
C       
C                (1,*) = 0 :  (generation is not eligible for allocation)   
C                        1 :  (generation is allocatable)   
C                        2 :  (generation is dropped, therefore ineligible  
C                             for allocation)   
C                (2,*) = 0/1  (bus type is not/is eligible for change)  
C                (3,*) = I    (cross index to TBX array)
C                (4,*) = NTYP (bus type)
C                (5,*) = J    (LTC index of controlled bus) 

      do 70 nb = 1, ntot
         ikk(1,nb) = 1  
         ikk(2,nb) = 1  
         ikk(3,nb) = 0  
         ikk(4,nb) = kbsdta(1,nb)   
  70  continue  
      do 80 i = 1, ntotb
         ltyp = tbx(1,i)   
         if (ltyp .lt. 10) then 
            nb = tbx(2,i)  
            if (ordtbx .eq. 2) nb = opt2inp(nb)   
            ikk(3,nb) = i   
         endif  
  80  continue  
      do 82 i = 1, ntota
         ltyp = mod (ltran(10,i), 10)   
         if (ltyp .eq. 1 .or. ltyp .eq. 2 .or. ltyp .eq. 4) then
            kc = ltran(2,i) 
            if (kc .eq. -1) then
               nb = ltran(1,i)  
            else if (kc .eq. -2) then   
               nb = ltran(9,i)  
            else if (kc .gt. 0) then
               nb = kc  
            else
               nb = ltran(1,i)  
            endif   
            if (ordltc .eq. 2) nb = opt2inp(nb)   
            ikk(5,nb) = i   
         else   
            nb = ltran(1,i) 
            if (ordltc .eq. 2) nb = opt2inp(nb)   
            ikk(5,nb) = i   
         endif  
   82 continue  
        
      if (index(buf,'GEN_DROP') .ne. 0 .or. 
     1    index(buf,'GENDROP') .ne. 0) then 
        
C       
C        /GEN_DROP, AI_CONTROL=CON, MAX_ITER=<nn>, TOL=<nn>,
C                              OFF  
C                              MON  
C                   SUMMARY = ON,
C                             OFF,
C       
C                   INITIAL_DROP=<nn>, AREAS=<area1,area2,...>, 
C       
C                   ZONES=<zone1,...>   
C       
         bigbuf = comprs (buf)  
C       
C        Check for and concatenate continuation records.
C       
  144    last = lastch (bigbuf) 
         if (bigbuf(last:last) .eq. '-') then   
            read (inp, 260, end=450) buf
            call space (1)  
            write (outbuf,233 ) buf(1:80)   
            call prtout (1) 
            bigbuf(last:) = comprs(buf) 
            go to 144   
         endif  
         call uscan (bigbuf, word, nwrd, '=', ' ,()<>/\\')   
         do  i = 1, nwrd 
           tempc = capital (word(i)) 
           word(i) = tempc
         enddo
         do 220 i = 2, nwrd - 1, 3  
            last = lastch(word(i+2))
            if (word(i)(1:2) .eq. 'AI') then
C       
C              Check for "=" separator. 
C       
               if (word(i+1) .ne. '=') then 
                  last = lastch (word(i))   
                  write (errbuf(1), 152) word(i)(1:last)
  152             format('Keyword (',a,') in / GEN_DROP ',
     &                   'text is not followed with an "=" sign.')
                  call prterx ('W', 1)
               endif
               if (word(i+2) .eq. 'OFF') then   
                  iopton(17) = 0
               else if (word(i+2) .eq. 'MON') then  
                  iopton(17) = 2
               else 
                  iopton(17) = 1
               endif
            else if (word(i)(1:3) .eq. 'TOL') then  
C       
C              Check for "=" separator. 
C       
               if (word(i+1) .ne. '=') then 
                  last = lastch (word(i))   
                  write (errbuf(1), 152) word(i)(1:last)
                  call prterx ('W', 1)  
               endif
               drptol = ftn_atof (word(i+2)(1:last))
            else if (word(i)(1:3) .eq. 'SUM') then  
C       
C              Check for "=" separator. 
C       
               if (word(i+1) .ne. '=') then 
                  last = lastch (word(i))   
                  write (errbuf(1), 152) word(i)(1:last)
                  call prterx ('W', 1)  
               endif
               if (word(i+2)(1:last) .eq. 'OFF' .or.
     &             word(i+2)(1:last) .eq. 'off') gensum_flag = 0
            else if (word(i)(1:3) .eq. 'INI') then  
C       
C              Check for "=" separator. 
C       
               if (word(i+1) .ne. '=') then 
                  last = lastch (word(i))   
                  write (errbuf(1), 152) word(i)(1:last)
                  call prterx ('W', 1)  
               endif
               drptot = ftn_atof (word(i+2)(1:last))
               dropmw = drptot  
            else if (word(i)(1:3) .eq. 'MAX') then  
C       
C              Check for "=" separator. 
C       
               if (word(i+1) .ne. '=') then 
                  last = lastch (word(i))   
                  write (errbuf(1), 152) word(i)(1:last)
                  call prterx ('W', 1)  
               endif
               itdpmx = ftn_atof (word(i+2)(1:last))
            else if (word(i)(1:4) .eq. 'AREA') then 
C       
C              Check for "=" separator. 
C       
               if (word(i+1) .ne. '=') then 
                  last = lastch (word(i))   
                  write (errbuf(1), 152) word(i)(1:last)
                  call prterx ('W', 1)  
               endif
               do 192 nb = 1, ntot  
  192          ikk(1,nb) = 0
               do 198 j = i+2, nwrd 
                  do 196 k = 1, ntotc   
                     if (arcnam(k) .eq. word(j)) then   
                         do 194 nb = 1, ntot
                            if (jarzn(nb) .eq. k) then  
                               ikk(1,nb) = 1
                            endif   
  194                    continue   
                         go to 198  
                     endif  
  196             continue  
                  last = lastch (word(j))   
                  write (errbuf(1), 197 ) word(j)(1:last)   
  197             format('Interchange area (', a ,') is not in system')
                  call prterx ('W', 1)  
  198          continue 
               go to 230
            else if (word(i)(1:4) .eq. 'ZONE') then 
C       
C              Check for "=" separator. 
C       
               if (word(i+1) .ne. '=') then 
                  last = lastch (word(i))   
                  write (errbuf(1), 152) word(i)(1:last)
                  call prterx ('W', 1)  
               endif
        
               do 202 nb = 1, ntot  
  202          ikk(1,nb) = 0
               do 206 j = i+2, nwrd 
                  found = .false.   
                  do 204 nb = 1, ntot   
                     if (zone(nb) .eq. word(j)) then
                         ikk(1,nb) = 1  
                         found = .true. 
                     endif  
  204             continue  
                  if (.not.found) then  
                     last = lastch (word(j))
                     write (errbuf(1), 205) word(j)(1:last) 
  205                format('ZONE (',a,') is not in system')
                     call prterx ('W', 1)   
                  endif 
  206          continue 
               go to 230
            else
               last = lastch (word(i))  
               write (errbuf(1), 208) word(i)(1:last)   
  208          format('0 Undefined keyword (',a,')')
               write (errbuf(2), 210) buf(1:80)  
  210          format('GEN_DROP text:  (',a,')')
               call prterx ('E', 2)
               error = 1
            endif   
  220    continue   
  230    continue   
        
         found = .false.
  232    read (inp, 260, end=450) buf   
         card = buf(1:1)
        
         if (gensum_flag .eq. 1) then
           call space (1) 
           write (outbuf, 233 ) buf(1:80)  
  233      format (' GEN_DROP text (',a,')')  
           call prtout (1)
         endif

         if (card .eq. '.') then
            go to 232   
         else if (card .eq. 'B') then   
            found = .true.  
            read (buf, 234) bus1, base1 
  234       format (bz, t7, a8, f4.0) 
            nb = find_bus (bus1, base1)   
            if (nb .le. 0) then 
               write (errbuf(1), 236) bus1, base1
  236          format ('DROPPED_GEN (', a8, f6.1, ') is not in system.')
               call prterx ('E', 1)
               error = 1
            else
               call uscan(buf(20:),word,nwrd,'=',' ,()<>/\\')   
               do  i = 1, nwrd 
                 tempc = capital (word(i)) 
                 word(i) = tempc
               enddo
               pmax = busdta(7,nb)  
               kt = inp2opt(nb)   
               pgen = (pnetu(kt) + ploadu(kt)) * bmva  
               pmin = 0.0   
               do i = 1, nwrd, 3
                  last = lastch(word(i+2))
                  if (word(i) .eq. 'PMIN') then 
                     pmin = ftn_atof (word(i+2)(1:last))
                  else if (word(i) .eq. 'PMAX') then
                     pmax = ftn_atof (word(i+2)(1:last))
                     busdta(7,nb) = pmax
                  else if (word(i) .eq. 'PGEN') then
                     pgenew = ftn_atof (word(i+2)(1:last))
                     busdta(8,nb) = busdta(8,nb) - pgen + pgenew
                     pgen = pgenew  
                     pnetu(kt) = dble(pgen / bmva) - ploadu(kt) 
                  else if (word(i)(1:3) .eq. 'TOL') then
                     tol = ftn_atof (word(i+2)(1:last))
                     pmin = pgen - tol
                     pmax = pgen + tol
                     busdta(7,nb) = pmax
                  else
                     write (errbuf(1),10236) word(i)(1:last), buf(1:40)
10236                format ('Unrecognized keyword "', a, 
     &                  '" in GEN_DROP record "', a, '"')
                     call prterx ('E', 1)
                     error = 1
                  endif
               enddo
 
               if (ikk(1,nb) .eq. 0) then
                  write (errbuf(1),237) bus1, base1
  237             format ('DROPPED_GEN bus ', a8, f6.1,
     &                    ' does not reside with pickup generators.')
                  call prterx ('W', 1)
               endif
               ikk(1,nb) = 2
               if (numdrp .gt. 50) then 
                  write (errbuf(1),10237) 50, bus1, base1   
10237             format ('More than ',i3, 'DROPPED_GEN "B" records. ',
     &                    'Overflow occurred at (', a8, f6.1, ')')
                  call prterx ('E', 1)  
                  error = 1 
               else 
                  numdrp = numdrp + 1   
                  gndpno(numdrp) = nb   
                  pgen = pnetu(kt) + ploadu(kt)
                  gndpol(numdrp) = dble(pgen * bmva)
                  gndpmn(numdrp) = dble(pmin)
                  gndpmx(numdrp) = dble(pmax)
C       
C                 Flag generator if it is a system or area slack bus.   
C       
                  do 20237 i = 1, ntotc 
                     k = karea(1,i) 
                     if (ordtie .eq. 2) k = opt2inp(k)
                     if (k .eq. nb) then
                        if (iopton(17) .eq. 1) then 
                           gndpty(numdrp) = i   
                        else if (kbsdta(1,nb) .eq. 3) then  
                           gndpty(numdrp) = i   
                        else
                           gndpty(numdrp) = 0   
                        endif   
                        go to 20238 
                     endif  
20237             continue  
                  if (kbsdta(1,nb) .eq. 3) then 
                     gndpty(numdrp) = 1 
                  else  
                     gndpty(numdrp) = 0 
                  endif 
20238             continue  
                  dropmw = dropmw + pgen * bmva - pmax  
               endif
            endif   
            go to 232   
         else   
            if (numdrp .eq. 0 .and. drptot .eq. 0.0) then   
               write (errbuf(1), 240 )  
  240          format('No generation dropped.') 
               call prterx ('E', 1)
               error = 1
            endif   
            go to 262   
         endif  
      else  
         write (errbuf(1), 242) buf(1:80)   
  242    format('Unrecognizable GEN_DROP text (', a, ')')   
         call prterx ('E', 1)   
         error = 1  
      endif 
        
  250 read (inp, 260, end=450) buf  
  260 format (a)
      if (gensum_flag .eq. 1) then
        call space (1)
        write (outbuf,233 ) buf(1:80) 
        call prtout (1)   
      endif
  
  262 card = buf(1:1)   
        
  270 if (card .eq. '.') then   
          go to 250 
      else if (card .eq. '>') then  
         if (index(buf(1:10),'EXCLUDE') .ne. 0) then
C       
C        > EXCLUDE_BUSSES < 
C       
  272       read (inp, 260, end=450) buf
            card = buf(1:1) 
        
            if (gensum_flag .eq. 1) then
              call space (1)  
              write (outbuf,233 ) buf(1:80)   
              call prtout (1)
            endif 
        
            if (card .eq. '.') then 
               go to 272
            else if (card .eq. 'B') then
               read (buf, 280) bus1, base1  
  280          format (bz, t7, a8, f4.0)  
               nb = find_bus (bus1, base1)
               if (nb .le. 0) then
                  write (errbuf(1),290) bus1, base1
  290             format ('EXCLUDE_BUS (', a8, f6.1,
     &                    ') is not in system.')
                  call prterx ('W', 1)
                  go to 272
               else
                  do 294 i = 1, numdrp
                     if (gndpno(i) .eq. nb) then
                        write (errbuf(1),292) bus1, base1
  292                   format ('EXCLUDE_BUS (', a8, f6.1,
     &                          ') cannot be a dropped generator')
                        call prterx ('W', 1)
                        go to 296   
                     else   
                     endif  
  294             continue  
                  ikk(1,nb) = 0 
  296             continue  
                  go to 272 
               endif
            else
               go to 270
            endif   
         else   
            write (errbuf(1), 430) buf(1:20)
  430       format('Unrecognized /GEN_DROP command (', a,').')  
            call prterx ('W', 1)
         endif  
         go to 452  
      else  
         go to 452  
      endif 
        
  450 buf = '( END ) GETDRP'
      card = buf(1:1)   
C       
C     Generate list of allocable generators.
C       
  452 do 470 nb = 1, ntot   
         kt = inp2opt(nb) 
         if (ikk(1,nb) .eq. 1) then 
            pgen = pnetu(kt) + ploadu(kt)
            if (pgen .gt. 0.005) then   
               pmax = busdta(7,nb)  
               pmin = 0.0   
               if (pgen * bmva - 0.5 .le. pmax) then
                  if (pmax / bmva + 0.005 .ge. pgen .and.
     1                pmin / bmva - 0.005 .le. pgen) then
                     if (numgen .gt. MAXGENDRP) then
                        write (errbuf(1),30460) MAXGENDRP, bus1, base1
30460                   format ('More than ',i3, 'reallocatable ',
     &                          'generators. Overflow occurred at (',
     &                           a8, f6.1, ')')
                        call prterx ('E', 1)
                        error = 1   
                     else   
                        numgen = numgen + 1 
                        gennum(numgen) = nb 
                        genpol(numgen) = dble(pgen * bmva)
C       
C                       Flag generator if it is a system or area
C                       slack bus.  
C       
                        do 20460 i = 1, ntotc   
                           k = karea(1,i)   
                           if (ordtie .eq. 2) k = opt2inp(k)  
                           if (k .eq. nb) then  
                              if (iopton(17) .eq. 1) then   
                                 gentyp(numgen) = i 
                              else if (kbsdta(1,nb) .eq. 3) then
                                 gentyp(numgen) = i 
                              else  
                                 gentyp(numgen) = 0 
                              endif 
                              go to 20462   
                           endif
20460                   continue
                        if (kbsdta(1,nb) .eq. 3) then   
                           gentyp(numgen) = 1   
                        else
                           gentyp(numgen) = 0   
                        endif   
20462                   continue
                     endif
                  else
                     write (errbuf(1),462) pmin, pmax, bus(nb),
     1                  base(nb), pgen * bmva
  462                format ('Limits (', f6.0, ',', f6.0,
     &                       ') should reflect generation on ',
     &                       'allocatable machine ',
     &                        a8, f6.1, ' PGEN = (', f10.1, ')')
                     call prterx ('W', 1)
                     ikk(1,nb) = 0
                  endif
               else
               endif
            endif   
         endif  
  470 continue  
      if ((numdrp .eq. 0 .and. drptot .eq. 0.0) .or. numgen .eq. 0) 
     1   error = 1  
C       
C     Sort generator dropping arrays.   
C       
      if (error .eq. 0) then
         if (numdrp .gt. 1) call qiksrt (1, numdrp, kmpgd1, swpgd1) 
         if (numgen .gt. 1) call qiksrt (1, numgen, kmpgd2, swpgd2) 
      endif 
        
      return
      end   
