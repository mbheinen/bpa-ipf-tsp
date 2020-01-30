C    %W% %G%
      subroutine rebldbse
c
c     This subroutine rebuilds the base case in residence, purging
c     deleted records, and resorting all data.
c
      include 'ipfinc/parametr.inc'

      include 'ipfinc/arcntl.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/arsort.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/bushasht.inc'
      include 'ipfinc/cbsorc.inc'
      include 'ipfinc/cbsort.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/filnam.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/mrgtxt.inc'
      include 'ipfinc/oldsrt.inc'
      include 'ipfinc/oldtbx.inc'
      include 'ipfinc/ordsta.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/tempbsnm.inc'
      include 'ipfinc/xdata.inc'
      include 'ipfinc/xsrt.inc'

      common /scratch/ nbr, array(2, MAXBRN), temp_array(2*MAXBRN)
      integer array, temp_array

      common /is_batch / is_batch

      character xbuf*120, id*1, busn*8, subtyp*1, zn*2
      logical dupsw, done
      integer komp_bus, komp_br2, komp_cb2, find_bus, sect,
     &        kpoldt, kmparc, pold, bptr1, bptr2, error,
     &        kmpari, p, h, bus_hash, shift, ptr, find_zon
      external komp_bus, swap_bus, swap_cb2, komp_cb2,
     &         komp_br2, swap_br2, kpoldt, spoldt,
     &         kmparc, swparc, swpari, kmpari, kmpzone, swpzon

c     kspare(1) = 0 : no base case data present
c                 1 : base case data present-rebuild data
c                 2 : base case data present-do not rebuild data
c
c     Convert to input order if in optimal order.
c
      if (ordvlt .eq. 2) then
         ordvlt = 1
         call mvnew1d(e,opt2inp,ntot)
         call mvnew1d(f,opt2inp,ntot)
      endif
      if (ordcap .eq. 2) then
         ordcap = 1
         call mvnew2d(capcor,opt2inp,ntot)
      endif

      do ix = 1, ntot_alf

         i = alf2inp(ix)
         ktype = kbsdta(1,i)
         if (ktype .eq. 5 .or. ktype .eq. 12) then
            j = 9
         else if (ktype .eq. 8 .or. ktype .eq. 11) then 
            j = 13
         else
            goto 120
         endif
         k = kbsdta(j,i)
         if (k.eq.0) go to 120
         ibuscb=ibuscb + 1
         if (ibuscb.gt.MXBSTP) then
            write (errbuf(1), 100) MXBSTP
  100       format(' Array over flow, more than ',i4,
     1             ' temporary bus names.')
            errbuf(2) = ' '
            call bcdbus(i,xbuf)
            write (errbuf(3), 110) xbuf(1:80)
  110       format(2x,a80)
            if (is_batch .eq. 0) then
               call prterx ('E',3)
            else
               call prterx ('F',3)
            endif
            ibuscb=1
         endif
         itype(ibuscb) = 1
         ixndx(ibuscb) = i
         buscb(ibuscb)=bus(k)
         basecb(ibuscb)=base(k)
         kbsdta(j,i)=-ibuscb
  120    continue
      enddo
c
c     Reinitialize XDATA
c
      do 130 i = 1, kxtot

         nb = xdata(1,i)
         ktype = kbsdta(1,i)
         if (ktype .eq. 11) go to 130
         capcor(2,nb) = (xdata(3,i) + xdata(4,i)) / bmva
         capcor(1,nb) = capcor(1,nb)
     1                - (xdata(5,i) - xdata(6,i)) / bmva
         xdata(5,i) = xdata(3,i)
         xdata(6,i) = xdata(4,i)

         m = xdata(2,i)
         if (m .gt. 0) then
            ibuscb = ibuscb + 1
            if (ibuscb .gt. MXBSTP) then
               write (errbuf(1), 100) MXBSTP
               errbuf(2) = ' '
               write (errbuf(3), 110) xbuf(1:80)
               call bcdbus(i,xbuf)
               if (is_batch .eq. 0) then
                  call prterx ('E',3)
               else
                  call prterx ('F',3)
               endif
               ibuscb=1
            else
               buscb(ibuscb)=bus(m)
               basecb(ibuscb)=base(m)
               xdata(2,i)=-ibuscb
               itype(ibuscb) = 3
               ixndx(ibuscb) = i
            endif
         endif
  130 continue

      if (kspare(1) .ne. 1) go to 900
c
c     Touch all valid entities of kx(), ky() (those which can be
c     reached by existing bus/branch pointers, and nullify the remaining
c     brnach entities.
c
      do i = 1, ltot2
         temp_array(i) = 0
      enddo

      do nb = 1, ntot
         ptr = kbsdta(16,nb)
         do while (ptr .gt. 0)
            temp_array(ptr) = nb
            ptr = brnch_nxt(ptr)
         enddo
      enddo

      do i = 1, ltot2
         if (temp_array(i) .eq. 0) then
            kx(i) = 19999       
            ky(i) = 19999       
            brnch_ptr(i) = 0
         endif
      enddo
c
c     Sort bus data
c
      do i = 1, ntot
         kbsdta(15,i) = 0    ! Nullify continuation bus pointer
         kbsdta(16,i) = 0    ! Nullify branch pointer
         keysrt(i) = i
      enddo

      ntot_old = ntot
      dupsw = .false.
      call qiksrt(1, ntot, komp_bus, swap_bus)

c     Truncate bus counter to eliminate deleted buses

      done = .false.
      do while (ntot .gt. 0 .and. .not. done )
         if ( bus(ntot) .eq. srtlst ) then
            ntot = ntot - 1
         else
            done = .true.
         endif
      enddo

      if (dupsw) then
c
c        Flag and remove duplicate buses...
c
         j = 1
         k = 2

         do while ( k .le. ntot ) 
            if ( komp_bus(j,k) .lt. 0 ) then
               j = j + 1
               if (j.lt.k) call swap_bus(j,k)
               k = k + 1
            else if (bus(j) .ne. srtlst) then
               write (errbuf(1), 140)
  140          format('Duplicate bus records ...')
               if (keysrt(j) .gt. keysrt(k)) call swap_bus(j,k)
               call bcdbus(j, xbuf)
               write (errbuf(2), 150) xbuf(1:80)
  150          format(' kept---- ',a )
               call bcdbus(k, xbuf)
               write (errbuf(3), 160) xbuf(1:80)
  160          format(' deleted- ',a )
               call prterx ('W',3)
               k = k + 1
            else
               k = k + 1
            endif
         enddo
         ntot = j
      endif

      do new = 1,ntot_old
         jold = keysrt(new)
         kbsort(jold) = new
      enddo
c
c     Update temporary bus name array
c
      do i = 1, ibuscb
         if (itype(i) .eq. 1) then
            j = ixndx(i)
            ixndx(i) = kbsort(j)
         endif
      enddo
c
c     Rebuild bus hash arrays from scratch
c
      call bushinit()
      call tbxhinit()

      do i = 1, ntot
         h = bus_hash (bus(i), base(i))
         p = htable_b(h)
         do while (p .gt. 0)         ! Position to end of linked list
            p = nextptr_b(p)
         enddo
         nextptr_b(i) = htable_b(h)
         htable_b(h) = i             ! Inserts new bus at top of stack
      enddo

      kbsort(ntot_old+1) = 19999

      do i = 1, ntot

         inp2alf(i) = i
         alf2inp(i) = i

         ktype = kbsdta(1,i)
         j = 0
         if (ktype .eq. 5 .or. ktype .eq. 12) j = 9
         if (ktype .eq. 8 .or. ktype .eq. 11) j = 13
         if (j .gt. 0) then
            jk = kbsdta(j,i)
            if (jk .gt. 0) then
               kbsdta(j,i) = kbsort(jk)
            else if (jk .lt. 0) then
               jk = -jk
               itype(jk) = 0
               ixndx(jk) = 0
               new = find_bus (buscb(jk), basecb(jk))
               if ( new .le. 0 ) then
                  new = 19999
                  write (errbuf(1), 162) bus(i), base(i), buscb(jk),
     &                basecb(jk)
  162             format('Bus ', a8, f6.1, ' refers to ',a8, f6.1,
     1             ' which is not in system')
                  call prterx ('W',1)
               else
                  kbsdta(j,i) = new
               endif
            endif
         endif
      enddo
c
c     Sort the continuation data
c
      if ( ntot2. gt. 0 )  then

         do 190 jold = 1,ntot2
            iold = kbctbl(1,jold)
            if ( iold .gt. 0 ) then
               new = kbsort( iold )
               if ( new.gt.ntot ) new = 19999
            else if (iold .lt. 0) then
               new = find_bus(cbname(jold),cbbase(jold))
               if ( new .le. 0 ) then
                  new = 19999
                  write (errbuf(1), 170) cbname(jold),cbbase(jold)
  170             format('Continuation bus ',a8,f7.1,
     1            ' has no corresponding bus record - skipped')
                  call prterx ('W',1)
               endif
            else 
              new = 19999
            endif
c
            kbctbl(1,jold) = new
            if (new .eq. 19999) then
               busn=srtlst
               basen=100.0
            else
               busn=bus(new)
               basen=base(new)
            endif
            write (cbkey(jold), 180) busn, basen, bctbl(8,jold), 
     1         bctbl(10,jold),bctbl(9,jold)
  180       format (a8,f6.1,a1,a3,a2)

  190    continue

         dupsw = .false.
         call qiksrt(1, ntot2, komp_cb2,swap_cb2)
c
c        Trnucate counter at any delete continuation buses

         done = .false.
         do while ( ntot2 .gt. 0 .and. .not. done )
            if ( kbctbl(1,ntot2) .eq. 19999 ) then
               ntot2 = ntot2 - 1
            else
               done = .true.
            endif
         enddo

         if ( dupsw ) then
c
c           Flag and remove duplicate continuation buses..
c
            j = 1
            k = 2

            do while (k .le. ntot2)
               if( komcbs(j,k) .lt. 0 ) then
                  j = j + 1
                  if (j .lt. k) call swpcbs(j,k)
                  k = k + 1
               else
                  write (errbuf(1), 200)
  200             format('Duplicate continuation bus records ...')
                  call bcdcbs(j, xbuf)
                  write (errbuf(2), 150) xbuf(1:80)
                  call bcdcbs(k, xbuf)
                  write (errbuf(3), 160) xbuf(1:80)
                  call prterx ('W',3)
                  k = k + 1
               endif
            enddo
            ntot2 = j
         endif
c
c        Link continuation buses up with bus data
c
         iold = 0
         pold = 0
         do 210 i = 1, ntot2
            nb = kbctbl(1,i)
            if (nb .eq. iold) then
               bctbl_nxt(pold) = i
            else
               kbsdta(15,nb) = i
               iold = nb
            endif
            pold = i
            bctbl_nxt(i) = 0                                             
  210    continue

      endif
c
c        Sort the switched X data
c
         if ( kxtot .gt. 0 )  then
         do 230 k = 1, kxtot

c           local bus name (num)

            jk = xdata(1,k)
            if ( jk.gt.0 ) then
               new = kbsort(jk)
               if ( new .gt. ntot ) new = 19999
            else
               jk = -jk
               itype(jk) = 0
               ixndx(jk) = 0
               new = find_bus(buscb(jk),basecb(jk))
               if ( new .le. 0 ) then
                  new = 19999
                  write (errbuf(1), 220) buscb(jk),basecb(jk)
  220             format('X-bus data refers to ',a8,f7.1,
     1            ' which is not in system - skipped')
                  call prterx ('W',1)
               endif
            endif
c
            if ( new .ne. 19999 ) then
 
c              check remote bus name (num)
 
               jk = xdata(2,k)
               if (jk .gt. 0) then
                  jnew = kbsort(jk)
                  if (jnew .gt. ntot) jnew=19999
               else if (jk .lt. 0) then
                  jk = -jk
                  itype(jk) = 0
                  ixndx(jk) = 0
                  jnew = find_bus (buscb(jk),basecb(jk))
                  if ( jnew .le. 0 ) then
                     new = 19999
                     jnew = 19999
                     write (errbuf(1), 220) buscb(jk),basecb(jk)
                     call prterx ('W',1)
                  endif
               else
                  jnew = new
               endif
c
               xdata(2,k) = jnew
c
            else
               jk = xdata(2,k)
               if (jk .lt. 0) then
                  jk = -jk
                  itype(jk) = 0
                  ixndx(jk) = 0
               endif
            endif
            xdata(1,k) = new
c
  230    continue
c
         call sort_xdta()
 
      endif
c
c     Sort the original TBX array
c
      if ( numtbx .gt. 0 )  then
         do 260 k = 1, numtbx
c
c           Local bus name (NUM)
c
            jk = oldtbx(2,k)
            if ( jk.gt.0 ) then
               new = kbsort(jk)
               if ( new.gt.ntot ) new = 19999
            else
               new = 19999
            endif
c
            if ( oldtbx(1,k) .ne. 3 .and. oldtbx(1,k) .ne. 5) then
               if ( new .ne. 19999 ) then
 
c                 Check remote bus name (NUM)
 
                  jk = oldtbx(8,k)
                  if (jk .gt. 0) then
                     jnew=kbsort(jk)
                     if (jnew.gt.ntot) jnew=19999
                  else
                     jnew = 0
                  endif
                  oldtbx(8,k) = jnew
               else
                  oldtbx(8,k) = 0
               endif
            endif
            oldtbx(2,k) = new
  260       continue
c
            dupsw = .false.
            call qiksrt(1, numtbx, kpoldt, spoldt)
c
            if ( dupsw .or. (oldtbx(2,numtbx) .eq. 19999)) then
 
c              Find OLDTBX buses to be deleted.
 
               done = .false.
               do while ( numtbx .gt. 0 .and.  .not. done )
                  if ( oldtbx(2,numtbx) .eq. 19999 ) then
                     numtbx = numtbx - 1
                  else
                     done = .true.
                  endif
               enddo
c
c              Remove duplicate OLDTBX buses.
c
               j = 1
               k = 2
c
               do while (k .le. numtbx) 
                  if ( kpoldt(j,k) .lt. 0 ) then
                     j = j + 1
                     if (j.lt.k) call spoldt(j,k)
                     k = k + 1
                  else
                     k = k + 1
                  endif
               enddo
               numtbx = j
            endif
         endif
c
c        Sort area interchange "AC" records
c
         if (ntotc.gt.0) then
 
            dupsw = .false.
            call qiksrt (1, ntotc, kmparc, swparc)
            if (dupsw) then
c
c              Flag and remove duplicate records
c
               j = 1
               k = 2
               do while (k .le. ntotc) 
                  if ( kmparc(j,k) .lt. 0 ) then
                     j = j + 1
                     if (j .lt. k) call swparc(j,k)
                     k = k + 1
 
                  else
 
                     write (errbuf(1), 270)
  270                format('duplicate area interchange records')
                     call bcdarc (j,xbuf)
                     write (errbuf(2), 150) xbuf(1:80)
                     call bcdarc (k,xbuf)
                     write (errbuf(3), 160) xbuf(1:80)
                     call prterx ('W',3)
                     k = k + 1
                  endif
               enddo
               ntotc = j
 
            endif
         endif
c
c        Rebuild jarzn array
c
         nztot = 0
         do ix = 1, ntot_alf
           i = alf2inp(ix)
           zn = zone(i)         
           do j = 1,nztot   
             if (acznam(j) .eq. zn) go to 272  
           enddo
           nztot = nztot + 1                        
           if (nztot .gt. MAXCZN) then 
             write (errbuf(1), 10272) MAXCZN      
10272        format(' More than ',i4, 
     1          ' unique zone names. Area control aborted.') 
             call prterx ('W',1)               
             jtie = 0                            
             ntotc = 0
             go to 277
           endif 
           acznam(nztot) = zn                     
           acznum(nztot) = 0
  272      continue
         enddo
                                               
         if (nztot .gt. 1) then 
           call qiksrt(1, nztot, kmpzone, swpzon) 
         endif 

         do i = 1, ntotc

c          Obtain count NUMXZN of zones for this area.            
                                                                
           do j = 1, MAXCAZ                                   
             zn = arczns(j,i)                                    
             if (zn .eq. '  ' .and. j .gt. 1) then               
               numxzn = j - 1                                   
               go to 274
             endif                                               
           enddo
           numxzn = MAXCAZ                                        
  274      continue

c          Assign area number "i" to each zone.                        

           do j = 1, numxzn                                  
             zn = arczns(j,i)                                   
             k = find_zon(zn) 
             if (k .le. 0) then                                 
               write (errbuf(1), 10276) zn, arcnam(i)              
10276          format(' Zone "',a2,'" in controlled area ',a10, 
     &                ' is not in system. Zone ignored.')  
               call bcdarc (i,xbuf)                      
               errbuf(2) = ' '                             
               write (errbuf(3),10277) xbuf(1:80)            
10277          format(2x,'(', a, ')') 
               call prterx ('I',3)                         
             else
               acznum(k) = i                                    
             endif
           enddo
         enddo

c        Find area number of each bus                                     
                                                                         
         do nbx = 1, ntot_alf                                                  
           nb = alf2inp(nbx)
           zn = zone(nb)                                        
           jarzn(nb) = 0                                       
           do j = 1,nztot                                 
             if (zn .eq. acznam(j)) go to 276
           enddo
           write (errbuf(1), 10278) zn, bus(nb), base(nb)       
10278      format(' Zone "', a2, '", bus ', a8, f6.1, 
     &            ' is not specified in any controlled area. ', 
     &            'Interchange control aborted.') 
           if (is_batch .eq. 0) then
             call prterx ('E',1)
           else
             call prterx ('F',1)
           endif
           jtie = 0
           go to 277

  276      k = acznum(j)                                    
           if (k .eq. 0) then 
             write (errbuf(1), 10279) zn, bus(nb), base(nb)       
10279        format(' Zone "', a2, '", bus ', a8, f6.1, 
     &              ' is not specified in any controlled area. ', 
     &              'Interchange control aborted.') 
             if (is_batch .eq. 0) then
               call prterx ('E',1)
             else
               call prterx ('F',1)
             endif
             go to 277
           else 
             jarzn(nb) = k                                  
           endif 
         enddo
  277    continue
c
c        Sort area intertie "I" records
c
         if (ntotic.gt.0) then
            dupsw = .false.
            call qiksrt (1,ntotic,kmpari,swpari)
            if (dupsw) then
c
c              Remove duplicate records
c
               ntotxx = ntotic
               do 330 i = 2,ntotxx
                  if (i .gt. ntotic) go to 340
                  if (arcint(1,i) .eq. arcint(1,i-1) .and.
     1                arcint(2,i) .eq. arcint(2,i-1)) then
c
c                    Duplicate found. If low-high alpha, delete second 
c                    entity. If high-low alpha, delete corresponding 
c                    transposed duplicate entity.
c
                     if (kompr(arcint(1,i),arcint(2,i),junk).lt.0) then
                        ikeep = i
                        idelet = i-1
                     else
                        do 280 j=1,i-1
                           if (arcint(1,j) .eq. arcint(2,i) .and.
     1                        arcint(2,j) .eq. arcint(1,i)) then
                              if (arcinp(j).eq.-arcinp(i)) then
                                 ikeep = i
                                 idelet = i-1
                              else
                                 ikeep = i-1
                                 idelet = i
                              endif
                              go to 300
                           endif
  280                   continue
                        write (errbuf(1), 290)
  290                   format ('Transpose of area intertie "I" record i
     &s not in system.')
                        call bcdari (i,xbuf)
                        write (errbuf(2), 150) xbuf(1:80)
                        if (is_batch .eq. 0) then
                           call prterx ('E',2)
                        else
                           call prterx ('F',2)
                        endif
                        ikeep = i-1
                        idelet = i
                     endif
  300                write (errbuf(1), 310)
  310                format ('Duplicate area intertie "I" records. Secon
     &d one deleted.')
                     call bcdari (ikeep,xbuf)
                     write(errbuf(2), 150) xbuf(1:80)
                     call bcdari(idelet,xbuf)
                     write (errbuf(3), 160)xbuf(1:80)
                     call prterx ('W',3)
                     if (ikeep.lt.idelet) then
                        istart = ikeep
                     else
                        istart = ikeep + 2
                     endif
                     do 320 j=istart,ntotic
                        arcint(1,j-1) = arcint(1,j)
                        arcint(2,j-1) = arcint(2,j)
                        arcinp(j-1) = arcinp(j)
  320                continue
                     ntotic = ntotic - 1
                  endif
  330          continue
  340          continue
            endif
        endif
c
c       Convert, sort, and relink all branch data in residence    
c       Note: KBRNCH is incomplete to reconstruct the sorting 
c       criteria.  We must work from kx(), ky(), etc.

        ix = ipack_2 (19999, 19999)
        do i = 1, ltot
           array(1,i) = ix
           array(2,i) = ix
        enddo

        do 350 i = 1, ltot2 
           indx = brnch_ptr(i)  
           if (indx .gt. 0) then 
              k1 = kx(i) 
              k2 = ky(i) 
              if (k1 .ne. 19999 .and. k2 .ne. 19999) then 
                 k1 = kbsort(k1) 
                 k2 = kbsort(k2) 
                 if (brtype(i) .eq. 4) then                
c
c                   Change all "R ", "RV", and "RO" data to "R "
c
                    call getchr (1, subtyp, kbrnch(3,indx))  
                    if (index (' VO',subtyp) .ne. 0) then  
                       if (subtyp .eq. 'V') then           
                          subtyp= ' '                      
                          call putchr(1, subtyp, kbrnch(3,indx))    
                       endif                               
                       kc = kbrnch(4,indx)          
                       if (kc .gt. 0) then                 
                          if (kc .ne. 19999) kc = kbsort(kc) 
                          kbrnch(4,indx) = kc         
                       endif                                
                    else                                    
                       kbrnch(4,indx) = 0     
                    endif                                   
                                                                        
                 else if (brtype(i) .eq. 1) then            
                                                                         
                    do j = 4,11        
                       brnch(j,indx) = 0.0                     
                    enddo 
                                                                        
                 endif                                      
                                                                        
c                Test for deleted items                     
                                                                        
                 if (max0 (k1,k2) .gt. ntot) then             
                    k1 = 19999
                    k2 = 19999
                 endif 
              endif 
              array(1,indx) = ipack_2 (k1, k2)
              array(2,indx) = ipack_4 (0, ichar(brid(i)), brsect(i), 
     &                                 brtype(i))
           endif
  350   continue
        
        dupsw = .false.
        call qiksrt(1, ltot, komp_br2, swap_br2)
c
c       Truncate counter at deleted branches
c
        done = .false.
        do while ( ltot .gt. 0  .and.  .not. done )
           if ( shift(array(1,ltot),-16) .eq. 19999 ) then
              ltot = ltot - 1
           else
              done = .true.
           endif
        enddo

        if ( dupsw ) then

c          Flag and remove duplicate branches
c
           j = 1
           k = 2

           do while (k .le. ltot) 
              if ( komp_br2(j,k) .lt. 0 ) then
                 j = j + 1
                 if (j.lt.k) call swap_br2(j,k)
                 k = k + 1
              else
                 write (errbuf(1), 360)
  360            format('Duplicate branch bus records ...')
                 if (kbsort(j) .gt. kbsort(k)) call swap_br2(j,k)
c
c                Set up temporary pointers to enable bcdbrn
c
                 kx(1) = shift(array(1,j), -16)
                 ky(1) = shift(shift(array(1,j), 16), -16)
                 brid(1) = char(shift(shift(array(2,j), 16), -24))
                 brsect(1) = shift(shift(array(2,j), 24), -28)
                 brtype(1) = shift(shift(array(2,j), 28), -28)
                 brnch_ptr(1) = j
                 call bcdbrn(1, xbuf)
                 write (errbuf(2), 150) xbuf(1:80)
c
c                Set up temporary pointers to enable bcdbrn
c
                 kx(1) = shift(array(1,k), -16)
                 ky(1) = shift(shift(array(1,k), 16), -16)
                 brid(1) = char(shift(shift(array(2,k), 16), -24))
                 brsect(1) = shift(shift(array(2,k), 24), -28)
                 brtype(1) = shift(shift(array(2,k), 28), -28)
                 brnch_ptr(1) = k
                 call bcdxdt(1,xbuf)
                 write (errbuf(3), 160) xbuf(1:80)
                 call prterx ('W',3)
                 k = k + 1
              endif
           enddo
           ltot = j
        endif
c
c       Link branch data up with bus data
c
        ltot2 = 0
        do i = 1, ltot

           k1 = shift(array(1,i), -16)
           k2 = shift(shift(array(1,i), 16), -16)
           id = char(shift(shift(array(2,i), 16), -24))
           sect = shift(shift(array(2,i), 24), -28)
           lntype = shift(shift(array(2,i), 28), -28)

           call lkbrdata (i, k1, k2, bptr1, bptr2, error)

           kx(bptr1) = k1                 
           ky(bptr1) = k2
           brid(bptr1) = id
           brsect(bptr1) = sect
           brtype(bptr1) = lntype

           kx(bptr2) = k2                 
           ky(bptr2) = k1
           brid(bptr2) = id
           brsect(bptr2) = sect
           brtype(bptr2) = lntype

        enddo

        call srtbrnch()
        kspare(1) = 1        ! Case rebuilt ! 
        knew = 1             ! (but y-matrix data must be rebuilt
C                            ! from scratch)
  900   continue
        return
        end
