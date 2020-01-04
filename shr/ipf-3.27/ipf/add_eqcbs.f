C    @(#)add_eqcbs.f	20.6 7/18/96
C****************************************************************
C
C      File: add_eqcbs.f
C
C      Purpose: Integer function to add an equivalent continuation
c               bus to the network data base. 
C
C      Author: Walt Powell  Date: 14 December 1992
C      Called by: assequiv.f
C
C****************************************************************
C
       integer function add_eqcbs (k1, cbtype, cbowner, cbyear, ncb)
       character cbtype*1, cbowner*3, cbyear*2

       include 'ipfinc/parametr.inc'

       include 'ipfinc/blank.inc'
c	Global variables used:
c		ntot2
       include 'ipfinc/bus.inc'
c	Global variables used:
c		base, kbsdta
       include 'ipfinc/cbus.inc'
c	Global variables used:
c		kbctbl, bctbl_nxt
       include 'ipfinc/red7.inc'
c	Global variables used:
c		None
       include 'ipfinc/prt.inc'
c	Global variables used:
c		errbuf

       common /scratch/ nbr, array(2,100),
     &                  ndel1, lindel1(MAXBRN), ndel2,
     &                  lindel2(MAXBRN2), ndel3, cbdel(MAXBUS),
     &                  br_status(MAXBRN)
       integer array, cbdel, br_status

       integer pold, ptr, compare
       character cbtypex*1, cbyearx*2, cbownerx*3, code*4, basec*4
       logical finished

       add_eqcbs = 0

       ncb = 0
       if (ndel3 .eq. 0) then 
          ntot2 = ntot2 + 1
          ncb = ntot2
       else  
          do i=1, ndel3
             if (cbdel(i) .gt. 0) then  
                ncb = cbdel(i)  
                cbdel(i) = 0 
                go to 130
             endif   
          enddo
          ntot2 = ntot2 + 1 
          ncb = ntot2
  130     continue
       endif 
       kbctbl(1,ncb) = k1
       call putchr(1, cbtype, kbctbl(8,ncb))
       call putchr(3, cbowner, kbctbl(10,ncb))
       call putchr(2, cbyear, kbctbl(9,ncb))
       do i = 2, 7
          kbctbl(i,ncb)=0  
       enddo
       kbctbl(11,ncb)=0  
       kbctbl(12,ncb)=0  
c
c      Connect to +bus linked pointers
c
       pold = 0
       icb = 0
       ptr = kbsdta(15,k1)  
       finished = .false.
       do while (ptr .gt. 0 .and. .not. finished)
          call getchr (1, cbtypex, kbctbl(8,ptr))
          call getchr (3, cbownerx, kbctbl(10,ptr))
          call getchr (2, cbyearx, kbctbl(9,ptr)) 
          compare = kompr (cbtypex // cbownerx // cbyearx, 
     &                     cbtype // cbowner // cbyear, junk)
          if (compare .lt. 0) then
c
c            Insert current entity between pold and ptr
c
             bctbl_nxt(ncb) = ptr
             if (pold .eq. 0) then
                kbsdta(15,k1) = ncb
             else
                bctbl_nxt(pold) = ncb
             endif
             finished = .true.
          else if (compare .eq. 0) then
             basec = code (base(k1), 4, 0)
             write (errbuf(1), 140) cbtype, cbowner, bus(k1), basec,
     &          cbyear
  140        format( ' +Bus already in system, record (+', a, 1x, a,
     &               a, a, a, ')')
             call prterx ('F',1)
             add_eqcbs = 1
             go to 9000
          else
             pold = ptr
             ptr = bctbl_nxt(ptr)
          endif
       enddo
       if (.not. finished) then
          bctbl_nxt(ncb) = ptr
          if (pold .eq. 0) then
             kbsdta(15,k1) = ncb
          else
             bctbl_nxt(pold) = ncb
          endif
       endif
 9000  continue
       return
       end
