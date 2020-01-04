C    @(#)writxt.f	20.4 8/20/98
        subroutine writxt
 
      	include 'ipfinc/parametr.inc'

      	include 'ipfinc/arcntl.inc'
c	Global variables used:
c		arcbus, arcbas, ntotic, arcint
      	include 'ipfinc/blank.inc'
c	Global variables used:
c		srtlst
      	include 'ipfinc/branch.inc'
c	Global variables used:
c		brtype, brsect, brnch_ptr, brnch, brnch_nxt, brid
      	include 'ipfinc/bus.inc'
c	Global variables used:
c		bus, kbsdta, owner, base, alf2inp, inp2alf
      	include 'ipfinc/cbus.inc'
c	Global variables used:
c		bctbl_nxt
      	include 'ipfinc/ikk.inc'
c	Global variables used:
c		ikk
      	include 'ipfinc/lfiles.inc'
c	Global variables used:
c		dbug
      	include 'ipfinc/merge.inc'
c	Global variables used:
c		comvar, comcha, itname, rename, renamc, irname,
c		
      	include 'ipfinc/mrgsys.inc'
c	Global variables used:
c		
      	include 'ipfinc/prt.inc'
c	Global variables used:
c		
      	include 'ipfinc/xdata.inc'
c	Global variables used:
C
        character xbuf*130
        integer ptr, status, rename_bus

c       Store X-data pointer in busxdtptr()
c
        if (.not. xdt_flag) then
           do nb = 1, ntot
              busxdtptr(nb)  = 0
           enddo
           do i = 1, kxtot
              kxd = xdata(1,i)
              if (kxd .gt. 0) busxdtptr(kxd) = i
           enddo
           xdt_flag = .true.
        endif
C
C      	RENAME BUSES
C
      	do i=1,itname
           j=irname(i)
           status = rename_bus (j, renamc(i)(1:8), rename(i))
           if (status .eq. 1) then
              write (errbuf(1), 80) j, ntot
   80         format (' Illegal index used in RENAME_BUS :',
     &           i6, ' ntot ', i6)
              call prterx ('W',1)
           else if (status .eq. 2) then
              write (errbuf(1), 82) bus(j), base(j), renamc(i)(1:8),
     &           rename(i)
   82         format (' Duplicate bus used in RENAME_BUS : old name :', 
     &           a6, f6.1, ' new name :', a8, f6.1)
              call prterx ('W',1)
           endif
        enddo
C
C       SAVE AREA INTERCHANGE SYSTEM IF REQUESTED
C
        if (comvar(9).eq.0 ) go to 118
        if (ntotc.eq.0) go to 118
        do 116 i = 1,ntotc
          if (itname.eq.0) go to 94
          do 90 j = 1,itname
             kt = irname(j)
             if (bus(kt).ne.arcbus(i)) go to 90
             if (base(kt).eq.arcbas(i)) go to 92
   90     continue
          go to 94
 
   92     arcbus(i)=renamc(j)
          arcbas(i)=rename(j)
   94     continue
          call bcdarc(i,xbuf)
          call bustxt (xbuf)
          write (outbuf,115) xbuf(1:120)
  115     format (2x,'(',a,')')
          call prtout(1)
          j = 1
          do while ( j .lt. MAXCAZ/MAXCAZR   .and. 
     &             ( arczns(j*MAXCAZR+1,i) .ne. ' ' ) )
            call bcdarc2 (i, j, xbuf)
            call bustxt (xbuf)
            write (outbuf,115) xbuf(1:120)
            call prtout(1)
            j = j + 1
          enddo
  116   continue
C
C       SAVE AREA INTERCHANGE "I" RECORDS
C
        do 117 i = 1,ntotic
           if (kompr (arcint(1,i),arcint(2,i),junk) .lt. 0) then
              call bcdari(i,xbuf)
              call bustxt(xbuf)
              write (outbuf,115) xbuf(1:120)
              call prtout (1)
           endif
  117   continue
  118   continue
C
C       PROCESS SELECTED BUS DATA
C
        jtot=0
        do 570 nb=1,ntot
           if(ikk(1,nb).le.0) go to 570
           call bcdbus (nb,xbuf)
           call bustxt (xbuf)
C
C          PROCESS "+" BUS DATA
C
           ncb=kbsdta(15,nb)
           do while (ncb .gt. 0)
              call bcdcbs(ncb,xbuf)
              call bustxt (xbuf)
              ncb = bctbl_nxt(ncb)
           enddo
C
C          PROCESS "X" BUS DATA
C
           if (kbsdta(1,nb) .eq. 11) then
              kxd = busxdtptr(nb)
              if (kxd .gt. 0) then
                 call bcdxdt(kxd,xbuf)
                 call bustxt (xbuf)
              endif
           endif
C
C          PROCESS BRANCH DATA
C
           ptr = kbsdta(16,nb)
           do while (ptr .gt. 0)
              k2 = ky(ptr)
              if (brtype(ptr) .eq. 1) then
              else if (k2 .gt. ntot) then
              else 
                 if (ikk(1,k2) .eq. 1) then
C
C                Branch is internal. Punch in order of original 
C                submission (BRNCH_PTR(PTR) > 0) so that sections 
C                are properly orientated.
C
                    i = kbsdta(16,k2)
                    do while (i .gt. 0)
                       if (ky(i) .eq. nb .and.
     1                     brid(i) .eq. brid(ptr) .and.
     2                     brsect(i) .eq. brsect(ptr) .and.
     3                     brtype(i)  .eq. brtype(ptr)) then
                          if (isign(1, brnch_ptr(i)) .ne. 
     1                        isign(1, brnch_ptr(ptr))) then
                             if (brnch_ptr(ptr) .gt. 0) then
                                call bcdbrn(ptr,xbuf)
                                call brntxt (xbuf)
                             endif
                          else if (inp2alf(nb) .lt. inp2alf(k2)) then
                             call bcdbrn(ptr,xbuf)
                             call brntxt (xbuf)
                             write (errbuf(1),358)
  358                        format (' (WRITXT) Branch transpose flag no
     1t set ')
                             write (errbuf(2),359) xbuf(1:120)
  359                        format (' (',a,')')
                             call prterx ('W',2)
                          endif
                          go to 362
                       endif
                       i = brnch_nxt(i)
                    enddo
                    call erexit
  362               continue
                 else if (ikk(1,k2) .eq. 0) then
                    call bcdbrn(ptr,xbuf)
                    if (brtype(ptr) .eq. 7) then
                       nbr = iabs(brnch_ptr(ptr))
                       if (brnch(8,nbr) .lt. 0.0) then
                          ktrpos=1
                          write (xbuf(121:130),370) ikk(3,nb),ktrpos,
     1                       owner(k2),owner(nb),comvar(1)
  370                     format(2i1,2a3,i2)
                       else
                          ktrpos=0
                          write (xbuf(121:130),370) ikk(3,nb),ktrpos,
     1                       owner(nb),owner(k2),comvar(1)
                       endif
                    else
                       ktrpos=0
                       write (xbuf(121:130),370) ikk(3,nb),ktrpos,
     1                    owner(nb),owner(k2),comvar(1)
                    endif
                    call facetx (xbuf)
                 endif
              endif
              ptr = brnch_nxt(ptr)
           enddo
  570   continue
C
C       Rename deleted buses and obtain new bus sort array of subsystem
C
        jtot = 0
        do i = 1,ntot
           if (ikk(1,i).eq.1) then
              jtot = jtot + 1
           else
              status = rename_bus (i, srtlst, 9999.0)
              if (status .eq. 1) then
                 write (errbuf(1), 80) j, ntot
                 call prterx ('W',1)
              endif
           endif
        enddo
        call sortbus

        if(nbsys1.eq.0) then
           nbsys1=jtot
           ix=0
        else
           nbsys2=jtot+nbsys1
           ix=nbsys1
           endif
        do i=1,jtot
           j = alf2inp(i)
           mrgbus(i+ix)=bus(j)
           mrgbas(i+ix)=base(j)
        enddo
        if (jtot.eq.0) go to 640
        if (comcha(10) .ne. ' ') then
           i = comvar(1)
           write (dbug,590) jtot,comcha(i+1)
  590      format ('0 THE FOLLOWING ',i4,' BUSES COMPRISE SUBSYSTEM ',
     &        a10,//)
 
           do i = 1,jtot,8
              nend = min0 (jtot-i+1,8)
              write (dbug,620) (bus(i+j-1),base(i+j-1),j=1,nend)
  620         format (8(2x,a8,f6.1))
           enddo
        endif
  640   continue
        return
        end
