C    @(#)bld2dc.f	20.7 7/18/96
        subroutine bld2dc(nb,kerr)
 
c       This subroutine generates entities into the dc2t array as
c       defined by bus and branch data.
 
 
        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
c	Global variables used:
c		kdtot
        include 'ipfinc/branch.inc'
c	Global variables used:
c		kx, ky, brsect, brnch_ptr, brnch
        include 'ipfinc/bus.inc'
c	Global variables used:
c		busdta, kbsdta, bus, zone, base
        include 'ipfinc/dc2t.inc'
c	Global variables used:
c		dc2t(r*8)
        include 'ipfinc/lfiles.inc'
c	Global variables used:
c		None
        include 'ipfinc/prt.inc'
c	Global variables used:
c		errbuf
c
c	Local variables
c
        common /is_batch / is_batch
 
C       BLKDTA-BLD2DC
C
        common /iswap/ iswap(2,9)
c
        dimension itab(MAX2DC)
	double precision xtemp
        integer bptr1, k1, k2, k1x, k2x, ksect
	real br
c
        save
C
C**********************************************************************
C**********************************************************************
C       BUS ENTRY
C
        entry dc2tbs (nb,kerr)
        k1=nb
        it=0
        do 100 i=1,kdtot
           if (dc2t(1,i) .eq. k1 .or. dc2t(3,i) .eq. k1) then 
              it=it+1
              itab(it)=i
           endif
  100   continue
        if (it-1) 102,190,150
c
c       add new entry
c
  102   nxsw1 = 1
  110   kdtot=kdtot+1
        if(kdtot.gt.MAX2DC) then
           write (errbuf(1),120) MAX2DC
  120      format ('0 More than ',i3,' 2-terminal dc lines.')
           if (is_batch .eq. 0) then
             call prterx ('E',1)
           else
             call prterx ('F',1)
           endif
           kerr=1
           kdtot=0
           go to 230
        endif 
        dc2t(1,kdtot) = k1
        dc2t(2,kdtot) = base(k1)
        do 140 i=3,45
           dc2t(i,kdtot)=0
  140   continue
        idc=kdtot
        go to (192, 310, 290) nxsw1
c
c       excessive entities
c
  150   write (errbuf(1),160)bus(nb),base(nb),zone(nb)
  160   format ('0 2-terminal dc converter bus ',a8,f7.1,' zone ',a2,
     1          ' is duplicated in the following dc circuits:')
        do 180 i=1,it
           j=itab(i)
           k1=dc2t(1,j)
           k2=dc2t(3,j)
           write (errbuf(i+1),170)bus(k1),base(k1),bus(k2),base(k2)
  170      format(2x,' LD ',a8,f6.1,2x,a8,f6.1)
  180   continue
        call prterx ('w',it+1)
        kerr=1
        idc=0
c
c       matched entity-check validity
c
  190   idc=itab(1)
  192   if (dc2t(1,idc).eq.nb) then
           br=busdta(3,nb)
           if (br.eq.0.0) br=1.0
           dc2t(12,idc)=br
           dc2t(13,idc)=0.001*busdta(7,nb)*br
           dc2t(14,idc)=busdta(8,nb)
           dc2t(18,idc)=0.0174532952*busdta(5,nb)
           dc2t(19,idc)=0.0174532952*busdta(6,nb)
           dc2t(27,idc)=busdta(4,nb)
           dc2t(33,idc)=kbsdta(9,nb)
        else 
           dc2t(3,idc)=nb
           dc2t(4,idc) = base(nb)
           br=busdta(3,nb)
           if (br.eq.0.0) br=1.0
           dc2t(15,idc)=br
           dc2t(16,idc)=0.001*busdta(7,nb)*br
           dc2t(17,idc)=busdta(8,nb)
           dc2t(23,idc)=0.01745329252*busdta(5,nb)
           dc2t(24,idc)=0.01745329252*busdta(6,nb)
           dc2t(28,idc)=busdta(4,nb)
           dc2t(34,idc)=kbsdta(9,nb)
        endif
 
  230   continue
        return
c
C**********************************************************************
C**********************************************************************
c       line entry
c
        entry dc2tln (bptr1,kerr)
        k1 = kx(bptr1)
        k2 = ky(bptr1)
        ksect = brsect(bptr1)
        nbr = brnch_ptr(bptr1)
c
c       Check if d-c line is already processed....
c
        do i = 1,kdtot
           if (k1 .eq. dc2t(1,i) .and. k2 .eq. dc2t(3,i)) goto 900
           if (k1 .eq. dc2t(3,i) .and. k2 .eq. dc2t(1,i)) goto 900
        enddo
 
        it=0
        do 240 i=1,kdtot
           itab(i)=0
           if(k1 .eq. dc2t(1,i) .or. k2 .eq. dc2t(1,i))then
              it=it+1
              itab(i)=1
           endif
           if (k1 .eq. dc2t(3,i) .or. k2 .eq. dc2t(3,i))then
              if (itab(i).eq.0)it=it+1
              itab(i)=itab(i)+1
           endif
  240   continue
        if(it.eq.0) then
           nxsw1 = 2
           go to 110
        endif
c
c       examine matched entities for validity.
c
        idc=0
        do 260 i=1,kdtot
           if (itab(i).eq.2) go to 280
  260   continue
        do 270 i=1,kdtot
           if(itab(i).eq.0)go to 270
           if(dc2t(1,i) .eq. 0 .or. dc2t(3,i) .eq. 0) go to 280
  270   continue
c
c       add new branches
c
        nxsw1 = 3
        go to 110
c
c       idc is index of currnet branch-nullify other entities.
c
  280   idc=i
  290   if (k1 .eq. dc2t(1,idc) .or. k2 .eq. dc2t(3,idc)) ksw=1
        if (k1 .eq. dc2t(3,idc) .or. k2 .eq. dc2t(1,idc)) ksw=2
        do 300 i=1,kdtot
           if (i .ne. idc .and. itab(i) .eq. 1) then
              if (k1 .eq. dc2t(1,i) .or. k2 .eq. dc2t(3,i))jsw=1
              if (k1 .eq. dc2t(3,i) .or. k2 .eq. dc2t(1,i))jsw=2
              do 292 j=1,9
                 j1=iswap(jsw,j)
                 j2=iswap(ksw,j)
                 dc2t(j2,idc)=dc2t(j1,i)
                 dc2t(j1,i)=0
  292         continue
           endif
  300   continue
c
c       build dc line entity
c
  310   continue
        jbr = iabs(nbr)
        if (brnch(8,jbr).gt.0.0)then
           ksw=1
           if (nbr.gt.0) then
              k1x=k1
              k2x=k2
           else
              k1x=k2
              k2x=k1
           endif
        else
           ksw=2
           if (nbr.lt.0) then
              k1x=k1
              k2x=k2
           else
              k1x=k2
              k2x=k1
           endif
        endif
c
        if (k1x .ne. dc2t(1,idc) .and. k2x .ne. dc2t(3,idc)) then
c
c       exchange rectifier and inverter
c
           do 320 i=1,9
              j1=iswap(1,i)
              j2=iswap(2,i)
              xtemp=dc2t(j1,idc)
              dc2t(j1,idc)=dc2t(j2,idc)
              dc2t(j2,idc)=xtemp
  320      continue
        endif
        if (dc2t(1,idc).eq.0) then
           dc2t(1,idc) = k1x
           dc2t(2,idc) = base(k1x)
        endif
        if (dc2t(3,idc).eq.0) then
           dc2t(3,idc) = k2x
           dc2t(4,idc) = base(k2x)
        endif
        if (ksw.eq.1) then
           dc2t(21,idc)=0.01745329252*brnch(10,jbr)
           dc2t(25,idc)=0.01745329252*brnch(18,jbr)
           dc2t(5,idc)=brnch(8,jbr)
        else
           dc2t(21,idc)=0.01745329252*brnch(18,jbr)
           dc2t(25,idc)=0.01745329252*brnch(10,jbr)
           dc2t(5,idc)=-brnch(8,jbr)
        endif
        dc2t(6,idc)=brnch(9,jbr)
        dc2t(7,idc)=ksect
        dc2t(8,idc)=brnch(5,jbr)
        dc2t(9,idc)=brnch(6,jbr)
        dc2t(10,idc)=brnch(7,jbr)
        dc2t(11,idc)=brnch(4,jbr)
  340   continue
 
  900   continue
        return
        end
