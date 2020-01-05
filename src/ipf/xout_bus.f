C    @(#)xout_bus.f	20.4 2/13/96
C****************************************************************
C
C   File: xout_bus.f
C   Purpose: Routine to obtain BCD image of INPUT/OUTPUT/SYSTEM
C            data from alternate base case.  Note that this is
c            a limited edition of the original out_bus module.
c            The limitation is that the alternate data set excludes
c            some bus input data and some data accessing routines.
C
C   Author: Walt Powell  Date: 26 July 1993
C                        Modified: 
C   Called by:
C
C****************************************************************
C
        subroutine xout_bus (nb, datarec)

        character datarec * (*)
c
c       This subroutine returns WSCC-formated output data records.
c       Output parameter:
c
c       datarec - a character string for storing data
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
c	Global variables used:
c		bmva
        include 'ipfinc/lfiles.inc'
c	Global variables used:
c		None
        include 'ipfinc/prt.inc'
c	Global variables used:
c		None
        include 'ipfinc/alt_case.inc'
c	Global variables used:
c		okdcmtb, oldowner, oldbus, okdc2t, ontypu,
c		oinp2opt, oqnetu, oqloadu, odcmtbs, odc2t,
c		oldbase, okdtot, omtdcbs, olde, oldf
c 
        character type * 1, xcode * 4, code * 4
c
        xcode = code (oldbase(nb), 4, 0)
        kt = oinp2opt(nb)
	ntyp = ontypu(kt)
        call typno (type, ntyp)
        
        write (datarec, 100) type, oldowner(nb), oldbus(nb), xcode, 
     &         oldzone(nb)
  100   format ('B', a, t4, a, t7, a, a, a)

	if (ntyp .eq. 5) then
           do idc = 1, okdtot
               if (okdc2t(1,idc) .eq. nb ) go to 10
               if (okdc2t(3,idc) .eq. nb ) go to 20
           enddo
   
   10      pdc = odc2t(42,idc) 
           qdc = odc2t(44,idc)
           pvlv = abs(pdc - 0.001 * odc2t(40,idc) * odc2t(39,idc))
           qvlv = qdc
           dci = odc2t(39,idc)
           angle = 57.2957795 * odc2t(22,idc)
           vdc = odc2t(40,idc)
           go to 30

   20      pdc = odc2t(43,idc) 
           qdc = odc2t(45,idc)
           pvlv = abs(pdc + 0.001 * odc2t(41,idc) * odc2t(39,idc))
           qvlv = qdc
           dci = -odc2t(39,idc)
           angle = 57.2957795 * odc2t(26,idc)
           vdc = odc2t(41,idc)

   30      write( datarec(21:), 40 )  pdc, qdc, vdc, angle, pvlv, 
     1                                   qvlv
   40      format(6e15.7)

        else if (ntyp .eq. 12) then

           do idc = 1, omtdcbs
               if (okdcmtb(1,idc) .eq. nb ) go to 110
           enddo
           call erexit
  110      if (okdcmtb(3,idc) .eq. 0) then
              pdc = 0.0
              qdc = 0.0
              pvlv = 0.0
              qvlv = 0.0
              dci = 0.0
	      angle = 0.0
              vdc = 0.0
           else 
              pdc = odcmtbs(25,idc)
              qdc = odcmtbs(26,idc)
              pvlv = abs(pdc - odcmtbs(19,idc))
              qvlv = qdc
              dci = 1000.0 * odcmtbs(19,idc) / odcmtbs(20,idc)
              angle = 57.2957795 * odcmtbs(13,idc)
              vdc = odcmtbs(24,idc)
           endif 
           write( datarec(21:), 40 )  pdc, qdc, vdc, angle, pvlv, 
     1                                qvlv
        else

           pload = (oploadu(kt) + oinetr(kt) * vk) * bmva
           qload = (oqloadu(kt) - oineti(kt) * vk) * bmva 
           pgen = (opnetu(kt) - oinetr(kt) * vk) * bmva + pload
           vk = sqrt (olde(kt) ** 2 + oldf(kt) ** 2)
           voltkv = vk * oldbase(nb)

           if (olde(kt) .eq. 0.0 .and. oldf(kt) .eq. 0.0) then 
              degree = 0.0                        
           else                                  
              degree = 57.2957795 * atan2( oldf(kt), olde(kt) )
           endif                                
           pvlv = 0.0
           qvlv = 0.0
           call typno( type, ntyp )
           call xallocq(nb, oqnetu(kt), qgen, qgnmax, qgnmin, qld, 
     &                  totcap, usecap, totrek, userek, unsked, qerr)
           if ( usecap + unsked .gt. 0.0 ) then
              if ( totcap .gt. 0.0 ) then
                 bsched = totcap
              else
                 bsched = totrek
              end if
              bused = usecap + unsked
           else if ( userek + unsked .lt. 0.0 ) then
              if ( totrek .lt. 0.0 ) then
                 bsched = totrek
              else
                 bsched = totcap
              end if
              bused = userek + unsked
           else if ( unsked .lt. 0.0 ) then
              if ( totrek .lt. 0.0 ) then
                 bsched = totrek
              else
                 bsched = totcap
              end if
              bused = unsked
           else if ( unsked .gt. 0.0 ) then
              if ( totcap .gt. 0.0 ) then
                 bsched = totcap
              else
                 bsched = totrek
              end if
              bused = unsked
           else if ( totrek .lt. 0.0 .and. totcap .eq. 0.0 ) then
              bsched = totrek
              bused = userek
           else
              bsched = totcap
              bused = usecap
           end if
           write( datarec(21:), 160 )  pgen, qgen, voltkv, degree, 
     &                                 pload, qload, bused, bsched,
     &                                 usecap, totcap, userek, totrek,
     &                                 unsked
  160      format(13e15.7)
	endif
	return
	end
