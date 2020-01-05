C    @(#)out_bus.f	20.4 2/13/96
C****************************************************************
C
C   File: out_bus.f
C   Purpose: Routine to obtain BCD image of INPUT/OUTPUT/SYSTEM
C            data
C
C   Author: Walt Powell  Date: 20 February 1992
C                        Modified: 20 February 1992
C   Called by:
C
C****************************************************************
C
        subroutine out_bus (nb, datarec)

        character datarec * (*)
c
c       This subroutine returns WSCC-formated output data records.
c       Output parameter:
c
c       datarec - a character string for storing data
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/alpha.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/cbus.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/xdata.inc'
        include 'ipfinc/dc2t.inc'
        include 'ipfinc/dcmt.inc'
        include 'ipfinc/ordsta.inc'
  
        character type * 1

        call bcdbus (nb, datarec)
        if (ordvlt .eq. 1) then
           kt = nb
        else
           kt = inp2opt(nb)
        endif
	ntyp = ntypu(kt)
        call typno (type, ntyp)
        
	if (ntyp .eq. 5) then
           do idc = 1, kdtot
               if (dc2t(1,idc) .eq. nb ) go to 10
               if (dc2t(3,idc) .eq. nb ) go to 20
           enddo
   
   10      pdc = dc2t(42,idc) 
           qdc = dc2t(44,idc)
           pvlv = abs(pdc - 0.001 * dc2t(40,idc) * dc2t(39,idc))
           qvlv = qdc
           dci = dc2t(39,idc)
           angle = 57.2957795 * dc2t(22,idc)
           vdc = dc2t(40,idc)
           go to 30

   20      pdc = dc2t(43,idc) 
           qdc = dc2t(45,idc)
           pvlv = abs(pdc + 0.001 * dc2t(41,idc) * dc2t(39,idc))
           qvlv = qdc
           dci = -dc2t(39,idc)
           angle = 57.2957795 * dc2t(26,idc)
           vdc = dc2t(41,idc)

   30      write( datarec(21:), 40 )  pdc, qdc, vdc, angle, pvlv, 
     1                                   qvlv
   40      format(6e15.7)

        else if (ntyp .eq. 12) then

           do idc = 1,mtdcbs
               if (dcmtbs(1,idc) .eq. nb ) go to 110
           enddo
           call erexit
  110      if (dcmtbs(3,idc) .eq. 0) then
              pdc = 0.0
              qdc = 0.0
              pvlv = 0.0
              qvlv = 0.0
              dci = 0.0
	      angle = 0.0
              vdc = 0.0
           else 
              pdc = dcmtbs(25,idc)
              qdc = dcmtbs(26,idc)
              pvlv = abs(pdc - dcmtbs(19,idc))
              qvlv = qdc
              if (dcmtbs(20,idc) .ne. 0.0) then
                 dci = 1000.0 * dcmtbs(19,idc) / dcmtbs(20,idc)
              else 
                 dci = 0.0
              endif
              angle = 57.2957795 * dcmtbs(13,idc)
              vdc = dcmtbs(24,idc)
           endif 
           write( datarec(21:), 40 )  pdc, qdc, vdc, angle, pvlv, 
     1                                   qvlv
        else
           call nrpqv (kt, pk, dpk, qk, dqk, vk)

           pload = (ploadu(kt) + inetr(kt) * vk) * bmva 
           qload = (qloadu(kt) - ineti(kt) * vk) * bmva 
           pgen = (pk - inetr(kt) * vk) * bmva + pload
           voltkv = vk * base(nb)
           if (e(kt) .eq. 0.0 .and. f(kt) .eq. 0.0) then 
              degree = 0.0                        
           else                                  
              degree = 57.2957795 * datan2( f(kt), e(kt) )
           endif                                
           pvlv = 0.0
           qvlv = 0.0
           call typno( type, ntyp )
           call allocq (nb, qk, qgen, qgnmax, qgnmin, qld, totcap,
     1                  usecap, totrek, userek, unsked, qerr)
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

