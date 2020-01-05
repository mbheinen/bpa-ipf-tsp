C    @(#)p_ldardata.f	20.5 1/7/99
C****************************************************************
C
C       File: p_ldardata.f
C
C       Purpose: Routine to load area interchange data
C
C       Author: Walt Powell  Date: 5 March 1993
C       Called by: p_gtdata.f
C
C****************************************************************
C
        integer function p_ldardata (in_buffer, out_buffer)

c
c       This subroutine returns the area interchange data
C
c       Output parameter:
c
c       in_buffer  - a character string specifying desired data
c       out_buffer - a character string for storing data
c
c       Return status: 0 - successful
c                      1 - errors
 
        include 'ipfinc/parametr.inc'

        character in_buffer * (MAXBUFFER), out_buffer * (MAXBUFFER)

        include 'ipfinc/blank.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/cbus.inc'
        include 'ipfinc/anlys.inc'
        include 'ipfinc/busanl.inc'
        include 'ipfinc/owncom.inc'
        include 'ipfinc/sortuvov.inc'
        include 'ipfinc/update.inc'

        common /area_data/ gentot, lodtot, lostot, arcact(4*MAXCAR),
     &                     arcflg(4*MAXCAR) 
        real gentot, lodtot, lostot, arcact
        integer arcflg

        character  null * 1, linefeed * 1
        integer o2
        logical found
        external kompai,swapai 

        null = char(0)
        linefeed = char(10)
        p_ldardata = 0

        out_buffer(1:1) = null
        o2 = index (out_buffer,null)
c
c       Conditionally update zone arrays
c
        call updzon()
C                                         
C       Sort ARCINT and ARCINP arrays     
C                                         
        if (ntotic .gt. 0) call qiksrt (1, ntotic, kompai, swapai)
C
C       Compute Scheduled and Actual intertie quantities
C       ARCFLG:  0 - No scheduled "I" records in base
C                1 - Scheduled "I" records in base
C
        do i = 1,ntotic
           arcflg(i) = 1
           arcact(i) = 0.0
        enddo
 
        do jt = 1,jtie
 
           k1=tie(1,jt)
           k2=tie(7,jt)
 
           ka1 = tie(2,jt)
           ka2 = tie(8,jt)
 
           pin = gtieflow (jt)
 
           do isw = 1, 2
              l1 = 1
              l2 = ntotic
              found = .false.
              do while (l1 .le. l2 .and. .not. found)
                 ll = (l1 + l2)/2
                 kompare = kompr(arcint(1,ll),arcnam(ka1),junk)
                 if (kompare .eq. 0) 
     &              kompare = kompr(arcint(2,ll),arcnam(ka2),junk)
                 if (kompare .lt. 0) then
                    l1 = ll + 1
                 else if (kompare .gt. 0) then
                    l2 = ll - 1
                 else 
                    found = .true.
                 endif
              enddo
              if (.not. found) then
C
C                Intertie not found - augment arrays
C
                 write (*, 187) arcnam(ka1), arcnam(ka2)
  187            format (' Inserted entity ', a, ' - ', a)
                 ll = max0 (l1,l2)
                 do j = ntotic,ll,-1
                    arcint(1,j+1) = arcint(1,j)
                    arcint(2,j+1) = arcint(2,j)
                    arcinp(j+1) = arcinp(j)
                    arcact(j+1) = arcact(j)
                    arcflg(j+1) = arcflg(j)
                 enddo
                 ntotic = ntotic + 1
                 arcint(1,ll) = arcnam(ka1)
                 arcint(2,ll) = arcnam(ka2)
                 arcact(ll) = 0.0
                 arcinp(ll) = 0.0
                 arcflg(ll) = 0
              endif
              arcact(ll) = arcact(ll) + pin
c
c             Transpose "I" record for second pass
c
              ka1 = tie(8,jt)
              ka2 = tie(2,jt)
              pin = -pin
           enddo
        enddo
        return
        end
