C    @(#)p_ldxardta.f	20.5 1/7/99
C****************************************************************
C
C       File: p_ldxardta.f
C
C       Purpose: Routine to load area interchange data from the
c                alternate base case data.
C
C       Author: Walt Powell  Date: 5 March 1993
C       Called by: p_gtdata.f
C
C****************************************************************
C
        integer function p_ldxardta (in_buffer, out_buffer)

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
        include 'ipfinc/alt_case.inc'
        include 'ipfinc/owncom.inc'
        include 'ipfinc/sortuvov.inc'
        include 'ipfinc/update.inc'

        common /xarea_data/ gentot, lodtot, lostot, arcact(4*MAXCAR),
     &                      arcflg(4*MAXCAR) 
        real gentot, lodtot, lostot, arcact
        integer arcflg

        character  null * 1, linefeed * 1
        integer o2
        logical found
        external kompxai, swapxai 

        p_ldxardta = 0              ! set default return = success
        null = char(0)
        linefeed = char(10)

        out_buffer(1:1) = null
        o2 = index (out_buffer,null)
C                                         
C       Sort OARCINT and OARCINP arrays     
C                                         
        if (ontotic .gt. 0) call qiksrt (1, ontotic, kompxai, swapxai)
C
C       Compute Scheduled and Actual intertie quantities
C       ARCFLG:  0 - No scheduled "I" records in base
C                1 - Scheduled "I" records in base
C
        do i = 1, ontotic
           arcflg(i) = 1
           arcact(i) = 0.0
        enddo
 
        do jt = 1, ojtie
 
           k1=oltie(1,jt)
           k2=oltie(7,jt)
 
           ka1 = oltie(2,jt)
           ka2 = oltie(8,jt)
 
           pin = xgtieflow (jt)
 
           do isw = 1, 2
              l1 = 1
              l2 = ontotic
              found = .false.
              do while (l1 .le. l2 .and. .not. found)
                 ll = (l1 + l2)/2
                 kompare = kompr(oarcint(1,ll), oarcnam(ka1), junk)
                 if (kompare .eq. 0) 
     &              kompare = kompr(oarcint(2,ll), oarcnam(ka2), junk)
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
                 write (*, 187) oarcnam(ka1), oarcnam(ka2)
  187            format (' Inserted entity ', a, ' - ', a)
                 ll = max0 (l1,l2)
                 do j = ontotic,ll,-1
                    oarcint(1,j+1) = oarcint(1,j)
                    oarcint(2,j+1) = oarcint(2,j)
                    oarcinp(j+1) = oarcinp(j)
                    arcact(j+1) = arcact(j)
                    arcflg(j+1) = arcflg(j)
                 enddo
                 ontotic = ontotic + 1
                 oarcint(1,ll) = oarcnam(ka1)
                 oarcint(2,ll) = oarcnam(ka2)
                 arcact(ll) = 0.0
                 oarcinp(ll) = 0.0
                 arcflg(ll) = 0
              endif
              arcact(ll) = arcact(ll) + pin
c
c             Transpose "I" record for second pass
c
              ka1 = oltie(8,jt)
              ka2 = oltie(2,jt)
              pin = -pin
           enddo
        enddo
        return
        end
