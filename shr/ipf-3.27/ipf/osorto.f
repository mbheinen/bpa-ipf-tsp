C    @(#)osorto.f	20.5 5/27/98
        subroutine osorto
C
C       PREPARE OUTPUT SORT ORDER TABLE BY ZONE/AREA
C
        include 'ipfinc/parametr.inc'

        include 'ipfinc/arcntl.inc'
        include 'ipfinc/arsort.inc'
        include 'ipfinc/asort.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
C
        external komozs,swaozs

        character *2 zn

        do i=1,ntot
           sorto(i)=alf2inp(i)
        enddo

        if ( kspare(11) .eq. 1) then

C          ZONE SORT ORDER (Default of no areas)

           call qiksrt(1,ntot,komozs,swaozs)

        elseif ( kspare(11) .eq. 3 ) then

C          OWNER SORT ORDER

           call qiksrt(1,ntot,komozs,swaozs)

        elseif (kspare(11) .eq. 2 .and.
     &         (natot .gt. 0 .or. ntotc .gt. 0)) then

C         AREA SORT ORDER

          if (natot .eq. 0) then
             natot=ntotc
             do i=1,natot
                arsnam(i)=arcnam(i)
                do j=1,MAXCAZ
                   arsens(j,i)=arczns(j,i)
                enddo
             enddo
          endif
C
          nab=0
          do i=1,natot
            do j=1,ntot_alf
               n = alf2inp(j)
               zn=zone(n)
               do k=1,MAXCAZ
                  if (arsens(k,i) .eq. ' ' .and. k.gt.1) go to 2200
                  if(zn.eq.arsens(k,i)) then
                     nab=nab+1
                     sorto(nab)=n
                     barea(nab)=i
                     go to 2200
                  endif
               enddo
2200        continue
            enddo
          enddo

          do j = ntot_alf+1, ntot
            nab=nab+1
            sorto(nab)=alf2inp(j)
            barea(nab)=natot + 1
          enddo

        else

C         Area sort reverts to alpha sort in absense of AREA records

          kspare(11) = 0
        endif

        return
        end
