C    @(#)zone_ren.f	20.4 5/27/99
C****************************************************************
C
C       File: zone_ren.f
C
C       Purpose: Rename zones per Zone Rename Z record.
C                                                                      *
C       Input parameters:
C
C       zonerenfil   - the logical unit opened
C
C       Author: Walt Powell            Date: 12 May 1999
C       Called by: p_svfile.f
C
C****************************************************************
C
        integer function zone_ren (zonerenfil)
        integer zonerenfil
 
        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/arcntl.inc' 
        include 'ipfinc/area.inc' 
        include 'ipfinc/bus.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/basval.inc'
        include 'ipfinc/header.inc'
        include 'ipfinc/coment.inc'
        include 'ipfinc/pti_data.inc'
        include 'ipfinc/wsccbase.inc'

        character text*120, zmod(2,15)*2, zn*2
        logical finished
C
C	Process zone rename card "Z"
C

        text = ' '
        zone_ren = 0
        finished = .false.
        do while (.not. finished)
          read (zonerenfil, fmt='(a)', end=10160) text(1:80)
          if (text(1:1) .eq. 'Z') then
            read (text, 10120) zmod
10120       format (3x,15(2a2,1x))
C
            i = 1
            do while (i .le. 15 .and. 
     &              ((zmod(1,i) .ne. ' ' .or. i .eq. 1)))
              n = 0
              m = 0
              do j = 1, ntot
                if (zone(j) .eq. zmod(1,i)) then
                  zone(j) = zmod(2,i)
                  n = n + 1
                endif
              enddo
C
C             Rename zones within areas
C
              do j = 1,ntotc
                do k = 1,MAXCAZ
                  if (arczns(k,j) .ne. '  ' .or. k .eq. 1) then
                    if (zmod(1,i) .eq. arczns(k,j)) then
                      arczns(k,j) = zmod(2,i)
                      m = 1
                      go to 10122
                    endif
                  endif
                enddo
              enddo
              write (*, 10121) zmod(1,i)
              write (dbug, 10121) zmod(1,i)
10121         format(' Rename zone ', a2, ' is not in any Area Interchan
     &ge record')
              go to 10130

c             Obtain count NUMXZN of zones for this area.            
                                                                
10122         continue
              do k = 1, MAXCAZ                                   
                zn = arczns(k,j)                                    
                if (zn .eq. '  ' .and. k .gt. 1) then               
                  numxzn = k - 1                                   
                   go to 10123
                endif                                               
              enddo
              numxzn = MAXCAZ                                        
10123         continue                                               
                                                                         
c             Check for duplicate zones within each area. 
         
              k = 1
              do while (k .le. numxzn)
10124           do kx = k+1, numxzn                     
                  if (arczns(kx,j) .eq. arczns(k,j) .and.  
     &                arczns(kx,j) .ne. ' ') then          
                    write (*, 10126) arczns(kx,j), arcnam(j) 
                    write (dbug, 10126) arczns(kx,j), arcnam(j) 
10126               format(' Duplicate zone ', a2, 
     &                ' removed in area ',a10)
                    do ky = kx, numxzn-1                       
                      arczns(ky,j) = arczns(ky+1,j)               
                    enddo
                    arczns(numxzn,j) = ' '                       
                    numxzn = numxzn - 1                          
                    if (k .le. numxzn) then                      
                      go to 10124                                 
                    else                                         
                      go to 10128                                 
                    endif                                        
                  endif                                           
                enddo
                k = k + 1
              enddo
10128         continue                                              

              write (dbug, 10140) n, m, zmod(1,i), zmod(2,i)
              write (*, 10140) n, m, zmod(1,i), zmod(2,i)
10140         format (1x, i5, ' Buses ', i2, 
     &             ' Areas changed from zone ', a, ' to zone ', a)

10130         continue
              i = i + 1
            enddo
          else
            write (dbug, 10150) text(1:40)
            write (*, 10150) text(1:40)
10150       format (' * Illegal zone rename record ignored ', a)
          endif
        enddo
10160   continue

        return
	end
