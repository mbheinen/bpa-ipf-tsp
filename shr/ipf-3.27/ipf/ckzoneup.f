C    @(#)ckzoneup.f	20.5 11/12/98
      integer function ckzoneup (status)
      integer status
C
C     This function reads >DEFINE user-analysis records to determine
C     whether it is necessary to udpate the ZSUM array.  Its return
C     value should be inserted into UPDATE(1):
C
C     CHKZONEUP = 0 -> User-analysis records are not present. Do
C                      not update ZSUM.     
C                 1 -> User-analysis records are present. Update ZSUM.     
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/prt.inc'
      include 'ipfinc/update.inc'
      include 'ipfinc/usranl.inc'

      logical opened
      integer open_file, first

      ckzoneup = 0

C     Search USRDEF for > DEFINE_TYPE ZONE_INDEX
C     commands to determine whether ZSUM should be updated. 
C     Open UA direct access input file if necessary.   

      lunusr = 25 
      inquire (unit=lunusr, opened=opened)
      if (.not. opened) then
         call close_file(lunusr)
         status = open_file(lunusr, 'USER_ANA.SCR', 'U', 'W', ios)
         if (status .ne. 0) then
            write (errbuf(1), 100) ios, lunusr
  100       format (' Error No. ', i3,
     &               ' opening scratch file on logical unit ', i2)
            call prterx ('W',1)
            go to 110
         endif
      endif  
      rewind lunusr  
      do iuser = 1, numusr 
         max1 = numdef(iuser)
         do first  = 1, max1, 200  
            last = min0 (first+199,max1) 
            read (lunusr) (usrdef(i),i=first,last)   
         enddo
         max2 = numtxt(iuser)
         do first = 1, max2, 200   
            last = min0 (first+199,max2) 
            read (lunusr) (usrtxt(i),i=first,last)   
         enddo
         do i = 1, numdef(iuser)   
            if (index(usrdef(i), 'DEFINE_TYPE') .ne. 0) then
               if (index(usrdef(i), 'ZONE_INDEX') .ne. 0 .or. 
     &             index(usrdef(i), 'OWNER_LOSS') .ne. 0 .or.
     &             index(usrdef(i), 'AREA_LOSS') .ne. 0 .or.
     &             index(usrdef(i), 'SYSTEM_LOSS') .ne. 0) then
                  ckzoneup = 1 
                  go to 110
               endif
            endif   
         enddo
      enddo
  110 continue   
      return
      end
