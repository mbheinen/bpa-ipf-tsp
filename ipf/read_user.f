C    @(#)read_user.f	20.5 11/11/97
      subroutine read_user (file)
      integer file
C
C     This subroutine processes /USER_ANALYSIS commands.
C
      include 'ipfinc/parametr.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/usranl.inc'
      include 'ipfinc/filnam.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/dtaiop.inc'
 
      character word(100)*30, capital*30, null*1, temfil*60, linefeed*1
      logical finished 
      integer teminp, first, error 
 
      null = char(0)
      linefeed = char(10)
      error = 0

      if (numusr .ge. 20) then
         write (errbuf(1), 112) numusr
  112    format (' More than ', i3, ' /USER_ANALYSIS commands. ',
     &           'The following records will be rejected.')
         call prterx ('W',1)
         error = 1
         finished = .false.
         do while (.not. finished)
            read (inp, 260, end=114) inrcd
            if (index ('(/', inrcd(1:1)) .ne. 0) then
               card = inrcd(1:1)
               finished = .true.
            endif
         enddo
  114    continue
         go to 900
 
      endif
      numusr = numusr + 1
      numdef(numusr) = 0
      numtxt(numusr) = 0
      usrdbg(numusr) = 0
      usrfil(numusr) = ' '
      cmnflg(numusr) = .false.

      read (file, 260, end=450) inrcd                                 

      if (index(inrcd,'USER_AN') .ne. 0 .or.
     1    index(inrcd,'USERAN') .ne. 0) then
C
C        /USER_ANALYSIS, FILE = * , DEBUG = ON, OUTPUT = <file_name>
C
         call uscan(inrcd, word, nwrd, '=',' ,'//linefeed)
c
c        Capitalize all word() except <filename>
c
         i = 0
         do while ( i .lt. nwrd )
            i = i + 1
            if ( word(i) .eq. '=' ) i = i + 1
            word(i) = capital(word(i))
            if ( word(i) .eq. 'OUTPUT' ) then
               if ( word(i) .eq. '=' ) i = i + 1
            endif
         enddo
 
         do iwrd = 2, nwrd-1, 2                                     
            if (ickdic(word(iwrd), 'FILE*', 1) .eq. 1) then
                temfil=inpnm
                teminp=file         
                inpnm = word(iwrd+1)
                file = 23           
                ierror=0
                call opnfil (file, inpnm, ierror)
                if (ierror.ne.0) then
                   write (errbuf(1),1410) inpnm
 1410              format ('0 File ', a, ' cannot be opened. File ignore
     1d.')
                   call prterx ('W',1)
                   inpnm=temfil
                   file=teminp
                else
                   incsw = 1
                   usrfil(numusr) = inpnm
                endif
 
            else if (word(iwrd) .eq. 'DEBUG') then
 
               if (word(iwrd) .eq. 'ON') then
                  usrdbg(numusr) = 1
               else
                  usrdbg(numusr) = 0
               endif
            else if (ickdic(word(iwrd), 'OUTPUT*', 1) .eq. 1) then
                usrfil(numusr) = word(iwrd+1)
            else if (ickdic(word(iwrd), 'APPEND*', 1) .eq. 1) then
                cmnflg(numusr) = .true.
            endif
 
         enddo

         numrcd = 0
         go to 232

      else

         numrcd = 0
         go to 262

      endif
  
  232 read (file, 260, end=450) inrcd                                 
  260 format (a)                                                     

  262 card = inrcd(1:1)                                              
      numrcd = numrcd + 1                                            
C
C     Three categories of data follow / USER_ANALYSIS                
C
C     1. Symbol definitions:                                         
C
C           > DEFINE_TYPE OWNER_LOSS, o1 = <owner1>, o2 = <owner2>
C           > DEFINE_TYPE AREA_LOSS,  a1 = <area1>, a2 = <area2>
C           > DEFINE_TYPE ZONE_LOSS,  z1 = <zone1>, z2 = <zone2>
C           > DEFINE_TYPE SYSTEM_LOSS, s
C           > DEFINE_TYPE BRANCH_P
C             LET p1 = <Bus1,base1,bus2,base2>, -
C             LET p2 = <Bus3,base3,bus3,base4>, -
C             ...
C             LET pn = <Busm,basem,busn,basen>
C           > DEFINE_TYPE BRANCH_Q
C             LET q1 = <Bus1,base1,bus2,base2>, -
C             LET q2 = <Bus3,base3,bus3,base4>, -
C             ...
C             LET qn = <Busm,basem,busn,basen>
C           > DEFINE_TYPE BRANCH_P_METER
C             LET p1 = <Bus1,base1,bus2,base2>, -
C             LET p2 = <Bus3,base3,bus3,base4>, -
C             ...
C             LET pn = <Busm,basem,busn,basen>
C           > DEFINE_TYPE BRANCH_Q_METER
C             LET q1 = <Bus1,base1,bus2,base2>, -
C             LET q2 = <Bus3,base3,bus3,base4>,
C             ...
C             LET qn = <Busm,basem,busn,basen>
C           > DEFINE_TYPE INTERTIE_P
C             LET i1 = <Area_1,Area_2>, -
C             LET i2 = <area_3,area_4>, -
C             ...
C             LET in = <area_m,area_n>
C           > DEFINE_TYPE INTERTIE_Q
C             LET j1 = <Area_1,Area_2>, -
C             LET j2 = <area_3,area_4>, -
C             ...
C             LET jn = <area_m,area_n>
C           > DEFINE_TYPE INTERTIE_P_SCHEDULED
C             LET i1 = <Area_1,Area_2>, -
C             LET i2 = <area_3,area_4>, -
C             ..
C             LET in = <area_m,area_n>
C           > DEFINE_TYPE FUNCTION
C             LET i = h + g + k
C           > DEFINE_TYPE BUS_INDEX
C             LET b1 = <Bus1,base1>, -
C             LET b2 = <Bus2,base2>, -
C             ...
C             LET bn = <Busn,base2n, -
C           > DEFINE_TYPE LINE_INDEX or DEFINE_TYPE BRANCH_INDEX
C             LET l1 = <Bus1,base1,bus2,base2,id>, -
C             LET l2 = <Bus3,base3,bus4,base4,id>, -
C             ...
C             LET lp = <Busp,basep,busq,baseq,id>, -
C           > DEFINE_TYPE INTERTIE_INDEX
C             LET i1 = <area1,area2>, -
C             LET i2 = <area3,area4>, -
C             ...
C             LET ip = <areap,areaq>
C
C           > DEFINE_TYPE AREA_INDEX
C             LET a1 = <area1>, -
C             LET a2 = <area2>, -
C             ...
C             LET ip = <areap>
C
C     2. Page Header/Subheader information:                          
C
C           H xxx (header text)
C           S xxx (subheader text)
C
C     3. Page text                                                   
C
C           C xxx (page text)
C
C
C     Skip "." comment text.                                         
C
      if (card .eq. '.') go to 232                                   
 
      if (index ('> ', card) .ne. 0) then                            
         if (numdef(numusr) .ge. 500) then
            write (errbuf(1),10237) 200                              
10237       format ('More than ',i3, '/ USER_ANALYSIS > definition recor
     &ds.')                                                          
            call prterx ('E', 1)                                     
            error = 1                                                
         else                                                        
            numdef(numusr) = numdef(numusr) + 1
            usrdef(numdef(numusr)) = inrcd
         endif                                                       
         go to 232                                                   
      else if (index ('HSC', card) .ne. 0) then                      
         if (numtxt(numusr) .gt. 500) then
            write (errbuf(1),10238) 500                              
10238       format ('More than ',i3, '/ USER_ANALYSIS text records .') 

            call prterx ('E', 1)                                     
            error = 1                                                
         else                                                        
            numtxt(numusr) = numtxt(numusr) + 1
            usrtxt(numtxt(numusr)) = inrcd
         endif                                                       
         go to 232                                                   
      else                                                           
         go to 232
      endif                                                          
C
C     End-of-file. This is normal if alternate file is used.         
C
  450 if (incsw .eq. 0) inrcd = '( END ) READ_USER'                     
C
C     Restore normal input file if alternate file invoked.           
C
  460 if (incsw .eq. 1) then                                         
         write (*, 300) numrcd, inpnm                           
  300    format (1x, i4, ' records read from /USER_ANALYSIS file ',  
     1      a)                                                       
         close( unit = file )                                         
         inpnm=temfil                                                
         file=teminp                                                  
         incsw = 0                                                   
         read (file, 260, end=450) inrcd                              
         card = inrcd(1:1)                                           
      endif                                                          

      numrcd = numrcd + 1
      last = numtxt(numusr)
      do i = last, 1, -1                                      
         if (usrtxt(i)(1:1) .eq. 'C') then
            usrtxt(i+2) = usrtxt(i)
         endif
      enddo
      i = 1 
      do while (i .lt. last .and. usrtxt(i)(1:1) .ne. 'C')
         i = i + 1
      enddo

      numtxt(numusr) = numtxt(numusr) + 2
      write (usrtxt(i), 302) cspare(30), cspare(31), clabl1, clabl2
  302 format ('C Case: ', a, ' Date: ', a, ' Description: ', 2a)
      write (usrtxt(i+1), 303) pversn, usrnam
  303 format ('C PF version: ', a, ' User: ', a)

      if (error .eq. 0 .and. numusr .gt. 0) then
        max1 = numdef(numusr)
        do first  = 1, max1, 200
          last = min0 (first+199,max1)
          write (lunusr) (usrdef(i),i=first,last)
        enddo
        max2 = numtxt(numusr)
        do first = 1, max2, 200
          last = min0 (first+199,max2)
          write (lunusr) (usrtxt(i),i=first,last)
        enddo
      else
        numusr = 0
      endif

  900 continue
      return
      end
