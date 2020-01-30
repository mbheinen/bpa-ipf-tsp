C    %W% %G%
      subroutine opents (ia)                                              
C                                                                       

C     This subroutine opens various files needed by the program       
c     NOTE: the following statements do not conform to the
c     FORTRAN-77 standard and may need to be removed for some platforms
c
c       disp='delete'
c       form='unformatted'
c       recordsize=4065
c       recordtype=variable
c       organization=relative
c
      include 'tspinc/blkcom1.inc'                                             
      include 'tspinc/files.inc'                                               
      include 'tspinc/reread.inc'                                              

      common /error_code/ error_code
      integer error_code
c
      logical debug                                                     
      integer status, open_file
      character type_open * 8
      data debug /.false./                                              

c     begin     begin     begin     begin     begin     begin 

C                                                                       
C     Master list of files
C
C       L1 = scratch files (INPUT and INITAL)
C       L2 = scratch files (INPUT and INITAL)
C       L3 = Powerflow base data file                  
C       L5 = Swing input data file
c       L6 = Swing printout file
C       L8 = Swing history file    
C       L9  = Existing saved swing data file
c       L11 = auxiliary (tabular) listing file   
C       L13 = Debug output file  
C       L15 = New saved swing data file
c       L22 = PDF plot neutral file.
C       L23 = PostScript Master file  
C                                                                       
      if (is_it_vms() .ne. 0) then
         type_open = 'new'
      else
         type_open = 'unknown'
      endif

      go to (101,201,301,401,501,601,701,801) ia                        
C                                                                       
C     Subroutine SWINGM
C
C       L1 = scratch files (INPUT and INITAL)
C       L2 = scratch files (INPUT and INITAL)
C       L3 = Powerflow base data file                  
C       L5 = Swing input data file
c       L6 = Swing printout file
C       L8 = Swing history file    
C                                                                       
 101  if (filefl .ne. ' ') then
         status = open_file (l5, ctrlfl, 'F', 'R', iostat)
         if (status .ne. 0) then
            error_code = 1
            call set_exit(error_code)
         endif
      else
         status = open_file (l5, ' ', 'F', 'R', iostat)
      endif
      if (status .eq. 0) then
         inquire (unit=l5, name=ctrlfl)
      endif
      return                                                            

 201  if (filefl .ne. ' ') then
         status = open_file (l3, bsefl, 'U', 'R', iostat)
         if (status .ne. 0) then
            error_code = 2
            call set_exit(error_code)
         endif
      else
         status = open_file (l3, ' ', 'U', 'R', iostat)
      endif
      if (status .eq. 0) then
         inquire (unit=l3, name=bsefl)
      endif
      if (filefl .ne. ' ') then                                         
        open (unit=l8, file=solfl, status = type_open,                      
     &        access = 'DIRECT', 
c    &        recordtype = 'FIXED',                      
     &        form = 'UNFORMATTED', 
     &        recl = 4*128)                             
      else                                                              
        open (unit=l8, status = type_open,                                  
     &        access = 'DIRECT', 
c    &        recordtype = 'FIXED',                      
     &        form = 'UNFORMATTED', 
     &        recl = 4*128)                             
      endif                                                             
      if (status .eq. 0) then
         inquire (unit=l8, name=solfl)
      endif

c     End VAX VMS version

      call inithis ('WRIT')                                             
      if (debug) then                                                   
        call dbgeko2 ('OPENTS - simulation stage - opening solution ',  
     &    'file.')                                                      
        call dbgwri ('  L8 /unit num/ = ',l8)                           
        call dbgwrc ('  SOLFL /file name/ = ',solfl)                    
      endif                                                             
C                                                                       
C     For subroutine INPUT1                                              

      if (is_it_vms() .eq. 0) then
C        
C       UNIX version

        open(unit=l1, 
     &       status='scratch', 
     &       access='direct', 
c    &       disp='delete',       
     &       form='unformatted')
c    &       recordtype='fixed',                       
C    &       recordsize=4*4000)
c    &       organization='relative',                      
c    &       file = 'stab.scr01')

      else

c       VAX VMS version

        open(unit=l1, 
     &       status='scratch', 
     &       access='direct', 
c    &       disp='delete',       
     &       form='unformatted') 
c    &       recordtype='variable',                    
c    &       recordsize=4*4000) 
c    &       organization='relative',
c    &       file = 'scratch.dat')                                


      endif
C                                                                       
C     For subroutine INPUT3                                             
C                                                                       
      if (is_it_vms() .eq. 0) then
C        
C       UNIX version

        open(unit=l2, 
     &       status='scratch', 
     &       access='direct', 
c    &       disp='delete',       
     &       form='unformatted') 
c    &       recordsize=4*4065) 
c    &       recordtype='fixed',       
c    &       organization='relative', 
c    &       file = 'stab.scr02')                                        

      else

c       VAX VMS version
c

        open(unit=l2,
     &       status='scratch',
     &       access='direct',
c    &       disp='delete',       
     &       form='unformatted')
c    &       recordsize=4*4065)
c    &       recordtype='variable',    
c    &       organization='relative', 
c    &       file = 'junk.dat')                                  

      endif
      return                                                            
C                                                                       
C     Subroutine TAPEWK
C
C     L9  = Existing saved swing data file
C                                                                       
 301  if (filefl .ne. ' ') then
         status = open_file (l9, savifl, 'U', 'R', iostat)
         if (status .ne. 0) then
            error_code = 3
            call set_exit(error_code)
         endif
      else
         status = open_file (l9, ' ', 'U', 'R', iostat)
      endif
      if (status .eq. 0) then
         inquire (unit=l9, name=savifl)
      endif
      return                                                            

C     Subroutine TAPEWK
C
C     L15 = New saved swing data file

 401  if (filefl .ne. ' ') then
         status = open_file (15, savofl, 'U', 'W', iostat)
         if (status .ne. 0) then
            error_code = 4
            call set_exit(error_code)
         endif
      else
         status = open_file (15, ' ', 'U', 'W', iostat)
      endif
      if (status .eq. 0) then
         inquire (unit=15, name=savofl)
      endif
      return                                                            
C                                                                       
C     Subroutine NOUT1 - plotting phase
C
C     L3 = Powerflow base data file                  
C       L8 = Swing history file    
c       L11 = auxiliary (tabular) listing file   
c       L22 = PDF plot neutral file.
C       L23 = PostScript Master file  
C                                                                       
 501  if (filefl .ne. ' ') then
         status = open_file (l3, bsefl, 'U', 'R', iostat)
         if (status .ne. 0) then
            error_code = 5
            call set_exit(error_code)
         endif
      else
         status = open_file (l3, ' ', 'U', 'R', iostat)
      endif
      if (status .eq. 0) then
         inquire (unit=l3, name=bsefl)
      endif
      if (filefl .ne. ' ') then                                         
         open (unit=l8, 
     &         file=solfl, 
     &         status = 'old',                      
     &         access = 'direct', 
c    &         recordtype = 'fixed',                      
     &         form = 'unformatted', 
     &         recl = 4*128)                             
      else                                                              
        open (unit=l8, 
     &        status = 'old',                                  
     &        access = 'direct', 
c    &        recordtype = 'fixed',                      
     &        form = 'unformatted', 
     &        recl = 4*128)                             
      endif                                                             
      if (status .eq. 0) then
         inquire (unit=l8, name=solfl)
      endif
      if (filefl .ne. ' ') then
         status = open_file (l11, auxfl, 'F', 'W', iostat)
         if (status .ne. 0) then
            error_code = 6
            call set_exit(error_code)
         endif
      else
         status = open_file (l11, ' ', 'F', 'W', iostat)
      endif
      if (status .eq. 0) then
         inquire (unit=l11, name=auxfl)
      endif
      if (filefl .ne. ' ') then
         status = open_file (l22, pltfl, 'F', 'W', iostat)
         if (status .ne. 0) then
            error_code = 7
            call set_exit(error_code)
         endif
      else
         status = open_file (l22, ' ', 'F', 'W', iostat)
      endif
      if (status .eq. 0) then
         inquire (unit=l22, name=pltfl)
      endif
c
c     PostScript master file POSTMSTR is optional
c
      if (filefl .ne. ' ') then
         status = open_file (l23, postmstr, 'F', 'R', iostat)
c        if (status .ne. 0) then
c           error_code = 7
c           call set_exit(error_code)
c        endif
      else
         status = open_file (l23, ' ', 'F', 'R', iostat)
      endif
      if (status .eq. 0) then
         inquire (unit=l23, name=postmstr)
      endif
      call inithis ('READ')                                             

      if (debug) then                                                   
        call dbgeko2 ('OPENTS - plotting stage - opening solution',     
     &    ' & plot files.')                                             
        call dbgwri ('  L3 /unit num/ = ',l3)                           
        call dbgwrc ('  BSEFL /file name/ = ',bsefl)                    
        call dbgwri ('  L8 /unit num/ = ',l8)                           
        call dbgwrc ('  SOLFL /file name/ = ',solfl)                    
        call dbgwri ('  L22 /unit num/ = ',l22)                         
        call dbgwrc ('  PLTFL /file name/ = ',pltfl)                    
        call dbgwri ('  L23 /unit num/ = ',l23)                         
        call dbgwrc ('  POSTMSTR /file name/ = ',postmstr)                    
        call dbgwri ('  L11 /unit num/ = ',l11)                         
        call dbgwrc ('  AUXFL /file name/ = ',auxfl)                    
      endif                                                             
      return                                                            
C                                                                       
C     Subroutine PRGMON
C                                                                       
 601  return

c     Open printout file 
C
c       L6 = Swing printout file
C       L13 = Debug output file  

 701  if (filefl .ne. ' ') then
         status = open_file (l6, prtfl, 'FF', 'W', iostat)
         if (status .ne. 0) then
            error_code = 8
            call set_exit(error_code)
         endif
      else
         status = open_file (l6, ' ', 'FF', 'W', iostat)
      endif
      if (status .eq. 0) then
         inquire (unit=l6, name=prtfl)
      endif
      if (filefl .ne. ' ') then
         status = open_file (l13, dbgofl, 'F', 'W', iostat)
         if (status .ne. 0) then
            error_code = 6
            call set_exit(error_code)
         endif
      else
         status = open_file (l13, ' ', 'F', 'W', iostat)
      endif
      if (status .eq. 0) then
         inquire (unit=l13, name=dbgofl)
      endif
      return 

c     Open file_list file 
c     Note: file list file contains file names.

 801  status = open_file (l1, filefl, 'F', 'R', iostat)
      if (status .ne. 0) then
         error_code = 9
         call set_exit(error_code)
      endif
      if (status .eq. 0) then
         inquire (unit=l1, name=filefl)
      endif
      return                                                            
      end                                                               
