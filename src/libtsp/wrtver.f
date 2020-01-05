C    %W% %G%
      subroutine wrtver
c      
c     This subroutine writes the program version and compilation
c     date to the output file
c      
      include 'tspinc/prt.inc'
      include 'tspinc/blkcom1.inc'
      character compdt*8, compty*5
C     
C     Set particulars for this pgm update

      ver = '6.07'                                                     
      compdt = '12/11/99'                                              
      compty = 'OPTIM'                                                

C     Set up temp print spec so version # goes to log file        

      ictemp = crtsw                                                
      crtsw = 1                                                    
C     
      write (outbuf,100)  compty
 100  format  ('0', 5x, 'BPA Swing program - ',a5,' compiler')
      call prtout (1)
      write (outbuf,200) ver
 200  format (6x,'Version number: ',a13)
      call prtout (1)
      write (outbuf,300) compdt
 300  format (6x,'Compilation date: ',a8)
      call prtout(1)
      crtsw = ictemp                                                    !dem
      return
      end
