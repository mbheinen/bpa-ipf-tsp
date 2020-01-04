C    %W% %G%
	subroutine err_exit () 
c
c       This subroutine is called indirectly from a initializing
c       call to the c-function setjmp() and an error exit call to 
c       the c-function longjmp().
c
	include 'tspinc/prt.inc'
        include 'tspinc/blkcom1.inc'                                               
        include 'tspinc/files.inc'                                               

        common /error_code/ error_code
        integer error_code

        write (errbuf(1), 100) error_code
  100   format (' Program aborted for reasons given (', i2, ')')
        call prterr ('E',1)

        call close_file (l1)
        call close_file (l2)
        call close_file (l3)
        call close_file (15)
        call close_file (l6)
        call close_file (l8)
        call close_file (l9)
        call close_file (l11)
        call close_file (l22)
        call close_file (l23)
	
        return
        end
