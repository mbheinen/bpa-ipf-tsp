C    %W% %G%
      subroutine whrec1 (recnam, lowrec, hihrec, ndrecs)
      implicit none
      character recnam*3
      integer lowrec, hihrec, ndrecs
c    
c     Subroutine to determine the record to write to in D-A file unit
c     1 for temp data storage.  
c     Arguments
c          Recnam (input) is a 3 char data type (e.g. 'MAC' or 'LSH') 
c          Lowrec (output) is rec # to hold data counter. 
c          Hihrec (output) is rec # to hold data cntr in dup storage 
c            area.
c          Ndrecs (output) is the number of records in the file 
c            following _lowrec_ allowed for storing that type of data
c
c
      include 'tspinc/params.inc'
      include 'tspinc/prt.inc'

c     These local vars replace the MASOLD array

      integer MAX_TYPES
      parameter (MAX_TYPES = 11)

      character drnam1(MAX_TYPES)*3                                     !dem
      integer darec1(MAX_TYPES), drsiz1(MAX_TYPES)                      !dem
      integer hirec1, la, i

      integer MXLREP
      parameter (MXLREP = MAXBUS / 5)

      save

      data drnam1 / 'MAC', 'LDA', 'LDZ', 'LDB', 'LN ', 'LSH', 'RG ',
     &              'RD ', 'RR ', 'DC ', 'MSR' /                                             !dem
c     
c     -     Begin     Begin     Begin     Begin     Begin     Begin

c     Assign values for the block lengths in the temp file                 
c     in 100 block packages.  Must assign one more block than is needed.
c     i.e. if you need 4000 slots for generator cards (including controls)
c     then drsiz1(1) must be set to 41. csw 6/93

c     machine cards (MAC)

      drsiz1(1) = (MAXMAC+99) / 100 + 1

c     nonlinear load rep by area (LDA)

      drsiz1(2) = (60+99) / 100 + 1

c     nonlinear load rep by zone (LDZ)

      drsiz1(3) = (150+99) / 100 + 1

c     nonlinear load rep by bus (LDB)

      drsiz1(4) = (MXLREP+99) / 100 + 1

c     load netting (LN)

      drsiz1(5) = (MAXGEN+99) / 100 + 1

c     load shedding (LSH)

      drsiz1(6) = (MXLSHD+99) / 100 + 1

c     series capacitor gap relaying (RG)

      drsiz1(7) = 2*(100+99) / 100 + 1

c     local relay (RD)

      drsiz1(8) = 2*(200+99) / 100 + 1

c     remote relaying (RR)

      drsiz1(9) = 2*(100+99) / 100 + 1

c     d-c (DC)

      drsiz1(10) = 6

C     RB AND RC CARDS (MSR)

      drsiz1(11) = 5

c     computing position of each first record:

      darec1(1) = 1
      do i = 2, MAX_TYPES
        darec1(i) = darec1(i-1) + drsiz1(i-1)
      end do
      hirec1 = darec1(MAX_TYPES) + drsiz1(MAX_TYPES)

      do la = 1, MAX_TYPES
        if (recnam .eq. drnam1(la)) then
          lowrec = darec1(la)
          hihrec = darec1(la) + hirec1
          ndrecs = drsiz1(la) - 1
          return
        endif
      enddo

c     here if unmatched data type string

      write (errbuf(1),'(3a)') 'WHREC1 - Unknown temporary storage',
     +  ' type: ',recnam
      call prterr ('E',1)
      lowrec = 0
      hihrec = 0
      ndrecs = 0
      return
      end
