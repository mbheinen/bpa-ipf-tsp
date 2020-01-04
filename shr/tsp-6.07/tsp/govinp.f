C    %W% %G%
      subroutine govinp(ign, line, bmva)
c     reads the governor cards

      implicit none

      include 'tspinc/params.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/gov.inc'

c     ign is the generator number. i and j are local counting variables.
c     igv is the index to the govdat array
c     bmva is the base mva for the run (defaults to 100.0)

      real    bmva
      integer ign, i, j, igv

      real temp(18)
      character line*80, type*1, subtyp*1, subtyp2*1, cardtype*3,
     &          templine*80

      read (line, '(3a1)') type, subtyp, subtyp2
      cardtype = type//subtyp

c     clear out residual values in temp vector
      do j = 1, 18
        temp(j) = 0.0
      enddo

      if (subtyp2.eq.' '.or.subtyp2.eq.'1') then

c       assign govdat array offset index for this governor
        govindx(ign) = govoffset 

c       assign governor offset for local use
        igv = govindx(ign)

c       check for near overflow of govdat array
        if (govoffset+50.gt.GOVSLOTS*MAXGEN) then
          errbuf(1) = 'Overflow of govdat array.  Increase GOVSLOTS.'
          call prterr('E', 1)
          stop
        endif

        if (cardtype.eq.'GS') then
          read (line, 10010)  (temp(j), j = 1, 8)
10010     format(bz, 16x, 2f6.1, 4f5.3, 2f6.1)

c         govdat values for GS:
c          1  pmax                 2  pmin                 3  r
c          4  2*T1/dt + 1          5  2*T2/dt + 1          6  1.0/T3
c          7  VelOpen              8  VelClose             9  Phigh
c         10  Plow                11  Iblock              12  db1  
c         13  e                   14  db2                 15  Ngv
c         16  2*Tt/dt + 1         17  Pgo                 18  hx12
c         19  hxint               20  hxtrans             21  x4 
c         22  x5                  23  genpo               24  dfreqpast
c         25  x1past              26  x3past              27  x4past

c         save governor type by index: 1=GS, 2=GH, 3=GP, 4=G2, 5=GW
          govtyp(ign) = 1

c         save govoffset for next governor.  Note that the space
c         allocated for the GS governor can be increased by increasing
c         this number.
          govoffset = igv + 27


c         save source of governor data by index: NOT IMPLEMENTED 
c         govsource(ign) =

c         save data into govdat
          govdat(igv+ 1) =  temp(1)/bmva
          govdat(igv+ 2) =  temp(2)/bmva
          govdat(igv+ 3) =  temp(3)/govdat(igv+1)
          govdat(igv+ 4) =  temp(4)              ! just store T values now and
          govdat(igv+ 5) =  temp(5)              ! adjust in govint
          govdat(igv+ 6) =  temp(6)
          govdat(igv+ 7) =  temp(7)*govdat(igv+1)
          govdat(igv+ 8) = -temp(8)*govdat(igv+1)
          do j = 9, 16
            govdat(igv+j) = 0.0
          enddo

        else if (cardtype.eq.'GH') then
          read (line, 10020)  (temp(j), j = 1, 9)
10020     format(bz, 16x, f6.1, 8f5.3)
c         govdat values for GH:
c          1  pmax                 2  pmin                 3  r
c          4  Tg                   5  2*Tp/dt + 1          6  2*Td/dt + 1
c          7  VelOpen              8  VelClose             9  Phigh
c         10  Plow                11  Iblock              12  db1  
c         13  e                   14  db2                 15  Ngv
c         16  2*Tt/dt + 1         17  Pgo*r               18  Dd
c         19  2*Dd*Td/dt          20  hx23                21  hx34
c         22  hx57                23  hxtran              24  x5
c         25  x8                  26  genpo               27  a1
c         28  a2                  29  dfreqpast           30  x1past
c         31  x4past              32  x5past

c         save governor type by index: 1=GS, 2=GH, 3=GP, 4=G2, 5=GW
          govtyp(ign) = 2

c         save govoffset for next governor.
          govoffset = igv + 32

c         save source of governor data by index: NOT IMPLEMENTED 
c         govsource(ign) =

c         save data into govdat
          govdat(igv+ 1) =  temp(1)/bmva
          govdat(igv+ 2) = -temp(1)/bmva
          govdat(igv+ 3) =  temp(2)/govdat(igv+1)
          govdat(igv+ 4) =  temp(3)/govdat(igv+1)
          govdat(igv+ 5) =  temp(4)   
          govdat(igv+ 6) =  temp(5)
          govdat(igv+ 7) =  temp(7)*govdat(igv+1)
          govdat(igv+ 8) = -abs(temp(8))*govdat(igv+1)
          govdat(igv+18) =  temp(9)/govdat(igv+1)
          do j = 9, 16
            govdat(igv+j) = 0.0
          enddo

c         if temp(6).ne.0 then set up turbine model
          if (temp(6).gt.1.0e-6) then
            templine = 'TW' // line(3:21)
            if (temp(6) .ge. 9.99 .or. temp(6) .lt. -0.99) then 
               write (templine(22:), 1000) '-2.00', temp(6), 3.0
 1000          format(a5, 5x, f5.2, f5.3)
            else
               write (templine(22:), 1010) '-2.00', temp(6), 3.0
 1010           format(a5, 5x, 2f5.3)
            endif
            call turbinp(ign, templine)
          endif
            
c ********************************************************************
        else if (cardtype.eq.'GP') then
          read (line, 10030)  (temp(j), j = 1, 12)
10030     format(bz, 16x, 2f6.1, 4f5.3, 2f6.1, 4f5.0)
c         govdat values for GP:
c          1  pmax                 2  pmin                 3  r
c          4  2*Td/dt + 1          5  2*Tf/dt + 1          6  2*Tp/dt + 1
c          7  VelOpen              8  VelClose             9  Phigh
c         10  Plow                11  Iblock              12  db1  
c         13  e                   14  db2                 15  Ngv
c         16  2*Tt/dt + 1         17  Pgo*r               18  Kp
c         19  Kd                  20  Ki                  21  Kg  
c         22  hx23                23  hx34                24  hx35     
c         25  hx89                26  genpo               27  hx910    
c         28  a1                  29  a2                  30  a3
c         31  a4                  32  x6                  33  x11
c         34  x12                 35  hxtrans             36  dfreqpast
c         37  x1past              38  x10past             39  x11past


c         save governor type by index: 1=GS, 2=GH, 3=GP, 4=G2, 5=GW
          govtyp(ign) = 3

c         save source of governor data by index: NDY
c         govsource(ign) =

c         save govoffset for next governor.
          govoffset = igv +  39

c         save data into govdat
          govdat(igv+ 1) =  temp(1)/bmva
          govdat(igv+ 2) =  temp(2)/bmva
          govdat(igv+ 3) =  temp(3)/govdat(igv+1)
          govdat(igv+ 4) =  temp(4)
          govdat(igv+ 5) =  temp(5)
          govdat(igv+ 6) =  temp(6)
          govdat(igv+ 7) =  temp(7)*govdat(igv+1)
          govdat(igv+ 8) = -abs(temp(8))*govdat(igv+1)
          govdat(igv+18) =  temp( 9)*govdat(igv+1)
          govdat(igv+19) =  temp(10)*govdat(igv+1)
          govdat(igv+20) =  temp(11)*govdat(igv+1)
          govdat(igv+21) =  temp(12)

c         zero all GP2 values
          do j = 9, 16
            govdat(igv+j) = 0.0
          enddo

c ********************************************************************
        else if (cardtype.eq.'G2') then
          read (line, 10040)  (temp(j), j = 1, 12)
10040     format(bz, 16x, 2f6.1, 4f5.3, 2f6.1, 4f5.0)
c         govdat values for GP:
c          1  pmax                 2  pmin                 3  r
c          4  2*Td/dt + 1          5  2*Tf/dt + 1          6  2*Tp/dt + 1
c          7  VelOpen              8  VelClose             9  Phigh
c         10  Plow                11  Iblock              12  db1  
c         13  e                   14  db2                 15  Ngv
c         16  2*Tt/dt + 1         17  Pgo*r               18  K1
c         19  K2                  20  Ki                  21  Kg  
c         22  hx12                23  hx23                24  hx24     
c         25  hx45                26  genpo               27  hx78     
c         28  hx1011              29  hx1112              30  a1
c         31  a2                  32  a3                  33  a4
c         34  x6                  35  x8                  36  x13     
c         37  x14                 38  hxtrans             39  dfreqpast
c         40  x1past              41  x12past             42  x13past

c         save governor type by index: 1=GS, 2=GH, 3=GP, 4=G2, 5=GW
          govtyp(ign) = 4

c         save source of governor data by index: NDY
c         govsource(ign) =

c         save govoffset for next governor.
          govoffset = igv +  42

c         save data into govdat
          govdat(igv+ 1) =  temp(1)/bmva
          govdat(igv+ 2) =  temp(2)/bmva
          govdat(igv+ 3) =  temp(3)/govdat(igv+1)
          govdat(igv+ 4) =  temp(4)
          govdat(igv+ 5) =  temp(5)
          govdat(igv+ 6) =  temp(6)
          govdat(igv+ 7) =  temp(7)*govdat(igv+1)
          govdat(igv+ 8) = -abs(temp(8))*govdat(igv+1)
          govdat(igv+18) =  temp( 9)
          govdat(igv+19) =  temp(10)
          govdat(igv+20) =  temp(11)*govdat(igv+1)
          govdat(igv+21) =  temp(12)

c         zero all G22 values
          do j = 9, 16
            govdat(igv+j) = 0.0
          enddo

c ********************************************************************
        else if (cardtype.eq.'GW') then
          read (line, 10050)  (temp(j), j = 1, 6)
10050     format(bz, 16x, 2f6.1, 4f5.3)
c         govdat values for GW:
c          1  pmax                 2  pmin                 3  r
c          4  2*T1/dt + 1          5  2*T2/dt + 1          6  2*T3/dt + 1
c          7  Pgo                  8  hx12                 9  hx23


c         save governor type by index: 1=GS, 2=GH, 3=GP, 4=G2, 5=GW
          govtyp(ign) = 5

c         save source of governor data by index: NOT IMPLEMENTED
c         govsource(ign) =

c         save govoffset for next governor.
          govoffset = igv + 9

c         save data into govdat
          govdat(igv+1) =  temp(1)/bmva
          govdat(igv+2) =  temp(2)/bmva
          govdat(igv+3) =  temp(3)/govdat(igv+1)
          govdat(igv+4) =  temp(4)              ! just store T values now and
          govdat(igv+5) =  temp(5)              ! adjust in govint
          govdat(igv+6) =  temp(6)              !

        endif

      else if (subtyp2.eq.'2'.and.cardtype.ne.'GW') then
        read (line, 10060)  (temp(j), j = 1, 8)
10060   format(bz, 16x, 2f6.1, 4f5.3, 1f6.1, 21x, 1f5.0)

        igv = govindx(ign)

c       store data in govdat
        govdat(igv+ 9) = temp(1)/bmva
        govdat(igv+10) = temp(2)/bmva
        govdat(igv+11) = temp(3)
        govdat(igv+12) = temp(4)/60.0
        govdat(igv+13) = temp(5)/60.0
        govdat(igv+14) = temp(6)/bmva
        govdat(igv+15) = temp(7)
        govdat(igv+16) = temp(8)

      endif

c     assign govname
      govname(ign) = line(4:11)

      return
      end
