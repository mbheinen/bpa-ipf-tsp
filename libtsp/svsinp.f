C    %W% %G%
      subroutine svsinp(subtyp2)
C     
C     This subroutine decodes the SVS data cards and does
c     initial error checking of the data.  It is called by
c     INPUT3.
C     
      include 'tspinc/params.inc'
      include 'tspinc/igentn.inc'
      include 'tspinc/packtn.inc'
      include 'tspinc/comn34.inc'
      include 'tspinc/param.inc'
      include 'tspinc/svs.inc'
      include 'tspinc/inp3.inc'
      include 'tspinc/inp3a.inc'
      include 'tspinc/lnk33n.inc'
      include 'tspinc/tzro.inc'
      include 'tspinc/prt.inc'

      dimension temp(16)
      character*8 nhibus, nfqbus, nvfbus
      character*1 subtyp2

c     Record could be either VN, VN2, VW or VW2.  Process them 
c     separately.

      if (subtyp2 .eq. ' ' .or. subtyp2 .eq. '1') then
c
c       Blank out "A" and "B" fields if both are "1".
c
        if (work80(icard)(31:32) .eq. '11') work80(icard)(31:32) = ' '
        read (work80(icard), 10000) 
     &      (temp(i), i = 1,4), (temp(i),  i= 7,16), nhibus, bkv1
10000   format(bz,16x, 2f3.3, 2f4.4, f5.3, f3.3, 2f4.0, 2f4.3, 2f4.2, 
     &   2f3.3, a8, f4.0)

c       temp(1)	  -	Ts1		temp(2)	  -	Vemax	
c	temp(3)   -	Ts2		temp(4)   -	Ts3
c	temp(7)	  -	Ts4		temp(8)	  -	Ts5
c	temp(9)	  -	Ksvs		temp(10)  -	Ksd
c	temp(11)  -	Bmax		temp(12)  -	B'max
c	temp(13)  -	B'min		temp(14)  -	Bmin
c	temp(15)  -	Ts4		temp(16)  -	DV
c       nhibus    -     Remote bus name bkv1      -	Rem. bus KV

c       Assign remote bus name and kv if there is one
        if (nhibus .eq. '        ') then
          jbus = 0
        else
          kb = nambas(bkv1)
          jbus = inam(nhibus, kb)
        endif


c       Write error and abort if ts3 <= zero
        if (temp(4) .le. 0.0) then
          write (errbuf(1), 10050) nbname, bkv, nid
10050     format ('0', 2x, a8, 2x, f5.1, 2x, a1, 5x, 
     &     'TS3 MUST BE GREATER THAN ZERO')
          call prterr('E', 1)
          imchn = 1
        endif

c       Write warning if ksvs <= zero
        if (temp(9) .le. 0.0) then
          write (errbuf(1), 10070) nbname, bkv, nid
10070     format ('0', 2x, a8, 2x, f5.1, 2x, a1, 5x, 
     &     'KSVS IS NOT GREATER THAN ZERO')
          call prterr('W', 1)
        endif

        pold = 1.0
        qold = 1.0

c       Process svc if it passed all the tests
        if (imchn .ne. 1) then

c         Increment machine counter and svc counter
          isgg 	= isgg + 1
          ksv 	= ksv  + 1

c         Update total svc's loaded
          ksvtot = ksv

c         Assign name and kv of control bus
          kb 		= nambas(bkv)
          jbusm 	= inam(nbname, kb)

c         Assign svstyp
          svstyp(ksv) = 1
          if (subtyp.eq.'W') svstyp(ksv) = 2

          igentn(1, isgg) = jbusm
          igentn(2, isgg) = 4
          igentc(isgg) 	= nid

          ipactn(2, jbusm) = isgg
          ipactn(1, jbusm) = 1

          icgenn(1, 1) 	= 9
          icgenn(2, 1) 	= ksv

          igr 	= 1
          mgen 	= 9
          igt 	= 6
          dv(ksv) = temp(16)
  
          if (dv(ksv) .eq. 0.0) then
            dvlo(ksv) = temp(12)/temp(9)
            dvhi(ksv) = temp(13)/temp(9)
          else
            dvlo(ksv) = dv(ksv)
            dvhi(ksv) =  - dv(ksv)
          endif

          isvno(ksv) 	= jbusm
          iptr(ksv) 	= isgg
          iremte(ksv) 	= jbus
          xcon 		= 2.0*frqbse/dtsvs

          as1(ksv) = xcon*temp(1)  + 1.
          as2(ksv) = xcon*temp(3)  + 1.
          as3(ksv) = xcon*temp(4)  + 1.
          as4(ksv) = xcon*temp(7)  + 1.
          as5(ksv) = xcon*temp(8)  + 1.
          as6(ksv) = xcon*temp(15) + 1.

          vmax(ksv) 	=  temp(2)
          vmin(ksv) 	= -temp(2)
          vemax(ksv) 	=  temp(2)
          vemin(ksv) 	= -temp(2)
          cksvs(ksv) 	=  temp(9)
          cksd(ksv) 	=  temp(10)
          bmax(ksv) 	=  temp(11)
          bpmax(ksv) 	=  temp(12)
          bpmin(ksv) 	=  temp(13)
          bmin(ksv) 	=  temp(14)
        endif

      else if (subtyp2.eq.'2') then

      read(work80(icard),10075) ( temp(i), i=1,8 )
10075 format(bz,16x, 8f6.0)

      if (vemax(ksv).lt.1.0e-6) then
        vmax(ksv)   = temp(1)
        vmin(ksv)   = temp(2)
        vemax(ksv)  = temp(3)
        vemin(ksv)  = temp(4)
      endif

      svsbias(ksv)= temp(5)
      dv2(ksv)    = temp(6)
      bshunt(ksv) = temp(7)
      tdelay(ksv) = temp(8)

      endif

      return
      end
