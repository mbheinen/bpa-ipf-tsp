C    %W% %G%
      subroutine vymdi
 
C     THIS SUBROUTINE CALCULATES THE INITAL VALUES FOR
C     THE VARIABLE ADMITTANCE MODULATION MODEL
 
      include 'tspinc/params.inc'
      include 'tspinc/param.inc'
      include 'tspinc/vym.inc'
      include 'tspinc/znox.inc'
      include 'tspinc/znox2.inc'
      include 'tspinc/vy1.inc'
      include 'tspinc/ecsind.inc'
      include 'tspinc/comn34.inc'
      include 'tspinc/namec.inc'
      include 'tspinc/busvolt.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/matrow.inc'
      include 'tspinc/spare1.inc'
      include 'tspinc/buskv.inc'
 
C     added by Yu Wang for TCSC test
      include 'tspinc/vymn.inc'
 
      character*8 name1, name2, name3, name4
 
      do i = 1, iznmax
        if (ivymsw(i) .ne. 0) then
          name1 = exnamc(iznbus(i))
          name2 = exnamc(jznbus(i))
 
C         OBTAIN ADMITTANCE FOR MONITORED LINE
 
          ibus = ivymb1(i)
          jbus = ivymb2(i)
          call getmat(ibus, ii)
          iii = ii
          do i1 = 4, iii, 3
            if (jbus .eq. matrow(i1)) goto 100
          enddo
          goto 110
  100     vymgij(i) =  - atrow(i1+1)
          vymbij(i) =  - atrow(i1+2)
 
C         CALCULATE  INITIAL LINE CURRENT
 
          eg1 = eyr(iznbus(i))
          fg1 = eyi(iznbus(i))
          eg2 = eyr(jznbus(i))
          fg2 = eyi(jznbus(i))
          zng = zngij(i) + zngii(i)
          znb = znbij(i) + znbii(i)
          c1r = eg1*zng - fg1*znb
          c1i = eg1*znb + fg1*zng
          c2r = eg2*zngij(i) - fg2*znbij(i)
          c2i = eg2*znbij(i) + fg2*zngij(i)
          ctr = c1r - c2r
          cti = c1i - c2i
C         ZNG = VYMGIJ(I)
C         ZNB = VYMBIJ(I)
C         C1R=EG1*ZNG - FG1*ZNB
C         C1I=EG1*ZNB + FG1*ZNG
C         C2R=EG2*ZNG - FG2*ZNB
C         C2I=EG2*ZNB + FG2*ZNG
C         CTR=C1R-C2R
C         CTI=C1I-C2I
          vymp(i) = sqrt(ctr*ctr+cti*cti)
          pptie0 = eg1*ctr + fg1*cti
 
C         INITIALIZE INPUT IVMYSW =1 IS LINE POWER
C         IVYMSW= 2 IS LINE CURRENT
 
          if (ivymsw(i) .eq. 1) vymp(i) = eg1*ctr + fg1*cti
          vympo(i) = vymp(i)
 
C         initialize GE control signal
 
          if (iranityp(i) .eq. 1) call gesigl(eg1, fg1, ctr, cti, i)
 
C         CALCULATE INITIAL PAST VALUE FACTORS
 
          itcsc = iranityp(i)
          if (itcsc .eq. 2) then
C           BPA-RANI
            vyinp0 = vymp(i)
          elseif (itcsc .eq. 3) then
C           other control
 
C           for TCSC control in SLATT with braking application
C           as disturbance
 
            ptie0(i) = pptie0
            ptie1(i) = pptie0
            iptie = 0
          else
C           GE TCSC control
            vyinp0 = dangl(i)
            dangl0 = dangl(i)
            tnxyw0 = 1.0
 
C           SEPARATE DIFFERENT CONTROL BLOCKER
            if (ibtyp(i) .eq. 2) then
 
              blok0(i) = pptie0
              p4blok0(i) = blok0(i)
              iblok(i) = 0
              blocker(i) = 0.0
            else
              blok0(i) = 0.
            endif
          endif
 
          vymhbe(i) =  - vyinp0*vymar(i)
 
          vymhbf(i) = 0.0
          vymhbg(i) = 0.0
          vymhb4(i) = 0.0
          vymhbf(i) = 0.0
          vymbo(i) = 0.0
          vyma1(i) = 0.0
          vyma2(i) = 0.0
          igeon(i) = 0
        endif
      enddo
      goto 120
  110 name3 = exnamc(ibus)
      name4 = exnamc(jbus)
      ib1 = ixnamn(ibus)
      ib2 = ixnamn(jbus)
      bkv3 = basekv(ib1)
      bkv4 = basekv(ib2)
      write (errbuf(1), 10000)
10000 format (5x, ' THE FOLLOWING LINE SPECIFIED ON THE RZ G CARD CAN '
     & , ' NOT BE FOUND IN THE DATA TABLES.')
      call prterr('E', 1)
      write (errbuf(1), 10010) name3, bkv3, name4, bkv4
10010 format (5x, 2(a8, 1x, f5.1, 2x))
      call prterr('E', 1)
      iabort = 1
  120 return
      end
