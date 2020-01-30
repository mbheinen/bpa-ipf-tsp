C    %W% %G%
      subroutine vymds(i1)                                              
c                                                                       
c       THIS SUBROUTINE SOLVES THE DIFFERENTIAL EQUATIONS ASSOCIATED    
c       WITH THE VARIABLE ADMITTANCE MODULATION MODEL                   
c                                                                       
      include 'tspinc/params.inc' 
      include 'tspinc/comvar.inc' 
      include 'tspinc/lnk12.inc' 
      include 'tspinc/vrgov.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/vy1.inc' 
      include 'tspinc/busvolt.inc' 
      include 'tspinc/vym.inc' 
      include 'tspinc/znox.inc' 
      include 'tspinc/ecsind.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/vymn.inc'

      complex pymx4, pymx5

      data pi/3.1415927/
c
c     for 2nd-order block
c
c      IIII=1  modified
c      IIII=2  new 
c      IIII=3  original
c
       iiii=1
c
      if (lppwr .ne. 0) go to  500
c
c     separate different controls
c
      itcsc=iranityp(i1)
      goto(3100,3200,3300),itcsc
c
c     GE TCSC control only
c
 3100 vyinp0=dangl(i1)
      goto 3500
c
c     BPA RANI only
c
 3200 vyinp0=vympo(i1)
      goto 3500
c
c     other control
c    
 3300 continue
 3500 continue
C
c      
c       CALCULATE STATE VECTORS FROM LAST TIME STEP
c      
      vymx1 = (vyinp0*vymar(i1)+vymhbe(i1))/vymare(i1)
      vymx2 = (vymx1*vymarf0(i1) + vymhbf(i1))/vymarf(i1)
      oldx3 = vymx3(i1)                                                 !pdolan
      vymx3(i1) = (vymx2*vymarg(i1) + vymhbg(i1))/vymarh(i1)            vymds      24
      oldx4 = pymx4                                                     !pdolan
      if (togflag .eq. 1) then                                          !pdolan
        vymarza = vymar(i1) * vymca(i1)                                 !pdolan
        pymx4 = (vymx3(i1) * (vymarza+roota1) - oldx3 * (vymarza-roota1)!pdolan
     1        + oldx4 * (vymar(i1)-rootb1)) / (vymar(i1)+rootb1)        !pdolan
        oldx5 = pymx5                                                   !pdolan
        pymx5 = (pymx4 + oldx4 + oldx5 * (vymar(i1)-rootb2))
     &                / (vymar(i1) + rootb2)                            !pdolan
      endif                                                             !pdolan
c
      if (togflag .eq. 0) then                                          !pdolan
        pymx4 = (vymx3(i1)*(vymar(i1)+roota1)-oldx3*(vymar(i1)-roota1)  !pdolan
     1        + oldx4*(vymar(i1)-rootb1))/(vymar(i1)+rootb1)            !pdolan
       oldx5= pymx5                                                     !pdolan
       pymx5 = (pymx4*(vymar(i1)+roota2)-oldx4*(vymar(i1)-roota2)       !pdolan
     1       + oldx5*(vymar(i1)-rootb2))/(vymar(i1)+rootb2)             !pdolan
      endif                                                             !pdolan
      vymx4(i1)=real(pymx4)                                             !pdolan
      vymx5(i1)=real(pymx5)                                             !pdolan
c
c     added by Yu Wang for temp use to test 2nd-order block
c
      iosci=0
      if (iosci.eq.1) then
        www0=2.0*3.1415926*0.3
        if (tnx.ge.30.0) then
          vymx3(i1)=0.07*cos(www0*(tnx-30.0)/60.0)
        else
          vymx3(i1)=0.0
        endif
      endif
c
c
c     vymx5(i1) = (vymx3(i1)*vymab(i1) + vymbo(i1))/vymdc(i1)
c     vymx4(i1) = (vymcb(i1)*vymx3(i1) - vymcd(i1)*vymx5(i1)
c    1             + vymhb4(i1))/vymar(i1)
c
c     modified by Yu Wang
c
      if (iiii.eq.2) then
        vymx4(i1)=vymar(i1)*frqbse*(vymca0(i1)*vymx3(i1)-
     1          -vymx5(i1))+vymhb4(i1)
      endif
C
c     added by Yu Wang for RANI test
      if (lppwr .eq. 0) then
         write(51,2004)tnx-1.0,vyinp0*57.2958
         write(52,2004)tnx-1.0,vymx1  
         write(53,2004)tnx-1.0,vymx2
         write(54,2004)tnx-1.0,vymx3(i1)
      endif
c     if (i1.eq.2) then
c        write(61,2000)vyinp0
c        write(62,2000)vymx1
c        write(63,2000)vymx2
c        write(64,2000)vymx3(i1)
c        i1=2
c        write(65,2000)vymx4(i1)
c        write(66,2000)vymx5(i1)
c     endif
 2000 format(1x,1pe13.5) 
c       
c      
c        MODIFY TIME FACTORS IF THE TIME STEP HAS CHANGED
c      
      if (al .ne. 0.0) then
         vymar(i1)  = vymar(i1)*tfac
         vymare(i1) = vymare(i1)*tfac - tfac + 1.
         vymarf0(i1)=vymarf0(i1)*tfac - vymcf(i1)*tfac * vymcf(i1)
         vymarf(i1) = vymarf(i1)*tfac - tfac + 1.
         vymarg(i1) = vymarg(i1)*tfac - vymce(i1)*tfac + vymce(i1)
         vymarh(i1) = vymarh(i1)*tfac - tfac + 1.
C
           vymab(i1)  = (vymca0(i1) + vymca(i1)/vymar(i1)
     1              + vymcb(i1)/(vymar(i1)*vymar(i1)))
           vymdc(i1)  = (1. + vymcc(i1)/vymar(i1)
     1              + vymcd(i1)/(vymar(i1)*vymar(i1)))
C
      endif
c      
c       AT A DISCONTINUITY INPUT STATE VECTORS MUST BE
c       RE INITITALIZED
c      
      if (idsw .eq. 3 .or. idsw .eq. 5) then
         eg1 = eyr(iznbus(i1))
         fg1 = eyi(iznbus(i1))
         eg2 = eyr(jznbus(i1))
         fg2 = eyi(jznbus(i1))
         zng = zngij(i1)
         znb = znbij(i1)
         c1r = eg1*zng - fg1*znb
         c1i = eg1*znb + fg1*zng
         c2r = eg2*zng - fg2*znb
         c2i = eg2*znb + fg2*zng
         ctr = c1r-c2r
         cti = c1i-c2i
c        zng = zngij(i1) + zngii(i1)
c        znb = znbij(i1) + znbii(i1)
c        c1r=eg1*zng - fg1*znb
c        c1i=eg1*znb + fg1*zng
c        c2r=eg2*zngij(i1) - fg2*znbij(i1)
c        c2i=eg2*znbij(i1) + fg2*zngij(i1)
c        ctr=c1r-c2r
c        cti=c1i-c2i
C        update GE signal
         if (iranityp(i1).eq.1)call gesigl(eg1,fg1,ctr,cti,i1)         
         if (ivymsw(i1) .eq. 1) then
            vymp(i1) = ctr*eg1 + cti*fg1
         else
            vymp(i1) = sqrt(ctr*ctr + cti*cti)
         endif
         vympo(i1) = vymp(i1)
      endif
c      
c       CALCULATE PAST VALUE FACTORS
c      
      vymhbe(i1) = -vymar(i1)*vyinp0 - vymx1*(2. -vymare(i1))
      vymhbf(i1) = vymx1*(2.*vymcf(i1) - vymarf0(i1))
     1            - vymx2*(2. -vymarf(i1))
      vymhbg(i1) = vymx2*(2.*vymce(i1) - vymarg(i1))
     1            - vymx3(i1)*(2.- vymarh(i1))
      goto(5100,5200,5300),iiii
c
c     modified one by Yu Wang
c
5100  vymbo(i1) = vymx5(i1)*(2.0-vymdc(i1)) + vymx4(i1)*edt -      
     1            vymx3(i1)*(2.0*vymca0(i1)-vymab(i1))                
      vymhb4(i1) = vymx4(i1)*vymar(i1) + vymcb(i1)*vymx3(i1)           

     1            - vymcd(i1)*vymx5(i1)
      goto 5500
c
c     added by Yu Wang for 2nd-order block modofication
c
 5200 vymarr=vymar(i1)*vymar(i1)
      vymbo(i1)=(vymdc(i1)-2.0*vymcd(i1)/vymarr)*vymx5(i1)
     1          -(2.0/(vymar(i1)*frqbse))*vymx4(i1)
     2          -(vymab(i1)-2.0*vymcb(i1)/vymarr)*vymx3(i1)
      vymhb4(i1)=-vymar(i1)*frqbse*(vymca0(i1)*vymx3(i1)
     1           -vymx5(i1))-vymx4(i1)
      goto 5500
c    
c     origonal one
c
 5300 vymbo(i1)=vymx5(i1)*vymdc(i1)+vymx4(i1)*edt-
     1          vymx3(i1)*vymab(i1)
      vymhb4(i1)=vymx4(i1)*vymar(i1)+vymcb(i1)*vymx3(i1)
     1           - vymcd(i1)*vymx5(i1)
 5500 continue    
 
c
      d1 = 1./(vymare(i1)*vymarf(i1)*vymarh(i1)*vymdc(i1))
      vyma1(i1) = vymar(i1)*vymarg(i1)*vymab(i1)*d1
      xn1 = vymhbe(i1)*vymarg(i1)*vymab(i1)*d1
      xn2 = vymhbf(i1)*vymarg(i1)*vymab(i1)*vymare(i1)*d1
      xn3 = vymhbg(i1)*vymab(i1)/(vymarh(i1)*vymdc(i1))
      vyma2(i1) = xn1 + xn2 + xn3 + vymbo(i1)/vymdc(i1)
c      
c       CALCULATE LINE CURRENT OR POWER
c      
 500  eg1 = eyr(iznbus(i1))
      fg1 = eyi(iznbus(i1))
      eg2 = eyr(jznbus(i1))
      fg2 = eyi(jznbus(i1))
      zng = zngij(i1)
      znb = znbij(i1)
      c1r = eg1*zng - fg1*znb
      c1i = eg1*znb + fg1*zng
      c2r = eg2*zng - fg2*znb
      c2i = eg2*znb + fg2*zng
      ctr = c1r-c2r
      cti = c1i-c2i
c     zng = zngij(i1) + zngii(i1)
c     znb = znbij(i1) + znbii(i1)
c     c1r=eg1*zng - fg1*znb
c     c1i=eg1*znb + fg1*zng
c     c2r=eg2*zngij(i1) - fg2*znbij(i1)
c     c2i=eg2*znbij(i1) + fg2*zngij(i1)
c     ctr=c1r-c2r
c     cti=c1i-c2i
c
c     print out line flow MW and MVAr
c     (*.pdf seems not to give correct results)
c
      pplot=eg1*ctr+fg1*cti
      aiplot=sqrt(ctr*ctr+cti*cti)
      iyw=iznbus(i1)
      ee1=eyr(iyw)
      ff1=eyi(iyw)
c     VVVSC=SQRT(EE1*EE1+FF1*FF1)
c
c     update GE signal
c
      if (iranityp(i1).eq.1) call gesigl(eg1,fg1,ctr,cti,i1)
c
      aim=sqrt(ctr*ctr+cti*cti)           
      if (lppwr .eq. 0) write(59,2004) tnx-1.0, aim*115.47
 2004 format(1x,1pe15.3,3x,1pe15.3)  

      if (ivymsw(i1) .eq. 1) then
         vymp(i1) = ctr*eg1 + cti*fg1
      else
         vymp(i1) = sqrt(ctr*ctr + cti*cti)
      endif
      vympo(i1) = vymp(i1)
c
c     separate different controls
c     
      itcsc=iranityp(i1)
      if (itcsc .eq. 1) then

c        update GE signal
         vyinp=dangl(i1)

      else if (itcsc .eq. 2) then

c        update BPA RANI input
         vyinp=vymp(i1)

      else
c        other control
      endif
c      
c     CALCULATE STATE VECTORS
c      
      vymx5n = vyinp*vyma1(i1) + vyma2(i1)
      vymx3n = (vymx5n*vymdc(i1)-vymbo(i1))/vymab(i1)
      vymx4n = (vymcb(i1)*(vymx3n +vymx3(i1)) - vymcd(i1)*
     1          (vymx5n+vymx5(i1)))/vymar(i1) +vymx4(i1)
      vymmd(i1) = vymx5n*vymck(i1)
c
c     try other way to get more smooth result
c
      vymmd(i1) = vymx5(i1)*vymck(i1)
c
c     added by Yu Wang for RANI test
c     if (i1.eq.1)write(57,2000)pplot
c     if (i1.eq.1)write(67,2000)aiplot
C     WRITE(6,1000)VYMP(I1),VYMX3N,VYMX4N,VYMX5N,VYMMD(I1)
C1000 FORMAT('0',5X,'PIN, X3,X4,X5,XOUT ',4X,5F9.4)

      if (itcsc .eq. 2) then
        if (vymmd(i1) .gt. vymimx(i1)) then
           vymmd(i1) = vymimx(i1)
        else if (vymmd(i1) .lt. vymimn(i1)) then
           vymmd(i1) = vymimn(i1)
        endif
      endif

      vymdo(i1) = vymmd(i1)
      if (lppwr .eq. 0) then
         write(55,2004) tnx-1.0,vymmd(i1)      
         write(56,2004) tnx-1.0,vymx5(i1)
      endif
      write(67,2000)vymck(i1)     
      return
      end
