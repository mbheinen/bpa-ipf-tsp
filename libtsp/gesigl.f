C    %W% %G%
        subroutine gesigl(vr,vi,air,aii,i)

C       This routine is used for calculating input signal
C       of GE TCSC control in BPA_TSP.
C
        include 'tspinc/params.inc'
        include 'tspinc/vrgov.inc'            
        include 'tspinc/vymn.inc'

        complex zsyn1,zsyn2,zsyn2p,vmeas,aimeas,vsyn1,vsyn2
C
C       The followings are some given data
C
c       estimation from GE report
c         ZSYN1=(0.0,0.15)
c         ZSYN2=(0.0,0.029)
c       estimation for method check
c          ZSYN1=(0.00066,0.04089)
c          ZSYN2=(0.00021,0.00881)
c
c       ZSYN1 and ZSYN2 were read in from RZ-G card 
c       IPARL was read in from RZ-D card
c
        riparl=real(iparl(i))
        xtcsc=xtcscge(i)/riparl 
        air=air*riparl
        aii=aii*riparl
        zsyn1=cmplx(0.0,zsyn1ge(i))
        zsyn2=cmplx(0.0,zsyn2ge(i))
c
C  
C       start to rotate reference angle and get Vmeas and Imeas
C
        vv=sqrt(vr*vr+vi*vi)
        vmeas=cmplx(vv,0.0)
        tha=atan2(vi,vr)
        aim=sqrt(air*air+aii*aii)
        thi0=atan2(aii,air)
        thi=thi0-tha
        aimeas=cmplx(aim*cos(thi),aim*sin(thi))
C
C       calculate synthesis voltages
C
c        XTCSC=XTCSCGE(I)
        zsyn2p=zsyn2+cmplx(0.0,xtcsc)
        vsyn1=vmeas+zsyn1*aimeas
        vsyn2=vmeas-zsyn2p*aimeas
C
C       CALCULATE EQUIVELANT ANGLE DIFERENCE
C        
        angle1=aimag(vsyn1)/real(vsyn1)
        angle2=aimag(vsyn2)/real(vsyn2)
        dangl(i)=(angle1-angle2)
c
c print out 
c
        write(61,2000)angle1
        write(62,2000)angle2
        vvsyn1=sqrt((real(vsyn1))**2+(aimag(vsyn1))**2)
        vvsyn2=sqrt((real(vsyn2))**2+(aimag(vsyn2))**2)
        write(63,2000)vvsyn1
        write(64,2000)vvsyn2
 2000   format(1x,1pe13.5)
c
c       sin signal for control loop adjustment
c
c       NOTES:  [1] let IOSCI=0 if you need prog back to normal
c               [2] when IOSCI=1, the prog use this sin input
c                   signal.   Let XXC=0, if may be easier to adjust
c                   control para.
        iosci=0
        if(iosci.ne.1) goto 1000 
          www0=2.0*3.1415926*0.3
          dangl(i)=dangl0
          if(tnx.ge.31.000) then
            dangl(i)=0.5*sin(www0*(tnx-31.00)/60.0)+dangl0
          endif                  
c
c
 1000   return
        end
