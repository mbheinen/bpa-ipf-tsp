C    @(#)opsln3.f	20.3 2/13/96
      subroutine opsln3
C
C     driver to compute the following sensitivities
C
C     1. Bus sensitivities: dTheta/dP and dVolt/dQ.
C
C     2. Line sensitivities: dPij/dXt (Line Pij flow with repect to
C                                      change transfer reactance Xt).
C
C                            dPij/dBs (Line Pij flow with repect to 
C                                      change shunt admittance Bs).
C
C                            dLoss/dXt (System losses with repect to 
C                                      change transfer reactance Xt).
C
C                            dLoss/dBs (System losses with repect to 
C                                      change shunt admittance Bs).
C
C                            dVi/dXt (Bus voltage with repect to change
C                                     transfer admittance Xt).
C
C                            dVi/dBs (Bus voltage with repect to change
C                                     shunt admittance Bs).
C
C     3. Transfer sensitivities:
C
C     4. Loss sensitivities: dLOSS/dPi (Losses with respect to net real
C                                       generation).
C
C                            dLOSS/dQ (Losses with respect to net 
C                                      reactive generation).
C
C                            dLOSS/dV (Losses with respect to scheduled
C                                      voltage).
C
 
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/amtrx.inc'
      include 'ipfinc/beta2.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/gamma.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/lnsen.inc'
      include 'ipfinc/loscom.inc'
      include 'ipfinc/optim1.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/sensit.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/tran.inc'
      include 'ipfinc/transf.inc'
 
      external kompsv,swapsv
        
      if (fichsw .eq. 1) then   
         outbuf='$MFCB      SENSITIVITIES'  
         call pfomf(1)  
      endif 
  90    if (index (buf,'BUSSEN') .gt. 0 .or.
     1      index (buf,'BUS_SEN') .gt. 0) then  
           outbuf = 'BUS_SENSITIVITIES' 
           call rpnlod  
           nsen = 0 
           nlsen = 0
           ndsen = 0
           call seninp  
           if (nsen .gt. 0) call bussen 
           inrcd = buf  
           go to 90 
        else if (index (buf,'LINESE') .gt. 0 .or.   
     1           index (buf,'LINE_SE') .gt. 0) then 
           outbuf = 'LINE_SENSITIVITIES'
           call rpnlod  
           nsen = 0 
           nlsen = 0
           ndsen = 0
           call seninp  
           if (nlsen .gt. 0) call linsen
           inrcd = buf  
           go to 90 
        else if (index (buf,'TRANSF') .gt. 0) then  
           outbuf = 'TRANSFER_SENSITIVITIES'
           call rpnlod  
           numf = 0 
           numl = 0 
           numt = 0 
           call seninp  
           if (numt + numf + numl .gt. 0) then  
              call bldtrn   
              call trnsen   
           endif
           inrcd = buf  
           go to 90 
        else if (index (buf,'LOSSSEN') .gt. 0 .or.  
     1           index (buf,'LOSS_SEN') .gt. 0) then
           outbuf = 'LOSS_SENSITIVITIES'
           call rpnlod  
        
           numlsa = 0   
           numlsz = 0   
           call seninp  
           call lossen  
           go to 90 
        endif   
      return
      end   
