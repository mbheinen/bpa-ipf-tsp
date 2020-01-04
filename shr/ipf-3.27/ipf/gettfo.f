C    @(#)gettfo.f	20.4 9/10/96
      subroutine gettfo (jt, pin, qin)
C                                                                  
C     This subroutine computes the line flow for area tie lline JT.
C                                               
C     Input parameters:                         
C                                               
C     JT     - index to TIE.                    
C                                               
C     Output Parameters:                        
C                                               
C     PIN - Flow (MW) in tie line.
C     QIN - Flow (MVAR) in tie line.
C                                               
      include 'ipfinc/parametr.inc'

      include 'ipfinc/arcntl.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/dc2t.inc'
      include 'ipfinc/dcmt.inc'
      include 'ipfinc/ordsta.inc' 
 
 
      k1=tie(1,jt)
      k2=tie(7,jt)
      if (ordtie .eq. 2) then
        k1 = opt2inp(k1)
        k2 = opt2inp(k2)
      endif
      kdc=tie(9,jt)
 
      if (kdc .gt. 0) then
C
C       Check multi-terminal d-c array.
C
        do mdc=1,mtdcln
 
          m1=dcmtln(1,mdc)
          m2=dcmtln(2,mdc)
          if (min0(m1,m2) .ne. min0(k1,k2)) go to 160
          if (max0(m1,m2) .eq. max0(k1,k2)) then
            if (m1.eq.k1) then
              l1 = dcmtln(8,mdc)
              l2 = dcmtln(9,mdc)
            else
              l1 = dcmtln(9,mdc)
              l2 = dcmtln(8,mdc)
            endif
 
            v1=dcmtbs(20,l1)
            v2=dcmtbs(20,l2)
            pin = v1*(v1-v2)/dcmtln(4,mdc)
            qin = 0.0
            return
          endif
  160     continue
        enddo
C
C       Check 2-terminal d-c arrays
C
        do mdc=1,kdtot
 
          if (dc2t(1,mdc) .eq. k1 .and. dc2t(3,mdc) .eq. k2) then
 
            pin=0.001*dc2t(39,mdc)*dc2t(40,mdc)
            qin = 0.0
            return
 
          else if (dc2t(1,mdc) .eq. k2 .and. dc2t(3,mdc) .eq. k1) then
 
            pin = -0.001*dc2t(39,mdc)*dc2t(41,mdc)
            qin = 0.0
            return
 
          endif
  194     continue
        enddo 
        call erexit
 
      else
 
        if (ordvlt .eq. 1) then
          kt = k1
          mt = k2
        else
          kt = inp2opt(k1)
          mt = inp2opt(k2)
        endif
        ek = e(kt)
        fk = f(kt)
        em = e(mt)
        fm = f(mt)
        gk11 = tie(3,jt)
        bk11 = tie(4,jt)
        gk12 = tie(5,jt)
        bk12 = tie(6,jt)
        aik = ek*gk11-fk*bk11+em*gk12-fm*bk12
        bik = ek*bk11+fk*gk11+em*bk12+fm*gk12
        pin = (ek*aik+fk*bik)*bmva
        qin = (-ek*bik+fk*aik)*bmva
        return
 
      endif
      end
