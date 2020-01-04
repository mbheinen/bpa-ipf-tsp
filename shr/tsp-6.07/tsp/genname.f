C    %W% %G%
      subroutine genname (gennum,fullnam)
        integer gennum
        character fullnam*16
C
C -  For a generator number, returns its full name as 
C    bus KV ID in a single character string:
C    ABCEDFGH 123.4 1 
C
      include 'tspinc/params.inc'
      include 'tspinc/lnk1a.inc'
      include 'tspinc/igentn.inc'
      include 'tspinc/namec.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/param.inc'
C     -  Local variables
      character busnam*8,genid*1
      real buskv
C   
C - For debugging data:
C    IGENTN[1,GENNUM] <- /IGENTN/ is bus numer (-> JBUS)
C    IGENTC[GENNUM] <- /IGENTC/ is gen ID
C    EXNAMC[JBUS] <- /NAMEC/ is bus name
C    BASEKV[IXNAMN[JBUS]] <- /NAMEN/, /POINTR/ is bus kv
C
C     -     Begin     Begin     Begin     Begin     Begin     Begin
      genid = igentc(gennum)
      jbus = igentn(1,gennum)
      busnam = exnamc(jbus)
      buskv = basekv(ixnamn(jbus)) 
      write (fullnam,'(A8,1X,F5.1,1X,A1)') busnam,buskv,genid
      return
      end
