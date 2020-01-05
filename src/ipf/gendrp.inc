
      integer MAXGENDRP 
      parameter (MAXGENDRP = 1200)
      common /gendrp/ gndpol(50), gndpmn(50), gndpmx(50),	
     1                genpol(MAXGENDRP), drptot, drptol, 
     2                numdrp, gndpno(50), gndpty(50), numgen,  	
     3                gennum(MAXGENDRP), gentyp(MAXGENDRP),    
     4                itdpmx, gensum_flag
c
c
      double precision gndpol, gndpmn, gndpmx, genpol, drptot, drptol
      integer gndpno, gennum, gndpty, gentyp, itdpmx, numdrp, numgen,
     &        gensum_flag
C
C     Definitions:
C
C     GNDPNO = array of dropped generators numbers.
C     GNDPOL = array of original value of dropped generation (MW).
C              (GeNerator DroPped OLd).
C     GNDPMN = array of minimum value of dropped generation (MW).
C     GNDPMX = array of maximum value of dropped generation (MW).
C     GNDPTY = array of generator types (0/n : normal/area interchange).
C     NUMDRP = counter of arrays GNDPNO and GNDPOL
C     GENNUM = array of allocable generators.
C     GENPOL = array of original value of allocable generation (MW).
C     GENTYP = array of generator types (0/n : normal/area interchange).
C     NUMGEN = counter of array GENNUM.
C     DRPTOT = total dropped (user-specified) (MW).
C     DROPTL = tolerance of dropped generation (MW).
C     ITDPMX = maximum number of major iterations to resolve
C              DROPMW.
C     GENSUM_FLAG = 1/0 (Print/Do not print) Generation Drop Summary
C
