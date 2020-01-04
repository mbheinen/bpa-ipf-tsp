
      integer MAXPIBUS, MAXPICBUS, MAXPIBACK, MAXPIBRN
      parameter (MAXPIBUS = 1500)
      parameter (MAXPICBUS = 8000)
      parameter (MAXPIBACK = 4000)
      parameter (MAXPIBRN = 8000)
      common /cut/ kpibak, mpibak, kpipar, kint, jbrt, jcbt, kdebug,
     &             nwscc, lpiprl(7,MAXPIBRN), kbuspi(17,MAXPIBUS),
     &             ebrnch(18,MAXPIBRN), jbrseq(MAXPIBRN), 
     &             ebctbl(12,MAXPICBUS), lpibak(3,MAXPIBRN), 
     &             mpsort(2,2*MAXPIBRN), intflg(2,100)
      dimension pibak(3,MAXPIBRN), piparl(7,MAXPIBRN), 
     &          buspi(17,MAXPIBUS), jbrnch(18,MAXPIBRN), 
     &          jbctbl(12,MAXPICBUS)
      equivalence (pibak,lpibak), (piparl,lpiprl), (buspi,kbuspi),
     &            (ebrnch,jbrnch), (ebctbl,jbctbl)

