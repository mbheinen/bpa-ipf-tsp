      parameter (MAXLS = 200)
      common /linsw1/ iftabn(MAXLS),jftabn(MAXLS),ipcdtn(MAXLS),
     &     mflt(MAXLS), cyc(MAXLS),fltr(MAXLS),fltx(MAXLS),perct(MAXLS),
     &     ndcckt(MAXLS) ,dmcln(2,MAXLS),dmpln(2,MAXLS)
      common /linsw2/ iftabc(MAXLS),jftabc(MAXLS),ipcdtc(MAXLS),
     &     idgnc(MAXLS)
      dimension dmpg(MAXLS),dmpl(MAXLS),dmcl(MAXLS),dmzg(MAXLS),
     &     dmzb(MAXLS)
      equivalence (jftab,dmpg),(ipcdt,dmpl),(fltr,dmcl),(fltx,dmzg),
     &    (perct,dmzb)
      dimension dpiord(MAXLS), angblk(MAXLS), iftab(MAXLS), 
     &    jftab(MAXLS), ipcdt(MAXLS)
      equivalence (angblk, fltr), (dpiord, fltx), (iftab, iftabn), 
     &    (jftabn, jftab), (ipcdtn, ipcdt)
      character*8 iftabc,jftabc,ipcdtc,idgnc
