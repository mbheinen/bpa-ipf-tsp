C    %W% %G%
      subroutine initl4
c      
c     This subroutine initlializes the dc tables, the default
c     distance relay, and the resistor brake.  It is called
c     by SWINGM.
c      
      include 'tspinc/params.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/cntrl2.inc'
      include 'tspinc/param.inc'
      include 'tspinc/contrl.inc'
      include 'tspinc/ecsind.inc'
      include 'tspinc/comn34.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/brake1.inc'
      include 'tspinc/relays.inc'
      include 'tspinc/vym.inc'
      include 'tspinc/znox.inc'
      include 'tspinc/igentn.inc'
      include 'tspinc/nwgntn.inc'
      include 'tspinc/namec.inc'
      include 'tspinc/bname.inc'
      include 'tspinc/buskv.inc'
      include 'tspinc/gentbla.inc'
      include 'tspinc/gentblb.inc'
      include 'tspinc/gentblc.inc'
      include 'tspinc/busvolt.inc'
      equivalence (kptbl1,igecse)
      equivalence (aeps,ar),(aes,brs,af1),(bes,bf1),(ba1s,br),
     1            (baps,a1),(bfs,a2)
      equivalence (creg(27),vrmlt)
c     -  Functions
      logical dbghere                                                   !dem
c     -  Local variables 
      dimension zoecs(512)
      character*8 ibus1
      character*1 igenid
      logical debug                                                     !dem

c     -      begin     begin     begin     begin     begin     begin

      debug = dbghere ('INITL4  ')                                      !dem
      call mpost('INITL4')
      link=1
      igtmax=0
      mxng = MAXGEN
c      
c       INITIALIZE ECS POINTERS FOR ARRAYS USED IN THE SOLUTION LOGIC
c      
      keyri=kdcmlt+1
      keyii=keyri+1
      kvmd=keyii+1
      kvmq=kvmd+1
      kvmagt=kvmq+1
      kepq=kvmagt+1
      kepd=kepq+1
      kvfl=kepd+1
      kgovp=kvfl+1
      kgenp=kgovp+1
      kangl=kgenp+1
      kang1=kangl+1
      kangp=kang1+1
      kangp2=kangp+1
      kpacc=kangp2+1
      kpccex=kpacc+1
      kpccm1=kpccex+1
      kcur=kpccm1+1
      kpccm2=kcur+1
      igecs=kpccm2 + 1
C     *
C     * ZERO THE E.C.S. FIELD LENGTH
      do 2800 i=1,512
 2800 zoecs(i)=0.0
      lastec=20000
      iecs=keyri
 2820 if ((iecs+512).gt.lastec) go to 2840
      call ritecs (zoecs,iecs,512)
      iecs=iecs+512
      go to 2820
 2840 k512=lastec-iecs+1
      call ritecs (zoecs,iecs,k512)
      lastec = 32000
 2880 iex = 35
c      
c       TRANSFER GENERATOR DATA TABLES FROM SCRATCH DISK TO SIMULATED EC
c      
      do 2980 ig=1,isgg
      isup = 33
      lgnth=igentn(2,ig)
      igentn(2,ig)=igecs
      read (l2,rec=ig) (datat(n),n=1,lgnth)
      mgen=igndta(1,ig)
      if (mgen.gt.igtmax) igtmax=mgen
      if (mgen.eq.1) igr=7
      if (mgen .eq. 2) igr = 26
      if (mgen.gt.2) igr=27
      if (mgen .gt. 5) igr = 39
      if (mgen .eq. 9) igr = 4
      igr2=igr
      mgov=igndta(3,ig)
      if (mgov.eq.0) go to 2940
      mgvecs=igecs+igr
      igndta(4,ig)=mgvecs
      igv = 19
      if (mgov.gt.5) igv = 32
      call ritecs (datat(igr+1),mgvecs,igv)
      igr=igr+igv
 2940 if (mgen .eq.9) go to 2960
      if ((mgen .ne.5) .and. (mgen .ne. 8)) go to 2960
      mex = igndta(5,ig)
      iex = 35
      if (mex .gt. 10) iex = 5
      mexecs=igecs+igr
      igndta(6,ig)=mexecs
      call ritecs (datat(igr+1),mexecs,iex)
      igr=igr+iex
      msupp=igndta(7,ig)
 2950 msupp = igndta(7, ig)
      if (msupp .eq. 0) go to 2960
      msupec=igecs+igr
      igndta(8,ig)=msupec
      call ritecs (datat(igr+1),msupec,isup)
      igr=igr+isup
c      
c        CHECK FOR PSS NOTCH FILTER
c      
      if (datat(igr) .gt. 0.) then
         call ritecs(datat(igr+1),msupec+isup,9)
         igr = igr + 9
      endif
 2960 call ritecs (datat,igecs,igr2)
      igecs=igecs+igr
 2980 continue
c      
c       FORM BUS NAME AND BUS VOLTAGE TABLES FOR SOLUTION
c      
      do 2990 k= 1,nmx
      bname(k) = exnamc(k)
      kbase = ixnamn(k)
 2990 buskv(k) = basekv(kbase)
      do 3000 k=1,isgg
      ibusno=igentn(1,k)
      igenid=igentc(k)
      ibus1=exnamc(ibusno)
      kbase=ixnamn(ibusno)
      nwgntn(k)=kbase
      nwgntc(1,k)=ibus1
      nwgntc(2,k)=igenid
 3000 continue
      ifirst = 3
      jfirst = 3
      irectp = 61                                                       !dem
      idesc = 61                                                        !dem
      irecln = 3                                                        !dem
      call puthisi (irectp,1)                                           !dem
      call puthisi (idesc,1)                                            !dem
      call puthisi (irecln,1)                                           !dem
      if (debug) then                                                   !dem
        call dbgeko2 ('INITL4 - writing counts of gens, 2- & m-term ',  !dem
     +    'DC buses')                                                   !dem
        call dbgwri ('  IRECTP /record type/ = ',irectp)                !dem
        call dbgwri ('  IDESC /rec descrip/  = ',idesc)                 !dem
        call dbgwri ('  IRECLN /rec length/  = ',irecln)                !dem
      endif                                                             !dem
      call puthisi (isg,1)                                              !dem
      call puthisi (ldc,1)                                              !dem
      call puthisi (ldc1,1)                                             !dem
C     WRITE (L8) IFIRST,JFIRST,ISG,LDC,LDC1
      irectp = 67                                                       !dem
      idesc = 256 * isg + 16                                            !dem
      irecln = 4 * isg + 1                                              !dem
      if (debug) then                                                   !dem
        call dbgeko2 ('INITL4 - writing gen bus names, & IDs ',         !dem
     +    'to history file.')                                           !dem
        call dbgwri ('  IRECTP /record type/ = ',irectp)                !dem
        call dbgwri ('  IDESC /rec descrip/  = ',idesc)                 !dem
        call dbgwri ('  IRECLN /rec length/  = ',irecln)                !dem
      endif                                                             !dem
      call puthisrc (irectp,idesc,irecln,nwgntc)                        !dem
      irectp = 69                                                       !dem
      idesc = 69                                                        !dem
      irecln = isg                                                      !dem
      if (debug) then                                                   !dem
        call dbgeko2 ('INITL4 - writing gen bus KV codes ',             !dem
     +    'to history file.')                                           !dem
        call dbgwri ('  IRECTP /record type/ = ',irectp)                !dem
        call dbgwri ('  IDESC /rec descrip/  = ',idesc)                 !dem
        call dbgwri ('  IRECLN /rec length/  = ',irecln)                !dem
      endif                                                             !dem
      call puthisri (irectp,idesc,irecln,nwgntn)                        !dem
C     WRITE(L8)((NWGNTC(I,K),I=1,2),K=1,ISG),(NWGNTN(K),K=1,ISG)
      igecse=igecs
 3019 if (keybrd(8) .ne. 0)then
         write (outbuf,3020)
         call prtout (1)
         write (outbuf,3021)
         call prtout (1)
         write (outbuf,3022) keyri,kpccm2,igecse
         call prtout (1)
 3020    format (1h0,5x,'ECS ADDRESSES')
 3021    format (6x,'KEYRI',4x,'KPCCM2',5x,'IGECSE')
 3022    format (1x,3i10)
      endif
c      
c       CALL LSCHK TO CHECK LS CARDS FOR ERRORS IN LOAD MODIFICATION,
c       GENERATOR DROPPING, OR TRANSIENT STABILIZER TRIGGER
c      
      call lschk
      nmxm4 = (nmx+4)/4
      if (ldc .eq. 0 .and. ldc1 .eq. 0) go to 11090
      if (jdc .eq. 0) go to 11090
c      
c       CALL DCINT TO INITIALIZE TWO TERMINAL DC TABLES
c      
      if (ldc .ne. 0) call dcint
c      
c       CALL MDCINT TO INITIALIZE MUTLITERMINAL DC TABLES
c      
      if (ldc1 .ne. 0) call mdcint
c      
c       CALL RBINT TO INITIALIZE RESISTANCE BRAKE DATA TABLES
c      
11090 if (nbrake .ne. 0)call rbint
c      
c        CALL RLINT2 TO FORM AND STORE
c        DEFAULT DISTANCE RELAY TABLES
c      
      if (iswbps .ne. 0) call rlint2
      itripl=idfltd+ndfltd
c      
c       CALL VYMDI TO INITIALIZE VARIABLE ADMITTANCE MODULATION
c      
      if (iznmax .ne. 0.0) call vymdi
      if (iabort.ne.0) then
         write (errbuf(1), 3920)
         call prterr ('E',1)
 3920    format('0LINK 4 COMPLETED WITH DATA ERRORS, PROGRAM STOPPED.')
         call erexit
      endif
      write (outbuf,3860)
      call prtout (1)
      write (outbuf,3861)
      call prtout (1)
 3860 format('0SUBROUTINE INITL4 HAS BEEN PROCESSED.')
 3861 format('0NO DATA ERRORS HAVE BEEN FOUND IN OVERLAYS 3 OR 4.')
      return
      end
