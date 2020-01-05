      subroutine gtswdta
      
      include 'ipfinc/parametr.inc'

      parameter (MAXKV = MAXBUS*2/100) 
      include 'tspinc/pointr.inc'

      parameter (MAXMACH = MAXBUS/2) 
      common/machic/machic(MAXMACH)       
      character*80 machic

      common /swdata/ swdata, junk, dat, remotf, relayf, serief,
     &                capcpf, loadf, reprf, areaf, busbas, shedf,
     &                machnf, jblnk, swcas, swdate, zones, loadn,
     &                nettf 
      character*10  swdata, junk, dat, remotf, relayf, serief, capcpf,
     &              loadf, reprf, areaf, busbas, shedf, machnf, jblnk,
     &              swcas, swdate, zones, loadn, nettf 

      read (4) swdata,swcas,swdate
      read (4) ibxyz, (basekv(i),i=1,ibxyz)
      read (4) relayf,junk,dat,lrd

      do 180 i=1,lrd,100
180   read (4)junk
      read (4) remotf,relayf,dat,lrr

      do 190 i=1,lrr,100
190   read (4)junk
      read (4) serief,capcpf,dat,lsc

      do 200 i=1,lsc,100
200   read (4)junk
      read (4) loadf,reprf,areaf,ldar

      do 210 i=1,ldar,100
210   read (4)junk
      read (4) loadf,reprf,zones,ldz

      do 220 i=1,ldz,100
220   read (4)junk
      read (4) loadf,reprf,busbas,lrep

      do 230 i=1,lrep,100
230   read (4)junk
      read (4) loadf,shedf,dat,ls

      do 240 i=1,ls,100
240   read (4)junk
      read (4) loadn,nettf,dat,ln

      if (ln.gt.0) then
         do 245 i=1,ln,600
245      read (4)junk
      endif

      read (4) machnf,jblnk,dat,mac

      do 250 i=1,mac,100
      last = min(i+99,mac)
250   read (4) (machic(j),j=i,last)	

      end
