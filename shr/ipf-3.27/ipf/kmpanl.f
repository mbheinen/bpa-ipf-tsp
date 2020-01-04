C    @(#)kmpanl.f	20.3 2/13/96
      function kmpanl   (m,n)
 
      include 'ipfinc/parametr.inc'
      include 'ipfinc/anlys.inc'
C
      character ljstfy *3

      kmpanl = 0
      return

      entry kompan (m,n)
      j = lstown(m)
      k = lstown(n)
      kompan=kompr (ljstfy (lowner(j)),ljstfy (lowner(k)),junk)
      return

      entry kmpan2  (m,n)
      j = lstvlt(m)
      k = lstvlt(n)
      kmpan2  = (avolt(j) - avolt(k))*100.0
      return
      end
