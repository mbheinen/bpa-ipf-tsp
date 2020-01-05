C    @(#)kmpvlt.f	20.3 2/13/96
        function kmpvlt (m,n)

C       Compare ownership in the over/undervoltage index array.

        include 'ipfinc/parametr.inc'

        include 'ipfinc/apcom.inc'
        include 'ipfinc/intbus.inc'
 
        common /intown/ intown(MAXBUS)
        character intown*3
 
        if (m .eq. n) then
           kmpvlt = 0
        else
           i = isort(m)
           j = isort(n)
           if (kompr (intown(i),intown(j),kmpvlt) .eq. 0) then
              kmpvlt = kompr (intbus(i),intbus(j),junk)
              if (kmpvlt .eq. 0) then
                 kmpvlt = 100.0 * (intbas(i) - intbas(j))
              endif
           endif
        endif
        return
        end
