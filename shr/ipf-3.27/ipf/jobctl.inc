c
        common /jobctl/ endjob, batch, nocase, nobus
        common /jobcar/ prgvsn, rdate, inrcd, jobreq(40)

        logical endjob,batch
        character prgvsn*8,rdate*9,jobreq*10,inrcd*120
        integer nocase, nobus
c
