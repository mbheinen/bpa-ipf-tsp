C    @(#)jobseq.f	20.3 2/13/96
        subroutine jobseq(seqcod)

C               PURPOSE OF THE SUBROUTINE IS TO GENERATE
C               A UNIQUE JOB SEQUENCE CODE.

C               SEQCOD IS A SIX CHARACTER STRING CONSISTING OF A
C               CODE FOR MONTH,DAY,HOUR,MINUTE,SECOND,
C               and a random number based on cpu seconds


        integer  mon, day, year, hour, min, sec, csec


        character code1*60,month*12,seqcod*6

        data code1(1:26) /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/,
     1      code1(27:52) /'abcdefghijklmnopqrstuvwxyz'/,
     2      code1(53:60) /'?%&*+@=#'/,
     3      month/'123456789OND'/
 

        call n_date( mon, day, year )
        call n_time( hour, min, sec )

        hour = hour + 1
        min = min + 1
        sec = sec + 1
        csec = cpu_secs(0.0) * 1117.0
        csec = mod( csec, 60 ) + 1

        seqcod(1:1) = month(mon:mon)
        seqcod(2:2) = code1(day:day)
        seqcod(3:3) = code1(hour:hour)
        seqcod(4:4) = code1(min:min)
        seqcod(5:5) = code1(sec:sec)
        seqcod(6:6) = code1(csec:csec)

        return
        end
