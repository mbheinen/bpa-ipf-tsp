
C       HEADER contains the first 'title' line, with caseid, etc.
C       COMENT(2) contains the two user 'title' lines
C       SUBHED is used only in BPF
C
        common /header/ header, coment(2), subhed(5), repnam
        character header*50, coment*133, subhed*133, repnam*60
C
