static char sccsid[]="@(#)fmtpg.c	20.9 2/15/95";
/*
*           IPF V2.x: Interactive PowerFlow Version 2.x
*              Bonneville Power Administration (BPA)
*         Authors:  D. L. Clark, J. G. Coleman, W. L. Powell, 
*                B. E. Rogers, K. E. Rowell, J. L. Rutis, D. M. Stefonik, 
*                D. M. Syzmanski 
*
*                              NOTICE
*
* Interactive Powerflow (IPF) was developed by BPA and its contractors
* with about 20% of the cost supported by the Electric Power Research
* Institute (EPRI).  By mutual agreement, as described in EPRI 
* Agreement RP2746-03 entitled Graphical User Interface for 
* Powerflow, March, 1992, all results of this project--including 
* the computer program and its documentation--are to be in the 
* public domain.  In a separate Memorandum of Understanding with the 
* Western Systems Coordinating Council (WSCC), BPA agreed in March, 1992, 
* to keep WSCC informed of progress, to make its best effort to develop 
* the program according to the Guidelines adopted by the WSCC Computer 
* Program Management Subcommittee, and to make the final results 
* available for possible further development by WSCC. 
*
* This notice must appear in all copies of this software and documentation.
*/
/****************************************************************************
*            fmtpg.c
*
* Mon Oct  5 14:38:16 PDT 1992  [dbs] bug noticed in hyper-links, the first may
				      be the background for all other pages???
* Fri Sep 11 17:27:32 PDT 1992  [dbs] added text to selection dialogs
*
*****************************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "dmgr.h"
#include "fmtpg.h"
#include "fm.h"
#include "em_p.h"
#include "em_p1.h"
#include "em.h"

em_init

static int compar(IPFCONST void *v1, IPFCONST void *v2);
/* static int compar		(void *v1, void *v2); */
int load_mif			(char *file,int cpage);

extern long inchtopixy		(double d);
extern long inchtopixx		(double d);
extern void showtext		( char * );
extern void new_text		(char *name, char *sel_box_name, char *txt);
extern void Vuit_MU		( char *, char * );

FILE *crnt_fp = NULL;
FILE *ucm_fp = NULL;  /* user comments for help system */
int eof=0;
char pgfnumstr[255]="\0";
int catalog=0;
int aframes=0;
int frame=0;
int textrect=0;
int flow=0;
int atbl=0;
int mkr=0;
int marker=0;
int para=0;
int units=UIN;
int paraline = 0;
int crntbkt = 0;
int crntpgf = 0;
int crntfnt = 0;
int crnttab = 0;
int crntcond= 0;
int crntpg = 0;
int maxpg = 0;
int maxpgf = 1;
int maxfnt = 1;
int maxcond= 0;
int maxtr= 0;
int crnttr= 0;
int crntmkr= 0;
double  mifversion=0;

Condition c[CONDMAX];
MifFont f[FONTMAX];
TRECT tr[TRECMAX];
Page pg[PAGEMAX];
Pgf p[PGFMAX];
Marker m[MKRMAX];
Ucm ucm_dir[UCM_DIR_MAX];

static int compar(IPFCONST void *v1, IPFCONST void *v2) 
/* static int compar(void *v1, void *v2) */
{
  Elem *e1=(Elem *)v1, *e2=(Elem *)v2;
  /* printf("e1->key=%s, e2->key=%s\n",e1->key,e2->key); */
  return((long)strncmp(e1->key, e2->key, sizeof(e1->key)));
}

nxtpg()
{
}

int qstring;
char *lastword;

char *getword()
{
  static int qstr=0;
  static char p[500];
  char *s = p;
  register int c;
  qstring=0;

  while(1)
  {
    if((c = getc(crnt_fp)) == EOF)
    {
      printf("end of input\n");
      printf("crntbkt=%d, crntpgf=%d, crntfnt=%d\n",crntbkt,crntpgf,crntfnt);
      printf("maxpgf=%d, maxfnt=%d, maxtr=%d\n",maxpgf,maxfnt,maxtr);
      return(NULL);
    }

    if(qstr == 0)       /* not part of quoted string */
    {
      if (c == '`')
      {
        qstr=1;
      }
      else if(isspace(c))
      {
	if(s != p)
          break;
      }
      else if(c == '<')
      {
	if (s != p)
        {
	  ungetc(c,crnt_fp);
	  break;
	}

        *s++ = c;
	break;
      }
      else if(c == '>')
      {
	if (s != p)
        {
	  ungetc(c,crnt_fp);
	  break;
	}

        *s++ = c;
	break;
      }
      else if(c == '#')
      {
	while((c = getc(crnt_fp)) != '\n');
	if(s != p)
          break;
      }
      else
        *s++ = c;
    }
    else if(c == '\'')       /* end of quoted string */
    {
      qstr=0;
      qstring=1;
      break;
    }
    else if(c =='\\')        /* substitute escaped characters */
    {
      int chr;

      switch(c = getc(crnt_fp))
      {
	case  't': {int x=0;for(;x<4;++x) *s++ = ' '; }break;
	case  'q': *s++ = '\''; break;
	case  'Q': *s++ = '`'; break;
	case '\\': *s++ = '\\'; break;
	case  'x': fscanf(crnt_fp,"%2x ",&chr);
                   switch(chr)
                   {
		     case 0xd4: *s++ = '`'; break;
		     case 0xd5: *s++ = '\''; break;
		     case 0xd3:
		     case 0xd2: *s++ = '\"'; break;
		     case 0xa5: *s++ = '*'; break;
		   }
		   break;
      }
    }
    else
      *s++ = c;             /* keep as part of quoted string */
  }
  *s = '\0';
  lastword = p;
  /* printf("{%s}",p); */
  return(p);
}

double getdim()
{
  char c;
  static double d;

  fscanf(crnt_fp," %lf ",&d);

  if((c=getc(crnt_fp)) == '\"')
    return(d);
  else if (c == '>')
  {
    ungetc(c,crnt_fp);
    return(d);
  }
  else if(c == 'p')
  {
    int c1;
    if((c1 = getc(crnt_fp)) == 't')
      return(d/72);
    else
      ungetc(c1,crnt_fp);
  }
  else if(c == 'c')
  {
    int c1;

    if((c1 = getc(crnt_fp)) == 'm')
      return(d*2.54);
    else
      ungetc(c1,crnt_fp);
  }
  else
  {
    ungetc(c,crnt_fp);
    printf("unknown units for dimension %lf *%c*\n",d,c);
  }
}

int getint()
{
  char *word = getword();
  return(atol(word));
}

int getdat()
{
  Elem *el;
  char *word = getword();

  /*
  * NOTE: make sure IPFCONST properly defined for machine prototypes 
  * -DIPFCONST=const for compile flag or fo VMS
  * which sets #define IPFCONST const in dmgr.h
  */
  el = (Elem *)bsearch(word,e,sizeof(e)/sizeof(e[0]),sizeof(e[0]),compar);

  if(el != NULL)
    return(el->data);
  else
  {
    printf("[%s not found]",word);
    return(0);
  }
}

Elem *gettok()
{
  char *word = getword();

  if (word == NULL)
  {
    eof=1;
    return(NULL);
  }
  return((Elem *)bsearch(word,e,sizeof(e)/sizeof(e[0]),sizeof(e[0]),compar));
}

extern long prevx, prevy;
long tflow=0;

find_link(char *link)
{
  char *filename,file[200],linkname[200];
  char text[1000];
  FILE *fp=crnt_fp;
  int c=strlen(&link[9]);

  while(--c)
  {
    if(link[9+c] == ':')
      break;
  }

  if (c > 0)
  { /* found filename */
    strncpy(file,&link[9],c);
    file[c]='\0';
    filename=file;
    strcpy(linkname,&link[9+c+1]);
    printf("opening file %s\n",filename);

    if((fp=gopen(filename,"r")) == NULL) {
      perror(filename);
      return(0);
    } else fseek(fp,0,0);
  }
  else
  {
    if(NULL == fp) { /* patch code for instance where no help file found */
      printf("Warning - no help file found\n");
      return;
    }
    filename=NULL;
    strcpy(linkname,&link[9]);
    fseek(fp,tflow,0);
  }

  while(fgets(text,1000,fp) != NULL)
  {
    char *tline;
    int mpg;

    if((tline=strstr(text,"<MText `newlink ")) == NULL)
      continue;

    tline += 16;

    if(strncmp(tline,linkname,strlen(tline)-3) == 0)
    {
#ifdef NOGOOD
      new_text("help_dialog_trace_text","help_trace_selection_dialog",linkname);
#endif
      fgets(text,1000,fp);

      if((tline=strstr(text,"<MCurrPage ")) == NULL)
        printf("bad marker\n");

/*      printf("text=%s,tline=%s*\n",text,tline); */
      mpg=atol(tline + 11);
      load_mif(filename,mpg+5);
      return(mpg+5);
    }
  }
  return(0);
}


int load_mif(char *file,int cpage)
{
  char ufilename[200];
  if((maxpg > 0) && (cpage > maxpg))
  {
    printf("end of document\n");
    cpage = maxpg;
  }

  marker=0;
  crnttr= 0;
  catalog=0;
  flow=0;
  para=0;
  paraline=0;
  crntbkt = 0;
  crntpgf = 0;
  crntfnt = 0;
  crnttab = 0;
  crntmkr = 0;
  crntcond= 0;
  crntpg = 0;
  eof = 0;

  if(file == NULL)  /* reposition to start of text flow */
  {
    fseek(crnt_fp,tflow,0);
  }
  else
  {
    /* open file and read from start */
    units=UIN;
    tflow = 0;
    maxpg = 0;

    for(; maxpgf >= 0; --maxpgf) 
      if(p[maxpgf].tabs)
        free(p[maxpgf].tabs);

    maxpgf = 1;
    maxfnt = 1;
    maxcond= 0;
    maxtr= 0;
    mifversion=0;
    if(crnt_fp != NULL) {
      fclose(crnt_fp);
    }
    crnt_fp = gopen(file,"r");
    if (crnt_fp == NULL)
    {
      char str1[200];
      sprintf(str1,"cannot find file %s\n",file);
      err.line=EM_LINE;
      err.msg=str1;
      err.type=INFO;
      err.link="help_files";
      err.ident=sccsid;
      em_show(&err);
      return(1);
    }
    if(ucm_fp != NULL ) {
      fclose(ucm_fp);
    }
    strcpy(ufilename,file);
    strcpy(&ufilename[strlen(ufilename)-3],"ucm");
    printf("userfilename=%s\n",ufilename);
    if ((ucm_fp = gopen(ufilename,"r+")) != NULL) {
      fread(ucm_dir,sizeof(ucm_dir),1,ucm_fp);
    } else {
      ucm_fp = gopen(ufilename,"w+");
      memset(ucm_dir,0,sizeof(ucm_dir));
    }

    /* sort keyword table so we can do binary search for key */
    qsort((void *)e, sizeof(e) / sizeof(e[0]), sizeof(e[0]), compar);
  }

/* removed 15 Dec - no text box in uil JLR
  if(file != NULL)
    new_text("help_dialog_file_text",NULL,file);
*/

  while(1)
  {
    char *nstr;
    Elem *tempElem = gettok();
    Elem *el;

    if(tempElem != NULL)
    {
      /* printf("token=%2d, %s\n",tempElem->data,tempElem->key); */

      switch(tempElem->data)
      {
	case MIFFILE:
          mifversion = getdim();
          continue;

	case CONDITIONCATALOG:
          catalog=crntbkt;
          continue;

	case CONDITION:
          if(++maxcond >= CONDMAX)
          {
	    printf("out of condition space\n"); 
	  }
          else
            memset(&c[maxcond],'\0',sizeof(c[maxcond]));

          continue;

	case CTAG:
          if(catalog)
          {
            nstr=getword();
            strcpy(c[maxcond].CTag,nstr);
          }
          else
          {
            /* find cond and set current condition */
          }

          continue;

	case CSTATE:  c[maxcond].CState=getdat();
		      continue;

	case CSTYLE:
          c[maxcond].CStyle=getdat();
          continue;

	case CSEPARATION:
          c[maxcond].CSeparation=getint();
          continue;

	case LT:
          ++crntbkt;
          /* printf("crntbkt=%d\n",crntbkt); */
          continue;

	case GT:
          --crntbkt;

          if(catalog > crntbkt)
          {
            catalog = 0;
          }
          else if(flow > crntbkt)
          {
            flow = 0;
          }
          else if(frame > crntbkt)
          {
            frame = 0;
          }
          else if(textrect > crntbkt)
          {
            textrect = 0;
          }
          else if(paraline > crntbkt)
          {
            paraline = 0;
            prevx=0;
            prevy += inchtopixy(p[0].PgfLeading);
          }
          else if(para > crntbkt)
          {
            para = 0;
            prevy += inchtopixy(p[0].PgfSpAfter);
          }

          continue;

	case PGFCATALOG:
          catalog=crntbkt;
          continue;

	case FONTCATALOG:
          catalog=crntbkt;
          continue;

	case PAGE:
          if(++maxpg >= PAGEMAX)
          {
            printf("out of page space\n"); 
          }
          else
            memset(&pg[maxpg],'\0',sizeof(pg[maxpg]));

          continue;

	case PAGETYPE:
          pg[maxpg].PageType=getdat();
          continue;

	case PAGENUM:
          strcpy(pg[maxpg].PageNum,getword());
          continue;

	case PAGETAG:
          strcpy(pg[maxpg].PageTag,getword());
          continue;

	case PGESIZE:
          pg[maxpg].PageSize.i1=getdim();
	  pg[maxpg].PageSize.i2=getdim();
          continue;

	case PGEORIENTATION:
          pg[maxpg].PageOrientation=getdat();
          continue;

	case PAGEBACKGROUND:
          {
            int x;
            nstr=getword();
              /* look for background one of 5 master pages*/
            for(x=0; x<= 5; ++x)
            {
              if(strncmp(pg[x].PageTag, nstr, sizeof(pg[x].PageTag)) == 0)
              {
                pg[maxpg].PageBackground=x;
                break;
              }
            }
            pg[maxpg].PageBackground=1;
          }

          continue;

	case NOTES:
          continue;

	case TEXTRECT:
          textrect=crntbkt;
          continue;

	case AFRAMES:
          aframes=ftell(crnt_fp);
          continue;

	case FRAME:
          frame=crntbkt;
          continue;

	case TEXTRECTID:
          crnttr=getint();
          prevy=0;
          continue;

	case TEXTFLOW:
          flow=crntbkt;

          if(tflow == 0)
            tflow=ftell(crnt_fp);

          continue;

	case TFTAG:
          getword();
          continue;

	case TFAUTOCONNECT:
          getword();
          continue;

	case PARA:
          para=crntbkt;
          continue;

	case PARALINE:
          paraline=crntbkt;
          prevy += inchtopixy(f[p[0].PgfFont].FSize);
          continue;

	case PGFNUMSTRING:
          nstr = getword();

          if((tr[crnttr].pgid == cpage) ||
             (tr[crnttr].pgid == pg[cpage].PageBackground))
          {
            strcpy(pgfnumstr,nstr);
            /* showtext(pgfnumstr); */
          }

          continue;

	case EMSTRING:
          nstr = getword();

          if((tr[crnttr].pgid == cpage) ||
             (tr[crnttr].pgid == pg[cpage].PageBackground))
          {
             if(pgfnumstr[0] != '\0')
             {
               showtext(pgfnumstr);
               pgfnumstr[0] = '\0';
             }
             showtext(nstr);
          }
          else if(tr[crnttr].pgid > cpage) {
	    if (ucm_dir[cpage].start > 0)
		Vuit_MU( "M", "paper_clip");
	    else
		Vuit_MU( "U", "paper_clip");
            return(0);
          }
          continue;

	case ATBL:
          atbl=getint();
          continue;

	case ID:
          if(textrect)
          {
            maxtr = getint();

            if(maxtr > TRECMAX)
            {
              printf("out of TRect space\n");
            }

            tr[maxtr].pgid = maxpg;
            tr[maxtr].ID = maxtr;
            continue;
          }
          else if(frame)
          {
            continue;
          }
          else
            printf("unknown ID???\n");

	case PEN:
          tr[maxtr].Pen = getint();
          continue;

	case FILL:
          tr[maxtr].Fill = getint();
          continue;

	case PENWIDTH:
          tr[maxtr].PenWidth = getdim();
          continue;

	case SEPARATION:
          tr[maxtr].Separation = getint();
          continue;

	case ANGLE:
          tr[maxtr].Angle = getint();
          continue;

	case BRECT:
          tr[maxtr].BRect.i1=getdim();
          tr[maxtr].BRect.i2=getdim();
          tr[maxtr].BRect.i3=getdim();
          tr[maxtr].BRect.i4=getdim();
          continue;

	case TRNEXT:
          tr[maxtr].TRNext = getint();
          continue;

	case PGF:
          if(catalog)
          {
	    if(++maxpgf >= PGFMAX)
            {
	      printf("out of paragraph space\n"); 
	    }
            else
              memset(&p[maxpgf],'\0',sizeof(p[maxpgf]));

	    crntpgf=maxpgf;
          }
          else
            crntpgf=0;

	  crnttab = -1;
	  continue;

	case PGFTAG:
          if(catalog)
          {
            nstr=getword();
            strcpy(p[crntpgf].tag,nstr);
          }
          else
          {
            /* find pgf and set current pgf */
            int x;
            nstr=getword();
            crntpgf = 0;

            for(x=0; x<= maxpgf; ++x)
            {
              if(strncmp(p[x].tag, nstr, sizeof(p[x].tag)) == 0)
              {
                memcpy(&p[0],&p[x],sizeof(p[0]));
                continue;
              }
            }

            prevy += inchtopixy(p[0].PgfSpBefore);
          }

          continue;

	case PGFUSENEXTTAG:
          p[crntpgf].PgfUseNextTag=getdat();
          continue;

	case PGFNEXTTAG:
          nstr=getword();
          strcpy(p[crntpgf].PgfNextTag,nstr);
          continue;

	case PGFALIGNMENT:
          p[crntpgf].PgfAlignment = getdat();
          continue;

	case PGFFINDENT:
          p[crntpgf].PgfFIndent=getdim();
          continue;

	case PGFLINDENT:
          p[crntpgf].PgfLIndent=getdim();
          continue;

	case PGFRINDENT:
          p[crntpgf].PgfRIndent=getdim();  
          continue;

	case PGFTOPSEPARATOR:
          nstr=getword();
          strcpy(p[crntpgf].PgfTopSeparator,nstr);
          continue;

	case PGFBOTSEPARATOR:
          nstr=getword();
          strcpy(p[crntpgf].PgfBotSeparator,nstr);
          continue;

	case PGFPLACEMENT:
          p[crntpgf].PgfPlacement=getdat();
          continue;

	case PGFSPBEFORE:
          p[crntpgf].PgfSpBefore=getdim();
          continue;

	case PGFSPAFTER:
          p[crntpgf].PgfSpAfter=getdim();
          continue;

	case PGFWITHPREV:
          p[crntpgf].PgfWithPrev=getdat();
          continue;

	case PGFWITHNEXT:
          p[crntpgf].PgfWithNext=getdat();
          continue;

	case PGFBLOCKSIZE:
          p[crntpgf].PgfBlockSize=getint();
          continue;

	case PGFFONT:
          if(catalog)
          {
            if(++maxfnt > FONTMAX)
            {
              printf("out of font space\n");
            }
            else
              memset(&f[maxfnt],'\0',sizeof(f[maxfnt]));

            crntfnt = maxfnt;
          }
          else
          {
            crntfnt=0;
            f[0].fontid = 0;
          }   

          p[crntpgf].PgfFont = crntfnt;
          strcpy(f[crntfnt].tag,"PgfFont");
          continue;

	case PGFLINESPACING:
          p[crntpgf].PgfLineSpacing = getdat();
          continue;

	case PGFLEADING:
          p[crntpgf].PgfLeading = getdim();
          continue;

	case PGFAUTONUM:
          p[crntpgf].PgfAutoNum = getdat();
          continue;

	case PGFNUMFORMAT:
          strcpy(p[crntpgf].PgfNumFormat,getword());
          continue;

	case PGFNUMBERFONT:
          strcpy(p[crntpgf].PgfNumberFont,getword());
          continue;

	case PGFNUMATEND:
          p[crntpgf].PgfNumAtEnd = getdat();
          continue;

	case PGFNUMTABS:
          {
            int cnt = getint();
            p[crntpgf].PgfNumTabs = getint();
            crnttab = -1;

            if(cnt > 0)
              p[crntpgf].tabs = calloc(cnt,sizeof(TSTOP));

            continue;
          }

	case TABSTOP:
          ++crnttab;
          continue;

	case TSX:
          p[crntpgf].tabs[crnttab].TSX  =getint();
          continue;

	case TSTYPE:
          p[crntpgf].tabs[crnttab].TSType  =getdat();
          if (p[crntpgf].tabs[crnttab].TSType == LEFT) {
             prevx = inchtopixx(tr[crnttr].BRect.i1 - p[0].PgfFIndent -
                p[crntpgf].tabs[crnttab].TSX);
          }
          continue;

	case TSLEADERSTR:
          strcpy(p[crntpgf].tabs[crnttab].TSLeaderStr,
          getword());
          continue;

	case TSDECIMALCHAR:
          p[crntpgf].tabs[crnttab].TSDecimalChar=getint();
          continue;

	case PGFHYPHENATE:
          p[crntpgf].PgfHyphenate  =getdat();
          continue;

	case HYPHENMAXLINES:
          p[crntpgf].HyphenMaxLines=getint();
          continue;

	case HYPHENMINPREFIX:
          p[crntpgf].HyphenMinPrefix=getint();
          continue;

	case HYPHENMINSUFFIX:
          p[crntpgf].HyphenMinSuffix=getint();
          continue;

	case HYPHENMINWORD:
          p[crntpgf].HyphenMinWord  =getint();
          continue;

	case PGFLETTERSPACE:
          p[crntpgf].PgfLetterSpace=getdat();
          continue;

	case PGFMINWORDSPACE:
          p[crntpgf].PgfMinWordSpace=getint();
          continue;

	case PGFOPTWORDSPACE:
          p[crntpgf].PgfOptWordSpace=getint();
          continue;

	case PGFMAXWORDSPACE:
          p[crntpgf].PgfMaxWordSpace=getint();
          continue;

	case PGFLANGUAGE:    p[crntpgf].PgfLanguage=getdat();
          continue;

	case PGFCELLALIGNMENT:
          p[crntpgf].PgfCellAlignment=getdat();
          continue;

	case PGFCELLMARGINS:
          p[crntpgf].PgfCellMargins.i1=getdim();
	                     p[crntpgf].PgfCellMargins.i2=getdim();
	                     p[crntpgf].PgfCellMargins.i3=getdim();
	                     p[crntpgf].PgfCellMargins.i4=getdim();
          continue;

	case PGFCELLLMARGINFIXED:
          p[crntpgf].PgfCellLMarginFixed=getdat();
          continue;

	case PGFCELLTMARGINFIXED:
          p[crntpgf].PgfCellTMarginFixed=getdat();
          continue;

	case PGFCELLRMARGINFIXED:
          p[crntpgf].PgfCellRMarginFixed=getdat();
          continue;

	case PGFCELLBMARGINFIXED:
          p[crntpgf].PgfCellBMarginFixed=getdat();
          continue;

	case FTAG:
          if(catalog)
          {
            strcpy(f[crntfnt].tag,getword());
          }
          else
          {
            int x;
            nstr=getword();
            crntfnt = 0;
	    /* find font by tag and set curent font 
            for(x=0; x<= maxfnt; ++x)
            {
	      if(strncmp(f[x].tag, nstr, sizeof(f[x].tag)) == 0)
              {
	        ld_fnt(x);
	        printf("font catalog %d,tag=%s,nstr=%s\n",x, f[x].tag,nstr);
	        memcpy(&f[0],&f[x],sizeof(f[0]));
	        break;
	      }
            } */
          }

          continue;

	case FONT:
          if(catalog)
          {
            if(++maxfnt > FONTMAX)
            {
              printf("out of font space\n");
            }

            crntfnt = maxfnt;
          }
          else
          {
            crntfnt = 0;
            f[0].fontid = 0;
          }

          p[crntpgf].PgfFont = crntfnt;
          continue;

	case FSEPARATION:
          f[crntfnt].FSeparation = getint();
          continue;

	case FFAMILY:
          nstr=getword();
          strcpy(f[crntfnt].FFamily,nstr);
          continue;

	case FVAR:    nstr=getword();
          strcpy(f[crntfnt].FVar,nstr);
          continue;

	case FWEIGHT:
          nstr=getword();
          strcpy(f[crntfnt].FWeight,nstr);
          continue;

	case FSHADOW:
          f[crntfnt].FShadow   = getdat();
          continue;

	case FOUTLINE:  f[crntfnt].FOutline  = getdat();
          continue;

	case FPAIRKERN:
          f[crntfnt].FPairKern = getdat();
          continue;

	case FOVERLINE:
          f[crntfnt].FOverline = getdat();
          continue;

	case FCHANGEBAR:
          f[crntfnt].FChangeBar = getdat();
          continue;

	case FSUBSCRIPT:
          f[crntfnt].FSubScript = getdat();
          continue;

	case FSUPSCRIPT:
          f[crntfnt].FSupScript = getdat();
          continue;

	case FUNDERLINE:
          f[crntfnt].FUnderline = getdat();
          continue;

	case FNUMERICUNDERLINE:
          f[crntfnt].FNumericUnderline = getdat();
          continue;

	case FDOUBLEUNDERLINE:
          f[crntfnt].FDoubleUnderline = getdat();
          continue;

	case FSTRIKE:
          f[crntfnt].FStrike = getdat();
          continue;

	case FITALIC:
          f[crntfnt].FItalic = getdat();
          continue;

	case FBOLD:
          f[crntfnt].FBold  = getdat();
          continue;

	case FPLAIN:
          f[crntfnt].FPlain = getdat();
          continue;

	case FDW:
          f[crntfnt].FDW = getdim();
          continue;

	case FDY:
          f[crntfnt].FDY = getdim();
          continue;

	case FDX:
          f[crntfnt].FDX = getdim();
          continue;

	case FSIZE:
          f[crntfnt].FSize = getdim();
          continue;

	case FANGLE:
          nstr=getword();
          strcpy(f[crntfnt].FAngle,nstr);
          continue;

	case MARKER:
          crntmkr++; 
          continue;

	case MTYPE:
          m[crntmkr].MType = mkr = getint();
          continue;

	case MCURRPAGE:
          m[crntmkr].MCurrPage = getint();
          continue;

	case MTEXT:   
          if((tr[crnttr].pgid == cpage) ||
             (tr[crnttr].pgid == pg[cpage].PageBackground))
          {
            strcpy(m[crntmkr].MText,getword());
            m[crntmkr].MRect.i1 = prevx;
            m[crntmkr].MRect.i2 = prevy;

	    switch(m[crntmkr].MType)
            {
              case 2:
                new_text("help_dialog_index_text","help_index_selection_dialog",
                m[crntmkr].MText);
                break;

              case 4:
                new_text("help_dialog_subject_text",
		  "help_trace_selection_dialog", m[crntmkr].MText);
                break;

              case 6:
                new_text("help_dialog_glossary_text",
		  "help_glossary_select_dialog", m[crntmkr].MText);
                break;

              case 8:
                if(strncmp(m[crntmkr].MText,"gotolink",8) == 0)
	          marker=1;

                break;
            }
          }
          else
            strcpy(m[crntmkr].MText,getword());

          continue;

	case UNITS:
          units = getdat();

          if(units != UIN)
          {
            printf("only Units of Uin supported %d\n",units);
          }

          continue;

	case VARIABLE:
          continue;

	case VARIABLENAME:
          nstr = getword();

          if((tr[crnttr].pgid == cpage) ||
              (tr[crnttr].pgid == pg[cpage].PageBackground))
          {
            if(strcmp(nstr,"Current Page #") == 0)
            {
              showtext(pg[cpage].PageNum);
            }
            else
	    {
#ifdef DEBUG
              printf("can't find %s\n",nstr);
#endif
	    }
          }

          continue;

	case EMCHAR:
          if((tr[crnttr].pgid == cpage) ||
             (tr[crnttr].pgid == pg[cpage].PageBackground))
          {
            switch(getdat())
            {
              case SOFTHYPHEN:
                showtext("-");
                break;

              case HARDRETURN:
                showtext("");
                break;

              case EMDASH:
                showtext("--");
                break;

              case TAB:
                showtext("        ");
		/* !!!! this need to use tabstops instead */
                break;

              default:
		{
#ifdef DEBUG
                printf("unsupported Char\n");
#endif
		}
            }
          }
          else
            getword();

          continue;

	default:
          printf("UNKNOWN TOKEN = %d\n",tempElem->data);
      }
    }
    else
    {
      int lbrkcnt=1;
      char *word;

      if(eof == 1)
        return(0);

#ifdef DEBUG
      printf("%s NOT FOUND skipping\n",lastword);
#endif 
      do
      {
        word = getword();

	if(qstring)
          continue;

/* JLR - Dies here when getword returns (nill) 
         If I do a return here on test of (nill),
         help_resize dies because x page dim is 0.0 */
	switch(word[0])
        {
	  case '>':
            --lbrkcnt;
            break;

	  case '<':
            ++lbrkcnt;
            break;
	}

	/* printf("nstr=%s, cnt =%d\n",nstr,lbrkcnt); */

	if(lbrkcnt <= 0)
          break;
      }
      while(1);

      --crntbkt;
    }
  }
}

prvpg()
{
}

int help_file_name_set(widget, tag, callback_data)
  Widget          widget;
  caddr_t         *tag;
  XmFileSelectionBoxCallbackStruct         *callback_data;
{
  XmString		compound_string;
  char			*text;
  XmStringContext	context;
  XmStringCharSet	charset;
  XmStringDirection	dir;
  Boolean		separator;
  char			file_name[81];
 
  compound_string = (XmString)callback_data->value;
  XmStringInitContext( &context, compound_string );
  XmStringGetNextSegment ( context, &text, &charset, &dir, &separator );
  sprintf( file_name, "%s", text );
  load_mif(file_name,1);

  return(0);
}
