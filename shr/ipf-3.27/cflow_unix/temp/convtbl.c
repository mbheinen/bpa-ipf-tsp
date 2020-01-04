#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "cflowlib.h"
#include "ft.h"

/*  "fabs" is in a special math library on the UnixWare C system,
*    and I couldn't figure out how to link it in bldcflowlu.m.
*    It requires a compiler parameter "-lm" FOLLOWING the name(s)
*    of the object(s) to which it applies. (See makefile in cfprogs.)
*
*    So I just defined it here in convtbl.c
*/
# ifndef fabs
# define fabs(x) ((x)>=0 ? (x) : -(x))
# endif

extern struct fld_table _ft[MAXRECNUM][MAXFIELDNUM];

int cv_error;
static int record;
static int field;
/*
static double _pow[]={1.,10.,100.,1000.,10000.,100000.,1000000.,10000000.};
*/
static double _pow[]={1e0,1e1,1e2,1e3,1e4,1e5,1e6,1e7,1e8,1e9,1e10,1e11,1e12,
    1e13,1e14,1e15};


/* Global variable cv_error holds the most recently encountered error.
   If DEBUG is enabled then the most recent error is printed, as well
   as the most recent record and field numbers.  Offset is the character
   position in the field where the error occured.
*/
static void conv_error(int offset, int error) {
#ifdef DEBUG
  printf("error %d @ record %d, field %d, offset %d\n", error, record, field, offset);
#endif
  cv_error = error;
}

/* Ascii (-0.00e-0 format) to float
   Read an ascii string of size bytes and convert to a float number.
   The string is assumed to be in FORTRAN format:
     spaces after the number are assumed to be 0's
     if a decimal point is not in the string then assume fixed dec places
*/
float a2f(char *str, int size, int dec) {
  register int x;
  int decimal = 0;  /* true if decimal point found in string */
  int exponent = 0; /* true if exponent (E,e) found in string */
  float f;
  char tmpbuf[20];
  for (x=0; x < size; ++x) {
    tmpbuf[x] = str[x];
    if (isdigit(tmpbuf[x])) continue;
    if (toupper(tmpbuf[x]) == 'E') {exponent=1; continue;}
    if ((tmpbuf[x] == '-') && exponent) continue;
    if (tmpbuf[x] == '-') {tmpbuf[x]='0';tmpbuf[0]='-';continue;}
    if (tmpbuf[x] == '.') {decimal=1; continue;}
    if (!isspace(tmpbuf[x])) conv_error(x,CV_INVALID_DIGIT);
    tmpbuf[x] = '0';
  }
  tmpbuf[x] = '\0';
  f = (float)atof(tmpbuf);
  if (!decimal) f /= (float)_pow[dec];
  return(f);
}

/* Ascii to float - the original function
   Read an ascii string of size bytes and convert to a float number.
   The string is assumed to be in FORTRAN format:
     spaces after the number are assumed to be 0's
     if a decimal point is not in the string then assume fixed dec places
*/
float a2f_(char *str, int size, int dec) {
  register int x;
  int decimal = 0;  /* true if decimal point found in string */
  float f;
  char tmpbuf[20];
  for (x=0; x < size; ++x) {
    tmpbuf[x] = str[x];
    if (isdigit(tmpbuf[x])) continue;
    if (tmpbuf[x] == '-') {tmpbuf[x]='0';tmpbuf[0]='-';continue;}
    if (tmpbuf[x] == '.') {decimal=1; continue;}
    if (!isspace(tmpbuf[x])) conv_error(x,CV_INVALID_DIGIT);
    tmpbuf[x] = '0';
  }
  tmpbuf[x] = '\0';
  f = (float)atof(tmpbuf);
  if (!decimal) f /= (float)_pow[dec];
  return(f);
}

/* Ascii to long
   Read an ascii string of size bytes and convert to a long integer.
   The string is assumed to be in FORTRAN format:
     spaces after the number are assumed to be 0's
*/
long a2l(char *str, int size) {
  register int x;
  char tmpbuf[20];
  for (x=0; x < size; ++x) {
    tmpbuf[x] = str[x];
    if (isdigit(tmpbuf[x])) continue;
    if (tmpbuf[x] == '-') {tmpbuf[x]='0';tmpbuf[0]='-';continue;}
    if (!isspace(tmpbuf[x])) conv_error(x,CV_INVALID_DIGIT);
    tmpbuf[x] = '0';
  }
  tmpbuf[x] = '\0';
  return(atol(tmpbuf));
}

/* Long to ascii
   Writes the long value as an ascii string with a maximum of size bytes.
   Returns pointer to the string.  A static buffer is used and will be 
   overwritten. The string is NULL terminated.
*/
static char tmpbuf[200];
char *l2a(long l, int size) {
  sprintf(tmpbuf,"%*ld", size, l);
  if ((int)strlen(tmpbuf) != size)
    conv_error(strlen(tmpbuf),CV_NUMERIC_OVERFLOW);
  return tmpbuf;
}

/* Float to ascii - WDRogers Aug-16-94
   This function replaces the old f2a which was renamed f2a_
   Writes the float value as an ascii string with a maximum of size bytes
   and a fixed decimal point.  Returns pointer to the string.
   A static buffer is used and will be overwritten.  The string is NULL 
   terminated.
*/
char *f2a(float f, int size, int dec)
{
  double int_part = 0, dec_part = 0;
  int    dec_space, dec_need;
  char   dec_str[20], dp[2];

  dec_part = fabs(modf(f, &int_part)); /* no sign on int or dec parts */
  sprintf(tmpbuf, "%s%d", (f < 0) ? "-" : "", (int)fabs(int_part));       
  if ( tmpbuf[(f < 0) ? 1 : 0] == '0' )        /* remove leading 0, keep - */
	tmpbuf[(f < 0) ? 1 : 0] = '\0';

  strcpy(dp, strlen(tmpbuf) == (size - dec) ? "" : ".");
  strcat( tmpbuf, dp);                 /* add decimal point */

  dec_space = size - strlen(tmpbuf);
  dec_need = sprintf(dec_str, "%0*.0f", dec_space,
                     _pow[dec_space]*(float)dec_part);
/* no room for decimal portion - use rounded-off int_part with . if needed. */
  if ( dec_space < dec_need ) {
	sprintf(tmpbuf, "%*.0f%s", size-strlen(dp), f, dp); 
	if ( tmpbuf[size] == '.' && dec == 0 )
		tmpbuf[size] = '\0'; 
  }
  else { /* dec_space > 0 */
	if ( atoi(dec_str) == 0 && (int)int_part == 0 )
		sprintf(tmpbuf, "%*s", size, "0");
	else
		strcat(tmpbuf, dec_str);
  }
  if ((int)strlen(tmpbuf) != size) { /* if field not filled correctly */
	printf(" error putting %f into an F%d.%d field\n", f, size, dec);
	printf(" field filled with blanks\n");
	conv_error(strlen(tmpbuf),CV_NUMERIC_OVERFLOW);
	sprintf(tmpbuf, "%*s", size, " "); /* fill field with blanks */
  }
  return tmpbuf;
}

/* Float to ascii
   This is the old f2a, but was renamed f2a_ just to keep it around for now
   Writes the float value as an ascii string with a maximum of size bytes
   and a fixed decimal point.  Returns pointer to the string.
   A static buffer is used and will be overwritten.  The string is NULL 
   terminated.
*/
char *f2a_(float f, int size, int dec) {
/* new code to handle decimals in integer field */
  f *= (float)_pow[dec];
  if (fabs(f - (float)((long)f)) > .0000001) { /* something must have been 
                                                 truncated, for example trying
                                                 to put 13.8 kv in pf record
                                                 because kv has no assumed
                                                 decimal point */
    sprintf(tmpbuf,"%-*g", size, f / _pow[dec]);
  } else {
    sprintf(tmpbuf,"%*ld", size, (long)f);
  }
  if ((int)strlen(tmpbuf) != size)
    conv_error(strlen(tmpbuf),CV_NUMERIC_OVERFLOW);
  return tmpbuf;
}

/* Read buf as appropriate for the type of field and store it in the 
   powerflow record arec.  Strings are left justified and numbers
   are right justified.  Buf is assumed to be a C type string, so
   trailing blanks are ignored, and whitespace characters are a delimeter.
   returns non zero if errors
*/
int put_fld_a(void *arec, char *buf, int fd) {
  int rec=fd / 256, fld=fd % 256;
  struct fld_table *ft = &(_ft[rec][fld]);
  int size = ft->asize;
  record = rec;
  field = fld;
  cv_error = 0;       
  switch(ft->bintype) {
    case 's': if (ft->asize > (int)strlen(buf)) {
                size = ft->bsize;
                memset(tmpbuf, ' ', ft->asize); /* init string to ' ' */
              }
              memcpy(tmpbuf, buf, strlen(buf));
              break;
    case 'f': f2a((float)atof(buf),size,ft->adec);
              break;
    case 'l': l2a(atol(buf),size);
              break;
    default: printf("undefined switch rec=%d, fld=%d\n", rec, fld);
  }
  memcpy((char *)arec + ft->aoffset, tmpbuf, size);
  return(cv_error);
}  
/* read buf as appropriate for the type of field and store it in the
   powerflow structure arec */
int put_fld_b(void *arec, char *buf, int fd) {
  int rec=fd / 256, fld=fd % 256;
  struct fld_table *ft = &(_ft[rec][fld]);
  int size = ft->bsize;
  record = rec;
  field = fld;
  cv_error = 0;       
  switch(ft->bintype) {
    case 's': if (ft->bsize > (int)strlen(buf)) {
                memset(tmpbuf, '\0', ft->bsize); /* init string to '\0' */
              } else ++size;
              memcpy(tmpbuf, buf, size);
              tmpbuf[ft->bsize] = '\0';
              break;
    case 'f': f2a((float)atof(buf),size,ft->adec);
              break;
    case 'l': l2a(atol(buf),size);
              break;
    default: printf("undefined switch rec=%d, fld=%d\n", rec, fld);
  }
  memcpy((char *)arec + ft->boffset, tmpbuf, size);
  return(cv_error);
}  

/* Read the requested field from the powerflow record and store
   it in a NULL terminated ascii string buf.
   returns non zero if errors
*/
int get_fld_a(char *buf, void *arec, int fd) {
  int rec=fd / 256, fld=fd % 256;
  struct fld_table *ft = &(_ft[rec][fld]);
  char *ascstr = (char *)arec + ft->aoffset;
  int size = ft->asize;
  int x;
  record = rec;
  field = fld;
  cv_error = 0;       
  switch(ft->bintype) {
    case 's': for (x=0; x < size; ++x) {
                buf[x] = ascstr[x];
              }
              buf[x] = '\0';
              break;
    case 'f': sprintf(buf,"%*.*f",ft->asize,ft->adec,
                a2f(ascstr,ft->asize,ft->adec));
              break;
    case 'l': sprintf(buf,"%*l",ft->asize,a2l(ascstr,ft->asize));
              break;
    default: printf("undefined switch rec=%d, fld=%d\n", rec, fld);
  }
  return(cv_error);
}  

/* Convert all fields in the powerflow record arec into their binary
   equivelent structure format.
   returns non zero if errors
*/
int cv_rec_a2b(void *brec, void *arec, int rec) {
  int fld=1;
  record=rec;
  cv_error = 0;
  while (_ft[rec][fld].asize > 0) {
    struct fld_table *ft = &(_ft[rec][fld]);
    char *ascstr = (char *)arec + ft->aoffset;
    void *binoff = (char *)brec + ft->boffset;
    char *binchr = (char *)brec + ft->boffset;
    int size = ft->bsize;
    field = fld;
    switch(ft->bintype) {
      case 's': if (ft->bsize > ft->asize) {
                  size = ft->asize;
                  memset(binoff, '\0', ft->bsize + 1); /* init to '\0' */
                } else if (ft->asize > ft->bsize) conv_error(size,CV_TRUNCATED);
                strncpy(binoff, ascstr, size );
                binchr[size]= '\0';
                break;
      case 'f': *(float *)binoff = (float) a2f(ascstr, ft->asize, ft->adec);
                break;
      case 'l': *(int *)binoff = (int) a2l(ascstr, ft->asize);
                break;
      default: printf("undefined switch rec=%d, fld=%d\n", rec, fld);
    }
    ++fld;
  }
  return(cv_error);
}

/* Convert rec from binary structure brec into powerflow record arec.
   returns non zero if errors
*/
int cv_rec_b2a(void *arec, void *brec, int rec) {
  int fld=1;
  record=rec;
  cv_error = 0;
  while (_ft[rec][fld].asize > 0) {
    struct fld_table *ft = &(_ft[rec][fld]);
    char *ascstr = (char *)arec + ft->aoffset;
    void *binoff = (char *)brec + ft->boffset;
    int size = ft->asize;
    field = fld;
    switch(ft->bintype) {
      case 's': { int sz=ft->asize;
                memset(tmpbuf, ' ', sz);
                if ((int)strlen(binoff) < sz) sz=strlen(binoff);
                else conv_error(strlen(binoff),CV_TRUNCATED);
                if (ft->bsize < sz) sz=ft->bsize;
                memcpy(tmpbuf, binoff, sz);
                break;
                }
      case 'f': f2a(*(float *)binoff,size,ft->adec);
                break;
      case 'l': l2a( (long) *(int *)binoff , size );
                break;
      default: printf("undefined switch rec=%d, fld=%d\n", rec, fld);
    }
    memcpy((char *)arec + ft->aoffset, tmpbuf, size);
    ++fld;
  }
  return(cv_error);
}

struct {
  char str[3];
  int  type;
  int  soln_type;
} tb[]={                   /* since this table is searched linearly
                              put most common record types first
                              and define 2 character keys before single
                              character keys */
        { "BD", DC_BUS  , DC_SOLN},
        { "BM", DC_BUS  , DC_SOLN},
        { "B",  AC_BUS  , AC_SOLN},
        { "+",  CBUS    , CBUS_SOLN},
        { "L*", L_LINE  , BRANCH_SOLN},
        { "LD", LD_LINE , BRANCH_SOLN},
        { "LM", LM_LINE , BRANCH_SOLN},
        { "L",  L_LINE  , BRANCH_SOLN},
        { "T*", T_LINE  , BRANCH_SOLN},
        { "TP", TP_LINE , BRANCH_SOLN},
        { "T",  T_LINE  , BRANCH_SOLN},
        { "E",  E_LINE  , BRANCH_SOLN},
        { "A",  AREA    , AREA_SOLN},
        { "I",  ITIE    , ITIE_SOLN},
        { "R ", R_LINE  , BRANCH_SOLN},
        { "RN", RN_LINE , BRANCH_SOLN},
        { "RQ", RQ_LINE , BRANCH_SOLN},
        { "RV", RV_LINE , BRANCH_SOLN},
        { "RM", RM_LINE , BRANCH_SOLN},
        { "RP", RP_LINE , BRANCH_SOLN},
        { "RZ", RZ_LINE , BRANCH_SOLN},
        { "X",  XDATA   , XDATA_SOLN},
        { ".",  COMM    },
        { "", 0}
};

int cv_pf_a2b(void *brec, void *ascrec) {
  int rtype=0;
  int len = strlen(ascrec);
  while (tb[rtype].type != 0) {
    if (strncmp(tb[rtype].str, ascrec, strlen(tb[rtype].str)) == 0) {
      int rsize = _ft[tb[rtype].type][0].asize;
      char arec[400];
      assert(rsize < 400);
      memcpy(arec, ascrec, len);
      if (len < rsize) 
        memset(&arec[len], ' ', rsize-len);    /* 14apr95wdr bug: was [len-1] */
      memset((pf_rec *)brec, '\0', sizeof(pf_rec));    /* zero out the pf_rec */
      return cv_rec_a2b(brec,arec,tb[rtype].type);
    }
    ++rtype;
  }
  printf("unsupported record type %-20s in cv_pf_a2b\n",ascrec);
  return -1;
}
int cv_pfs_a2b(void *brec, void *ascrec) {
  int rtype=0;
  int len = strlen(ascrec);
  /* printf("cv_pfs_a2b ascrec=%s~\n",ascrec); */
  while (tb[rtype].soln_type != 0) {
    if (strncmp(tb[rtype].str, ascrec, strlen(tb[rtype].str)) == 0) {
      int rsize = _ft[tb[rtype].soln_type][0].asize;
      char arec[400];
      assert(rsize < 400);
      memcpy(arec, ascrec, len);
      if (len < rsize) 
        memset(&arec[len], ' ', rsize-len);    /* 14apr95wdr bug: was [len-1] */
      memset(&(((pf_rec *)brec)->s), '\0', sizeof(solution_data)); /*zero soln*/
      return cv_rec_a2b(&(((pf_rec *)brec)->s),arec,tb[rtype].soln_type);
    }
    ++rtype;
  }
  printf("unsupported record type %-20s in cv_pfs_a2b\n",ascrec);
  return -1;
}
int cv_pf_b2a(void *ascrec, void *brec) {
  int rtype=0;
  while (tb[rtype].type != 0) {
    if (strncmp(tb[rtype].str, ((pf_rec *)brec)->i.ACbus.type,
        strlen(tb[rtype].str)) == 0) {
      int rsize = _ft[tb[rtype].type][0].asize;
      assert(rsize < 400);
      memset((char *)ascrec, ' ', rsize);
      ((char *)ascrec)[rsize]='\0';
      return cv_rec_b2a(ascrec,brec,tb[rtype].type);
    }
    ++rtype;
  }
  printf("unsupported record type %-20s in cv_pf_b2a\n",ascrec);
  return -1;
}
int cv_pfs_b2a(void *ascrec, void *brec) {
  int rtype=0;
  while (tb[rtype].soln_type != 0) {
    if (strncmp(tb[rtype].str, ((pf_rec *)brec)->i.ACbus.type,
        strlen(tb[rtype].str)) == 0) {
      int rsize = _ft[tb[rtype].soln_type][0].asize;
      assert(rsize < 400);
      memset((char *)ascrec, ' ', rsize);
      ((char *)ascrec)[rsize]='\0';
      return cv_rec_b2a(ascrec,&(((pf_rec *)brec)->s),tb[rtype].soln_type);
    }
    ++rtype;
  }
  printf("unsupported record type %-20s in cv_pf_b2a\n",ascrec);
  return -1;
}
void pf_init_rec(void *brec,int rec) {
  struct fld_table *ft = &(_ft[rec][0]);
  memset(brec,'\0',ft->bsize);
}

#ifdef TEST
static char cr[]="BS OFFA SLACK 13.8A0                                     1050  10                          ";
static char dr[100];
static char er[100];
int pf_rec_bus(pf_rec *r, char *action);
pf_rec r[100];   /* array for 100 records */
pf_comments rc;   
main() {
  char str[200];
  int cnt=0;
/*  
  FILE *fp;
  char line[200];
  if((fp = fopen("input.rec", "r")) != NULL) {
    while(fgets(line, 200, fp) != NULL) {
      cv_pf_a2b(&r[cnt].i, line);
      cv_pf_b2a(str, &r[cnt].i);
      printf("line=%s\n str=%s\n",line,str);
      if (++cnt > 100) break;
    }
    fclose(fp);
  }
*/
  cv_pf_a2b(&r[0].i, cr);
/*// must have separate subroutine to handle solution a2b & b2a */
/*// because record types are the same as input and must distinguish */
/*  cnt=0;   
  if((fp = fopen("soln.rec", "r")) != NULL) {
    while(fgets(line, 200, fp) != NULL) {
      cv_pf_a2b(&r[cnt].s, line);
      if (++cnt > 100) break;
    }
    fclose(fp);
  }
*/
  printf("cv_rec_a2b = %d\n",cv_rec_a2b(dr,cr,1));
  printf("cv_rec_b2a = %d\n",cv_rec_b2a(er,dr,1));
  get_fld_a(str, er, AC_BUS_PMAX);
  put_fld_a(er, str, AC_BUS_QMIN);
  pf_load_netdata("fgrove.net");
  pf_load_changes("*\nC First change record\nC Next change record");
  pf_solution();
  pf_rec_bus(&(r[0]), "f");
  pf_rec_bus(&(r[0]), "n");
  pf_rec_bus(&(r[0]), "g");
  pf_rec_area(&(r[0]), "f");
  pf_rec_area(&(r[0]), "n");
  pf_rec_area(&(r[0]), "g");
  pf_rec_branch(&(r[0]), "f");
  pf_rec_branch(&(r[0]), "f1");
  pf_rec_branch(&(r[0]), "f2");
  pf_rec_branch(&(r[0]), "n");
  pf_rec_branch(&(r[0]), "n1");
  pf_rec_branch(&(r[0]), "n2");
  pf_rec_branch(&(r[0]), "g");
  pf_rec_cbus(&(r[0]), "f");
  pf_rec_cbus(&(r[0]), "f1");
  pf_rec_cbus(&(r[0]), "n");
  pf_rec_cbus(&(r[0]), "n1");
  pf_rec_cbus(&(r[0]), "g");
  pf_rec_comments(&rc, "g");
  pf_rec_itie(&(r[0]), "f");
  pf_rec_itie(&(r[0]), "n");
  pf_rec_itie(&(r[0]), "g");
  pf_rec_qcurve(&(r[0]), "f");
  pf_rec_qcurve(&(r[0]), "n");
  pf_rec_qcurve(&(r[0]), "g");
  pf_rec_xdata(&(r[0]), "f");
  pf_rec_xdata(&(r[0]), "n");
  pf_rec_xdata(&(r[0]), "g");
/* read input record from powerflow
   cv_rec_a2b( structure, pf_record, record type)
   cv_pf_a2b( structure, pf_record)
   cv_pf_b2a( pf_record, structure)      
*/
  return 0;
}
#endif
