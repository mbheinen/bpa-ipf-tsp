/* #define DEBUG */
/*****************************************************************************
* file: c2f_wraps.c
* author: Jay Coleman
* purpose: This is a set of "C" wrappers for Fortran routines to allow
*          "C" to call fortran routines.
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#if defined VMS
#include <descrip.h>

#elif defined WIN32
typedef struct descriptor {
    char          *addr;
    unsigned       len;
} descriptor;
#endif

#include "ipf_ipc.h"


/* prototypes */


#if defined VMS
   void apnd_stat( struct dsc$descriptor_d *, int *,
                   struct dsc$descriptor_d * );
   int srv_fcmdprs( struct dsc$descriptor_d *, struct dsc$descriptor_d * );
   void plot_load( struct dsc$descriptor_d *, struct dsc$descriptor_d *,
                   struct dsc$descriptor_d * );

#elif defined WIN32
#pragma aux ipf_fexit_cw "^"
#pragma aux initlz_cw "^"
#pragma aux pfinit_cw "^"
#pragma aux set_batch "^"
#pragma aux apnd_stat_cw "^"
#pragma aux srv_fcmdprs_cw "^"
#pragma aux plot_load_cw "^"
#pragma aux cutting_sub_cw "^";
#pragma aux net_data_sub_cw "^";
#pragma aux ipf_reports_cw "^";
#pragma aux pf_cmd_cw "^";
#pragma aux ipf_fexit "^"
#pragma aux initlz "^"
#pragma aux pfinit "^"
#pragma aux apnd_stat "^"
#pragma aux srv_fcmdprs "^"
#pragma aux plot_load "^"
#pragma aux cutting_sub "^";
#pragma aux net_data_sub "^";
#pragma aux ipf_reports "^";
#pragma aux pf_cmd "^";
   int  apnd_stat( descriptor *, int *, descriptor *);
   int srv_fcmdprs( descriptor * , descriptor *);
   void plot_load( descriptor * , descriptor *, descriptor *);

#elif defined UNDERSCORE
   void apnd_stat_( char *, int *, char *, int *, int * );
   int srv_fcmdprs_( char * , char *, int *, int * );
   void plot_load_( char * , char *, char * , int *, int *, int * );

#elif defined DUNDERSCORE
   void apnd_stat__( char *, int *, char *, int *, int * );
   int srv_fcmdprs__( char * , char *, int *, int * );
   void plot_load__( char * , char *, char * , int *, int *, int * );

#else /* UNIX with NO Underscore */
   void apnd_stat( char *, int *, char *, int *, int * );
   int srv_fcmdprs( char * , char *, int *, int * );
   void plot_load( char * , char *, char * , int *, int *, int * );

#endif /* VMS, WIN32, UNDERSCORE */

/* put any routines here that only have an "underscore" dependency */

#if defined UNDERSCORE
   void initlz_( int * );
   void pfinit_( void );
   void set_batch_( void );
   void ipf_fexit_( void );
   void cutting_sub_();
   void net_data_sub_();
   void ipf_reports_();
   void pf_cmd_();

#elif defined DUNDERSCORE
   void initlz_( int * );
   void pfinit_( void );
   void set_batch__( void );
   void ipf_fexit__( void );
   void cutting_sub__();
   void net_data_sub__();
   void ipf_reports__();
   void pf_cmd__();

#else /* UNDERSCORE */
   void initlz( int * );
   void pfinit( void );
   void set_batch( void );
   void ipf_fexit( void );
   void cutting_sub();
   void net_data_sub();
   void ipf_reports();
   void pf_cmd();

#endif /* UNDERSCORE */


#define FNAM_SZ 60
#define RNAM_SZ 15
static  int   i8k = FORT_BUFSIZE, i15 = RNAM_SZ, i60 = FNAM_SZ;


/********************************************************************
 ********************************************************************/
void cutting_sub_cw( int argc, char *argv[]  )
{
#if defined UNDERSCORE
   cutting_sub_();
#elif defined DUNDERSCORE
   cutting_sub__();
#else
   cutting_sub();
#endif
   return;
}
/********************************************************************
 ********************************************************************/
void net_data_sub_cw( int argc, char *argv[]  )
{
#if defined UNDERSCORE
   net_data_sub_();
#elif defined DUNDERSCORE
   net_data_sub__();
#else
   net_data_sub();
#endif
   return;
}
/********************************************************************
 ********************************************************************/
void ipf_reports_cw( int argc, char *argv[]  )
{
#if defined UNDERSCORE
   ipf_reports_();
#elif defined DUNDERSCORE
   ipf_reports__();
#else
   ipf_reports();
#endif
   return;
}
/********************************************************************
 ********************************************************************/
void pf_cmd_cw( int argc, char *argv[]  )
{
#if defined UNDERSCORE
   pf_cmd_();
#elif defined DUNDERSCORE
   pf_cmd__();
#else
   pf_cmd();
#endif
   return;
}
/********************************************************************
 ********************************************************************/
void ipf_fexit_cw( void )
{
#if defined UNDERSCORE
   ipf_fexit_();
#elif defined DUNDERSCORE
   ipf_fexit__();
#else
   ipf_fexit();
#endif
   return;
}
/********************************************************************
 ********************************************************************/
int initlz_cw( void )
{
   static int status;

#if defined UNDERSCORE
   initlz_(&status); /* init for cpyinpbuf */
#elif defined DUNDERSCORE
   initlz_(&status); /* init for cpyinpbuf */
#else
   initlz(&status);  /* init for cpyinpbuf */
#endif
   return status;
}
/********************************************************************
 ********************************************************************/
void pfinit_cw( void )
{
#if defined UNDERSCORE
   pfinit_();
#elif defined DUNDERSCORE
   pfinit_();
#else
   pfinit();
#endif
   return;
}
/********************************************************************
 ********************************************************************/
void set_batch_cw( void )
{
#if defined UNDERSCORE
   set_batch_();
#elif defined DUNDERSCORE
   set_batch__();
#else
   set_batch();
#endif
   return;
}
/********************************************************************
 ********************************************************************/
void apnd_stat_cw( char *outbuf, int status, char *rname )
{
   char  rname_2f[RNAM_SZ+1];
#if defined VMS
   struct dsc$descriptor_d dsc_rname_2f;
   struct dsc$descriptor_d dsc_outbuf;

   dsc_rname_2f.dsc$w_length = (unsigned short)RNAM_SZ;
   dsc_rname_2f.dsc$b_dtype = (char)DSC$K_DTYPE_T;
   dsc_rname_2f.dsc$b_class = (char)DSC$K_CLASS_D;
   dsc_rname_2f.dsc$a_pointer = (char *)rname_2f;

   dsc_outbuf.dsc$w_length = (unsigned short)FORT_BUFSIZE;
   dsc_outbuf.dsc$b_dtype = (char)DSC$K_DTYPE_T;
   dsc_outbuf.dsc$b_class = (char)DSC$K_CLASS_D;
   dsc_outbuf.dsc$a_pointer = (char *)outbuf;

#elif defined WIN32
   descriptor dsc_rname_2f, dsc_outbuf;

   dsc_outbuf.addr = outbuf;
   dsc_outbuf.len  = FORT_BUFSIZE;

   dsc_rname_2f.addr = rname;
   dsc_rname_2f.len  = RNAM_SZ;

#endif /* VMS */

   memset( rname_2f, '\0', RNAM_SZ+1 );
   strcpy( rname_2f, rname );

#if defined VMS
   apnd_stat( &dsc_outbuf, &status, &dsc_rname_2f );
#elif defined WIN32
   apnd_stat( &dsc_outbuf, &status, &dsc_rname_2f );
#elif defined UNDERSCORE
   apnd_stat_( outbuf, &status, rname_2f, &i8k, &i15 );
#elif defined DUNDERSCORE
   apnd_stat__( outbuf, &status, rname_2f, &i8k, &i15 );
#else /* UNDERSCORE */
   apnd_stat( outbuf, &status, rname_2f, &i8k, &i15 );
#endif /* UNDERSCORE */

   return;
}
/********************************************************************
 ********************************************************************/
int srv_fcmdprs_cw( char *cmdbuf, char *databuf )
{
   int status;
#if defined VMS
   struct dsc$descriptor_d dsc_cmdbuf;
   struct dsc$descriptor_d dsc_databuf;

   dsc_cmdbuf.dsc$w_length = (unsigned short)FORT_BUFSIZE;
   dsc_cmdbuf.dsc$b_dtype = (char)DSC$K_DTYPE_T;
   dsc_cmdbuf.dsc$b_class = (char)DSC$K_CLASS_D;
   dsc_cmdbuf.dsc$a_pointer = (char *)cmdbuf;

   dsc_databuf.dsc$w_length = (unsigned short)FORT_BUFSIZE;
   dsc_databuf.dsc$b_dtype = (char)DSC$K_DTYPE_T;
   dsc_databuf.dsc$b_class = (char)DSC$K_CLASS_D;
   dsc_databuf.dsc$a_pointer = (char *)databuf;

   status = srv_fcmdprs( &dsc_cmdbuf, &dsc_databuf );
#elif defined WIN32
   descriptor dsc_cmdbuf, dsc_databuf;

   dsc_cmdbuf.addr = cmdbuf;
   dsc_cmdbuf.len  = FORT_BUFSIZE;

   dsc_databuf.addr = databuf;
   dsc_databuf.len  = FORT_BUFSIZE;

   status = srv_fcmdprs( &dsc_cmdbuf, &dsc_databuf );
#elif defined UNDERSCORE
   status = srv_fcmdprs_( cmdbuf, databuf, &i8k, &i8k );
#elif defined DUNDERSCORE
   status = srv_fcmdprs__( cmdbuf, databuf, &i8k, &i8k );
#else /* UNDERSCORE */
   status = srv_fcmdprs( cmdbuf, databuf, &i8k, &i8k );
#endif /* UNDERSCORE */

   return status;
}
/********************************************************************
 ********************************************************************/
void plot_load_cw( char *coord_file, char *base1_file, char *base2_file )
{
   int status;
#if defined VMS
   struct dsc$descriptor_d dsc_coord;
   struct dsc$descriptor_d dsc_base1;
   struct dsc$descriptor_d dsc_base2;

   dsc_coord.dsc$w_length = (unsigned short)FNAM_SZ;
   dsc_coord.dsc$b_dtype = (char)DSC$K_DTYPE_T;
   dsc_coord.dsc$b_class = (char)DSC$K_CLASS_D;
   dsc_coord.dsc$a_pointer = (char *)coord_file;

   dsc_base1.dsc$w_length = (unsigned short)FNAM_SZ;
   dsc_base1.dsc$b_dtype = (char)DSC$K_DTYPE_T;
   dsc_base1.dsc$b_class = (char)DSC$K_CLASS_D;
   dsc_base1.dsc$a_pointer = (char *)base1_file;

   dsc_base2.dsc$w_length = (unsigned short)FNAM_SZ;
   dsc_base2.dsc$b_dtype = (char)DSC$K_DTYPE_T;
   dsc_base2.dsc$b_class = (char)DSC$K_CLASS_D;
   dsc_base2.dsc$a_pointer = (char *)base2_file;

   plot_load( &dsc_coord, &dsc_base1, &dsc_base2 );
#elif defined WIN32
   descriptor dsc_coord, dsc_base1, dsc_base2;

   dsc_coord.addr = coord_file;
   dsc_coord.len  = FNAM_SZ;

   dsc_base1.addr = base1_file;
   dsc_base1.len  = FNAM_SZ;

   dsc_base2.addr = base2_file;
   dsc_base2.len  = FNAM_SZ;

   plot_load( &dsc_coord, &dsc_base1, &dsc_base2 );
#elif defined UNDERSCORE
   plot_load_(coord_file,base1_file,base2_file,&i60,&i60,&i60);
#elif defined DUNDERSCORE
   plot_load__(coord_file,base1_file,base2_file,&i60,&i60,&i60);
#else /* UNDERSCORE */
   plot_load(coord_file,base1_file,base2_file,&i60,&i60,&i60);
#endif /* UNDERSCORE */

   return;
}
/********************************************************************
 ********************************************************************/
