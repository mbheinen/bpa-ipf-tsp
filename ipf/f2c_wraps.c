/*****************************************************************************
* file: f2c_wraps.c
* author: Jay Coleman
* purpose: This is a set of "C" wrappers for "C" routines to allow
*          Fortran to call "C" routines.
******************************************************************************/
/* #define DEBUG */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#if defined(VMS)
#include <descrip.h>

#elif defined(WIN32) || defined (__WATCOMC__)
typedef struct descriptor {
    char          *addr;
    unsigned       len;
} descriptor;
#endif

#include "ipf_ipc.h"

/* prototypes */
int srv_cmdprs( char *inbuf, char *outbuf );


/********************************************************************/
#if defined(VMS)
int srv_cmdprs_cw( struct dsc$descriptor_d *dsc_inbuf,
                   struct dsc$descriptor_d *dsc_outbuf )
{
   char  *inbuf = (char *)dsc_inbuf->dsc$a_pointer;
   char  *outbuf = (char *)dsc_outbuf->dsc$a_pointer;
#elif defined(WIN32) || defined (__WATCOMC__)
#pragma aux srv_cmdprs_cw "^"
int srv_cmdprs_cw( descriptor *dsc_inbuf, descriptor *dsc_outbuf )
{
   char  *inbuf = (char *)dsc_inbuf->addr;
   char  *outbuf = (char *)dsc_outbuf->addr;
#elif defined(UNDERSCORE)
int srv_cmdprs_cw_( char *inbuf, char *outbuf )
{
#elif defined(DUNDERSCORE)
int srv_cmdprs_cw__( char *inbuf, char *outbuf )
{
#else
int srv_cmdprs_cw( char *inbuf, char *outbuf )
{
#endif

   return srv_cmdprs( inbuf, outbuf );
}
/********************************************************************/
