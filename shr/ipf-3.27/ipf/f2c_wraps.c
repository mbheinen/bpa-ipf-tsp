/* #define DEBUG */
/*****************************************************************************
* file: f2c_wraps.c
* author: Jay Coleman
* purpose: This is a set of "C" wrappers for "C" routines to allow
*          Fortran to call "C" routines.
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#ifdef VMS
#include <descrip.h>
#endif /* VMS */

#ifdef WIN32
typedef struct descriptor {
    char          *addr;
    unsigned       len;
} descriptor;
#endif
#include "ipf_ipc.h"

/* prototypes */
int srv_cmdprs( char *inbuf, char *outbuf );


/********************************************************************
 ********************************************************************/
#ifdef VMS
int srv_cmdprs_cw( struct dsc$descriptor_d *dsc_inbuf,
                   struct dsc$descriptor_d *dsc_outbuf )
{
   char  *inbuf = (char *)dsc_inbuf->dsc$a_pointer;
   char  *outbuf = (char *)dsc_outbuf->dsc$a_pointer;
#elif WIN32
#pragma aux srv_cmdprs_cw "^"
int srv_cmdprs_cw( descriptor *dsc_inbuf, descriptor *dsc_outbuf )
{
   char  *inbuf = (char *)dsc_inbuf->addr;
   char  *outbuf = (char *)dsc_outbuf->addr;
#else /* VMS */
#ifdef UNDERSCORE
int srv_cmdprs_cw_( char *inbuf, char *outbuf )
{
#else /* UNDERSCORE */
int srv_cmdprs_cw( char *inbuf, char *outbuf )
{
#endif /* UNDERSCORE */
#endif /* VMS */

   return srv_cmdprs( inbuf, outbuf );
}
/********************************************************************
 ********************************************************************/
