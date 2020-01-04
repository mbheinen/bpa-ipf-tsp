/*************************************************************************\
***************************************************************************
**                                                                       **
**	theCursors                                                       **
**                                                                       **
**	                                                                 **
**                                                                       **
***************************************************************************
\*************************************************************************/

#define CURSOR_WIDTH  16
#define CURSOR_HEIGHT 16

char bus_cursor_background[] =
     {
       0XFF, 0XFF, 0XFF, 0XFF,
       0XFF, 0XFF, 0XFF, 0XFF,
       0X0F, 0XF0, 0X0F, 0XF0,
       0X0F, 0XF0, 0X0F, 0XF0,
       0X0F, 0XF0, 0X0F, 0XF0,
       0X0F, 0XF0, 0X0F, 0XF0,
       0XFF, 0XFF, 0XFF, 0XFF,
       0XFF, 0XFF, 0XFF, 0XFF,
     };

char bus_cursor_foreground[] =
     {
       0X00, 0X00, 0XFE, 0X7F,
       0XFE, 0X7F, 0X06, 0X60,
       0X06, 0X60, 0X06, 0X60,
       0X06, 0X60, 0X06, 0X60,
       0X06, 0X60, 0X06, 0X60,
       0X06, 0X60, 0X06, 0X60,
       0X06, 0X60, 0XFE, 0X7F,
       0XFE, 0X7F, 0X00, 0X00,
     };

char line_cursor_background[] =
     {
       0X00, 0X00, 0X00, 0X00,
       0X78, 0X00, 0XF8, 0X01,
       0XF8, 0X07, 0XF8, 0X1F,
       0XFF, 0XFF, 0XFF, 0XFF,
       0XFF, 0XFF, 0XFF, 0XFF,
       0XF8, 0X1F, 0XF8, 0X07,
       0XF8, 0X01, 0X78, 0X00,
       0X00, 0X00, 0X00, 0X00,
     };

char line_cursor_foreground[] =
     {
       0X00, 0X00, 0X00, 0X00,
       0X00, 0X00, 0X30, 0X00,
       0XF0, 0X00, 0XC0, 0X03,
       0X00, 0X0F, 0XFE, 0X7F,
       0XFE, 0X7F, 0X00, 0X0F,
       0XC0, 0X03, 0XF0, 0X00,
       0X30, 0X00, 0X00, 0X00,
       0X00, 0X00, 0X00, 0X00,
     };

char xfmr_cursor_background[] =
     {
       0XE0, 0X01, 0XE0, 0X03,
       0XE0, 0X07, 0XC0, 0X0F,
       0X80, 0X0F, 0XC0, 0X0F,
       0XFF, 0XFF, 0XFF, 0XFF,
       0XFF, 0XFF, 0XFF, 0XFF,
       0XC0, 0X0F, 0X80, 0X0F,
       0XC0, 0X0F, 0XE0, 0X07,
       0XE0, 0X03, 0XE0, 0X01,
     };

char xfmr_cursor_foreground[] =
     {
       0X00, 0X00, 0XC0, 0X00,
       0XB0, 0X01, 0X00, 0X03,
       0X00, 0X06, 0X00, 0X03,
       0X80, 0X01, 0XFE, 0X7F,
       0XFE, 0X7F, 0X80, 0X01,
       0X00, 0X03, 0X00, 0X06,
       0X00, 0X03, 0X80, 0X01,
       0XC0, 0X00, 0X00, 0X00,
     };

char load_cursor_background[] =
     {
       0XC0, 0X03, 0XFE, 0X7F,
       0XFE, 0X7F, 0XFE, 0X7F,
       0XFE, 0X7F, 0XFE, 0X7F,
       0XFE, 0X7F, 0XFE, 0X7F,
       0XFE, 0X7F, 0XFE, 0X7F,
       0XFE, 0X7F, 0XFE, 0X7F,
       0XFE, 0X7F, 0XFE, 0X7F,
       0XFE, 0X7F, 0XC0, 0X03,
     };

char load_cursor_foreground[] =
     {
       0X00, 0X00, 0X80, 0X01,
       0XFC, 0X3F, 0XFC, 0X3F,
       0X0C, 0X30, 0XEC, 0X37,
       0XEC, 0X37, 0X8C, 0X33,
       0XCC, 0X31, 0XEC, 0X37,
       0XEC, 0X37, 0X0C, 0X30,
       0XFC, 0X3F, 0XFC, 0X3F,
       0X80, 0X01, 0X00, 0X00,
     };

char outg_cursor_background[] =
     {
       0X3F, 0X00, 0X7F, 0X00,
       0XFF, 0X00, 0XFE, 0X7C,
       0XF8, 0XFE, 0XF0, 0XFF,
       0XE0, 0XFF, 0XE0, 0X3F,
       0XE0, 0X3F, 0XE0, 0XFF,
       0XF0, 0XFF, 0XF8, 0XFE,
       0XFE, 0X7C, 0XFF, 0X00,
       0X7F, 0X00, 0X3F, 0X00,
     };

char outg_cursor_foreground[] =
     {
       0X00, 0X00, 0X1E, 0X00,
       0X3C, 0X00, 0X70, 0X00,
       0XE0, 0X38, 0XC0, 0X7C,
       0XC0, 0X1F, 0X80, 0X04,
       0X80, 0X04, 0XC0, 0X1F,
       0XC0, 0X7C, 0XE0, 0X38,
       0X70, 0X00, 0X3C, 0X00,
       0X1E, 0X00, 0X00, 0X00,
     };

char kill_cursor_background[] =
     {
       0XF0, 0X0F, 0XFC, 0X3F,
       0XFE, 0X7F, 0XFE, 0X7F,
       0XFF, 0XFF, 0XFF, 0XFF,
       0XFF, 0XFF, 0XFF, 0XFF,
       0XFF, 0XFF, 0XFF, 0XFF,
       0XFF, 0XFF, 0XFF, 0XFF,
       0XFE, 0X7F, 0XFE, 0X7F,
       0XFC, 0X3F, 0XF0, 0X0F,
     };

char kill_cursor_foreground[] =
     {
       0X00, 0X00, 0XE0, 0X07,
       0XF8, 0X1F, 0X3C, 0X3C,
       0X0C, 0X3C, 0X0E, 0X7E,
       0X06, 0X67, 0X86, 0X63,
       0XC6, 0X61, 0XE6, 0X60,
       0X7E, 0X70, 0X3C, 0X30,
       0X3C, 0X3C, 0XF8, 0X1F,
       0XE0, 0X07, 0X00, 0X00,
     };

char alph_cursor_background[] =
     {
       0XF8, 0X1F, 0Xf8, 0X1F,
       0XF8, 0X1F, 0XF8, 0X1F,
       0XC0, 0X02, 0XC0, 0X02,
       0XC0, 0X02, 0XC0, 0X02,
       0XF0, 0X0F, 0XF0, 0X0F,
       0XF0, 0X0F, 0XF0, 0X0F,
       0XF8, 0X1F, 0XF8, 0X1F,
       0XF8, 0X1F, 0XF8, 0X1F,
     };

char alph_cursor_foreground[] =
     {
       0X00, 0X00, 0X70, 0X0E,
       0X70, 0X0E, 0X80, 0X01,
       0X80, 0X01, 0X80, 0X01,
       0X80, 0X01, 0X80, 0X01,
       0X80, 0X01, 0XE0, 0X07,
       0XE0, 0X07, 0X80, 0X01,
       0X80, 0X01, 0X70, 0X0E,
       0X70, 0X0E, 0X00, 0X00,
     };

