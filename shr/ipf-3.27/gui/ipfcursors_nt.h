/************************************************************************\
**************************************************************************
**                                                                      **
**   ipfcursors - bitmap collection of all cursors used by toolbox      **
**                                                                      **
**      used by: utils.c                                                **
**                                                                      **
**************************************************************************
\************************************************************************/

char input_cursor_background[] = {
   '\x00', '\x00', '\x80', '\x1f', '\xf0', '\x7f', '\xf0', '\x7f', '\xf0', '\xff', '\xff', '\xff',
   '\xff', '\xff', '\xff', '\xff', '\xff', '\xff', '\xff', '\xff', '\xf0', '\xff', '\xf0', '\x7f',
   '\xf0', '\x7f', '\x80', '\x1f', '\x00', '\x00', '\x00', '\x00'};

char input_cursor_foreground[] = {
   '\x00', '\x00', '\x00', '\x00', '\x00', '\x0f', '\x20', '\x3f', '\x60', '\x30', '\xe0', '\x60',
   '\xfe', '\x61', '\xfe', '\x63', '\xfe', '\x61', '\xe0', '\x60', '\x60', '\x30', '\x20', '\x3f',
   '\x00', '\x0f', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00'};

char output_cursor_background[] = {
   '\x00', '\x00', '\xf8', '\x01', '\xfe', '\x07', '\xfe', '\x0f', '\xff', '\x1f', '\xff', '\x3f',
   '\xff', '\x7f', '\xff', '\xff', '\xff', '\x7f', '\xff', '\x3f', '\xff', '\x1f', '\xfe', '\x0f',
   '\xfe', '\x07', '\xf8', '\x01', '\x00', '\x00', '\x00', '\x00'};

char output_cursor_foreground[] = {
   '\x00', '\x00', '\x00', '\x00', '\xf0', '\x00', '\xfc', '\x04', '\x0c', '\x0c', '\x06', '\x1c',
   '\xe6', '\x3f', '\xe6', '\x7f', '\xe6', '\x3f', '\x06', '\x1c', '\x0c', '\x0c', '\xfc', '\x04',
   '\xf0', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00'};

char io_cursor_background[] = {
   '\xf0', '\x03', '\xf0', '\x03', '\xfc', '\x07', '\xfc', '\x0f', '\xfc', '\x1f', '\xff', '\x3f',
   '\xff', '\x7f', '\xff', '\xff', '\xff', '\x7f', '\xff', '\x3f', '\xfc', '\x1f', '\xfc', '\x0f',
   '\xfc', '\x07', '\xf0', '\x03', '\xf0', '\x03', '\x00', '\x00'};

char io_cursor_foreground[] = {
   '\x00', '\x00', '\xe0', '\x01', '\xe0', '\x01', '\x08', '\x04', '\x18', '\x0c', '\x38', '\x1c',
   '\x7e', '\x3f', '\xfe', '\x7e', '\x7e', '\x3f', '\x38', '\x1c', '\x18', '\x0c', '\x08', '\x04',
   '\xe0', '\x01', '\xe0', '\x01', '\x00', '\x00', '\x00', '\x00'};

char move_cursor_foreground[] = {
   '\x00', '\x00', '\x02', '\x00', '\x06', '\x00', '\x0e', '\x00', '\x1e', '\x00', '\x3e', '\x00',
   '\x7e', '\x00', '\xfe', '\x00', '\x3e', '\x00', '\x3e', '\x00', '\x32', '\x00', '\x60', '\x00',
   '\x60', '\x00', '\xc0', '\x00', '\xc0', '\x00', '\x00', '\x00'};

char move_cursor_background[] = {
   '\x03', '\x00', '\x07', '\x00', '\x0f', '\x00', '\x1f', '\x00', '\x3f', '\x00', '\x7f', '\x00',
   '\xff', '\x00', '\xff', '\x01', '\xff', '\x00', '\x7f', '\x00', '\x7f', '\x00', '\xf3', '\x00',
   '\xf0', '\x01', '\xe0', '\x01', '\xe0', '\x01', '\xc0', '\x01'};

char del_cursor_background[] = {
   '\xF0', '\x0F', '\xFC', '\x3F', '\xFE', '\x7F', '\xFE', '\x7F', '\xFF', '\xFF', '\xFF', '\xFF',
   '\xFF', '\xFF', '\xFF', '\xFF', '\xFF', '\xFF', '\xFF', '\xFF', '\xFF', '\xFF', '\xFF', '\xFF',
   '\xFE', '\x7F', '\xFE', '\x7F', '\xFC', '\x3F', '\xF0', '\x0F'};

char del_cursor_foreground[] = {
   '\x00', '\x00', '\xE0', '\x07', '\xF8', '\x1F', '\x3C', '\x3C', '\x0C', '\x3C', '\x0E', '\x7E',
   '\x06', '\x67', '\x86', '\x63', '\xC6', '\x61', '\xE6', '\x60', '\x7E', '\x70', '\x3C', '\x30',
   '\x3C', '\x3C', '\xF8', '\x1F', '\xE0', '\x07', '\x00', '\x00'};

char undel_cursor_background[] = {
   '\xe0', '\x03', '\xf8', '\x0f', '\xfc', '\x1f', '\xfe', '\x3f', '\xfe', '\x3f', '\xff', '\x7f',
   '\xff', '\x7f', '\xff', '\x7f', '\xff', '\x7f', '\xff', '\x7f', '\xfe', '\x3f', '\xfe', '\x3f',
   '\xfc', '\x1f', '\xf8', '\x0f', '\xe0', '\x03', '\x00', '\x00'};

char undel_cursor_foreground[] = {
   '\x00', '\x00', '\xe0', '\x03', '\xf8', '\x0f', '\xfc', '\x1f', '\xfc', '\x1f', '\xfe', '\x3f',
   '\xfe', '\x3f', '\xfe', '\x3f', '\xfe', '\x3f', '\xfe', '\x3f', '\xfc', '\x1f', '\xfc', '\x1f',
   '\xf8', '\x0f', '\xe0', '\x03', '\x00', '\x00', '\x00', '\x00'};

char bus_cursor_background[] = {
   '\xFF', '\xFF', '\xFF', '\xFF', '\xFF', '\xFF', '\xFF', '\xFF', '\x0F', '\xF0', '\x0F', '\xF0',
   '\x0F', '\xF0', '\x0F', '\xF0', '\x0F', '\xF0', '\x0F', '\xF0', '\x0F', '\xF0', '\x0F', '\xF0',
   '\xFF', '\xFF', '\xFF', '\xFF', '\xFF', '\xFF', '\xFF', '\xFF'};

char bus_cursor_foreground[] = {
   '\x00', '\x00', '\xFE', '\x7F', '\xFE', '\x7F', '\x06', '\x60', '\x06', '\x60', '\x06', '\x60',
   '\x06', '\x60', '\x06', '\x60', '\x06', '\x60', '\x06', '\x60', '\x06', '\x60', '\x06', '\x60',
   '\x06', '\x60', '\xFE', '\x7F', '\xFE', '\x7F', '\x00', '\x00'};

char line_cursor_background[] = {
   '\x00', '\x00', '\x00', '\x00', '\x78', '\x00', '\xF8', '\x01', '\xF8', '\x07', '\xF8', '\x1F',
   '\xFF', '\xFF', '\xFF', '\xFF', '\xFF', '\xFF', '\xFF', '\xFF', '\xF8', '\x1F', '\xF8', '\x07',
   '\xF8', '\x01', '\x78', '\x00', '\x00', '\x00', '\x00', '\x00'};

char line_cursor_foreground[] = {
   '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x30', '\x00', '\xF0', '\x00', '\xC0', '\x03',
   '\x00', '\x0F', '\xFE', '\x7F', '\xFE', '\x7F', '\x00', '\x0F', '\xC0', '\x03', '\xF0', '\x00',
   '\x30', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00'};

char draw_cursor_background[] = {
   '\xc0', '\x01', '\xc0', '\x01', '\xc0', '\x01', '\xc0', '\x01', '\xc0', '\x01', '\xc0', '\x01',
   '\xff', '\x7f', '\xff', '\x7f', '\xff', '\x7f', '\xc0', '\x01', '\xc0', '\x01', '\xc0', '\x01',
   '\xc0', '\x01', '\xc0', '\x01', '\xc0', '\x01', '\x00', '\x00'};

char draw_cursor_foreground[] = {
   '\x00', '\x00', '\x80', '\x00', '\x80', '\x00', '\x80', '\x00', '\x80', '\x00', '\x80', '\x00',
   '\x80', '\x00', '\xfe', '\x3f', '\x80', '\x00', '\x80', '\x00', '\x80', '\x00', '\x80', '\x00',
   '\x80', '\x00', '\x80', '\x00', '\x00', '\x00', '\x00', '\x00'};

char alph_cursor_background[] = {
   '\xF8', '\x1F', '\xf8', '\x1F', '\xF8', '\x1F', '\xF8', '\x1F', '\xC0', '\x02', '\xC0', '\x02',
   '\xC0', '\x02', '\xC0', '\x02', '\xF0', '\x0F', '\xF0', '\x0F', '\xF0', '\x0F', '\xF0', '\x0F',
   '\xF8', '\x1F', '\xF8', '\x1F', '\xF8', '\x1F', '\xF8', '\x1F'};

char alph_cursor_foreground[] = {
   '\x00', '\x00', '\x70', '\x0E', '\x70', '\x0E', '\x80', '\x01', '\x80', '\x01', '\x80', '\x01',
   '\x80', '\x01', '\x80', '\x01', '\x80', '\x01', '\xE0', '\x07', '\xE0', '\x07', '\x80', '\x01',
   '\x80', '\x01', '\x70', '\x0E', '\x70', '\x0E', '\x00', '\x00'};

static char star_cursor_foreground[] = {
   '\x00', '\x00', '\x02', '\x40', '\x04', '\x20', '\x08', '\x10', '\xf0', '\x0f', '\x10', '\x08',
   '\x10', '\x08', '\x10', '\x08', '\x10', '\x08', '\x10', '\x08', '\x10', '\x08', '\xf0', '\x0f',
   '\x08', '\x10', '\x04', '\x20', '\x02', '\x40', '\x00', '\x00'};

static char star_cursor_background[] = {
   '\x03', '\xc0', '\x07', '\xe0', '\x0e', '\x70', '\xfc', '\x3f', '\xf8', '\x1f', '\xf8', '\x1f',
   '\x38', '\x1c', '\x38', '\x1c', '\x38', '\x1c', '\x38', '\x1c', '\xf8', '\x1f', '\xf8', '\x1f',
   '\xfc', '\x3f', '\x0e', '\x70', '\x07', '\xe0', '\x03', '\xc0'};

char xfmr_cursor_background[] = {
       '\xE0', '\x01', '\xE0', '\x03', '\xE0', '\x07', '\xC0', '\x0F', '\x80', '\x0F', '\xC0', '\x0F',
       '\xFF', '\xFF', '\xFF', '\xFF', '\xFF', '\xFF', '\xFF', '\xFF', '\xC0', '\x0F', '\x80', '\x0F',
       '\xC0', '\x0F', '\xE0', '\x07', '\xE0', '\x03', '\xE0', '\x01', };

char xfmr_cursor_foreground[] = {
       '\x00', '\x00', '\xC0', '\x00', '\xB0', '\x01', '\x00', '\x03', '\x00', '\x06', '\x00', '\x03',
       '\x80', '\x01', '\xFE', '\x7F', '\xFE', '\x7F', '\x80', '\x01', '\x00', '\x03', '\x00', '\x06',
       '\x00', '\x03', '\x80', '\x01', '\xC0', '\x00', '\x00', '\x00', };

char load_cursor_background[] = {
       '\xC0', '\x03', '\xFE', '\x7F', '\xFE', '\x7F', '\xFE', '\x7F', '\xFE', '\x7F', '\xFE', '\x7F',
       '\xFE', '\x7F', '\xFE', '\x7F', '\xFE', '\x7F', '\xFE', '\x7F', '\xFE', '\x7F', '\xFE', '\x7F',
       '\xFE', '\x7F', '\xFE', '\x7F', '\xFE', '\x7F', '\xC0', '\x03', };

char load_cursor_foreground[] = {
       '\x00', '\x00', '\x80', '\x01', '\xFC', '\x3F', '\xFC', '\x3F', '\x0C', '\x30', '\xEC', '\x37',
       '\xEC', '\x37', '\x8C', '\x33', '\xCC', '\x31', '\xEC', '\x37', '\xEC', '\x37', '\x0C', '\x30',
       '\xFC', '\x3F', '\xFC', '\x3F', '\x80', '\x01', '\x00', '\x00', };

char outg_cursor_background[] = {
       '\x3F', '\x00', '\x7F', '\x00', '\xFF', '\x00', '\xFE', '\x7C', '\xF8', '\xFE', '\xF0', '\xFF',
       '\xE0', '\xFF', '\xE0', '\x3F', '\xE0', '\x3F', '\xE0', '\xFF', '\xF0', '\xFF', '\xF8', '\xFE',
       '\xFE', '\x7C', '\xFF', '\x00', '\x7F', '\x00', '\x3F', '\x00', };

char outg_cursor_foreground[] = {
       '\x00', '\x00', '\x1E', '\x00', '\x3C', '\x00', '\x70', '\x00', '\xE0', '\x38', '\xC0', '\x7C',
       '\xC0', '\x1F', '\x80', '\x04', '\x80', '\x04', '\xC0', '\x1F', '\xC0', '\x7C', '\xE0', '\x38',
       '\x70', '\x00', '\x3C', '\x00', '\x1E', '\x00', '\x00', '\x00', };

char grp_cursor_foreground[] = {
   '\x00', '\x00', '\x00', '\x00', '\x3c', '\x3c', '\x34', '\x2c', '\x0c', '\x30', '\x0c', '\x30',
   '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x0c', '\x30', '\x0c', '\x30',
   '\x34', '\x2c', '\x3c', '\x3c', '\x00', '\x00', '\x00', '\x00'};

char grp_cursor_background[] = {
   '\x00', '\x00', '\xfe', '\x7f', '\xfe', '\x7f', '\xfe', '\x7f', '\xfe', '\x7f', '\xfe', '\x7f',
   '\xfe', '\x7f', '\xfe', '\x7f', '\xfe', '\x7f', '\xfe', '\x7f', '\xfe', '\x7f', '\xfe', '\x7f',
   '\xfe', '\x7f', '\xfe', '\x7f', '\xfe', '\x7f', '\x00', '\x00'};

char plot_cursor_foreground[] = {
   '\x00', '\x00', '\x00', '\x00', '\x3c', '\x3c', '\x04', '\x20', '\xc4', '\x23', '\xe4', '\x27',
   '\x60', '\x00', '\x60', '\x0f', '\x60', '\x0f', '\x60', '\x06', '\xe4', '\x27', '\xc4', '\x23',
   '\x04', '\x20', '\x3c', '\x3c', '\x00', '\x00', '\x00', '\x00'};


char plot_cursor_background[] = {
   '\x00', '\x00', '\xfe', '\x7f', '\xfe', '\x7f', '\xfe', '\x7f', '\xfe', '\x7f', '\xfe', '\x7f',
   '\xfe', '\x7f', '\xfe', '\x7f', '\xfe', '\x7f', '\xfe', '\x7f', '\xfe', '\x7f', '\xfe', '\x7f',
   '\xfe', '\x7f', '\xfe', '\x7f', '\xfe', '\x7f', '\x00', '\x00'};

