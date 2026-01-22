#include "gdevprn.h"
#include "malloc_.h"
private dev_proc_map_rgb_color(tekink_map_rgb_color);
private dev_proc_map_color_rgb(tekink_map_color_rgb);
private dev_proc_print_page(tekink_print_page);
private gx_device_procs tekink_procs =
prn_color_procs(gdev_prn_open, gdev_prn_output_page, gdev_prn_close,
tekink_map_rgb_color, tekink_map_color_rgb);
const gx_device_printer far_data gs_tek4696_device =
prn_device(tekink_procs,"tek4696",
85,120,
120,120,
0.0,0.0,0.0,0.0,
4,
tekink_print_page);
static gx_color_index rgb_to_index[8]={1,6,12,4,10,2,8,0};
static ushort index_to_rgb[16][3]={
{65535,65535,65535},
{0,0,0},
{65535,0,65535},
{2,2,2},
{65535,65535,0},
{2,2,2},
{65535,0,0},
{2,2,2},
{0,65535,65535},
{2,2,2},
{0,0,65535},
{2,2,2},
{0,65535,0},
{2,2,2},
{2,2,2},
{2,2,2}
};
private gx_color_index
tekink_map_rgb_color(gx_device *dev, const gx_color_value cv[])
{
gx_color_value r = cv[0];
gx_color_value g = cv[1];
gx_color_value b = cv[2];
return(rgb_to_index[(((b>32767) << 2) + ((g>32767) << 1) +
(r>32767)) & 7]);
}
private int
tekink_map_color_rgb(gx_device *dev, gx_color_index color, ushort prgb[3])
{
register ushort c = (ushort)color;
register int i;
if (c>15) return -1;
if (index_to_rgb[c][0]==2) return -1;
for (i=0;i<3;i++){
prgb[i]=index_to_rgb[c][i];
}
return 0;
}
private int
tekink_print_page(gx_device_printer *pdev,FILE *prn_stream)
{
int line_size,color_line_size,scan_line,num_bytes,scan_lines,color_plane;
int roll_paper,out_line,micro_line,pending_micro_lines,line_blank,
blank_lines;
byte *outdata,*indata1,*bdata1,*mdata1,*ydata1,*cdata1;
register byte *indata,*bdatap,*mdatap,*ydatap,*cdatap;
register byte bdata,mdata,ydata,cdata;
register byte mask,inbyte;
register byte *indataend,*outdataend;
line_size = gdev_mem_bytes_per_scan_line((gx_device *)pdev);
color_line_size=(pdev->width+7)/8;
indata1=(byte *)malloc(line_size+4*(color_line_size+1));
if (indata1==NULL) return -1;
indataend=indata1+line_size;
bdata1=indataend;
mdata1=bdata1+(color_line_size+1);
ydata1=mdata1+(color_line_size+1);
cdata1=ydata1+(color_line_size+1);
roll_paper=!strcmp(pdev->dname,"tek4696");
out_line=0;
blank_lines=0;
scan_lines=pdev->height;
for (scan_line=0;scan_line<scan_lines;scan_line++){
gdev_prn_copy_scan_lines(pdev,scan_line,indata1,line_size);
bdatap = bdata1+1;
mdatap = mdata1+1;
ydatap = ydata1+1;
cdatap = cdata1+1;
bdata=0;
mdata=0;
cdata=0;
ydata=0;
mask=0x80;
memset(indataend,0,4*(color_line_size+1));
for (indata=indata1;indata<indataend;indata++){
inbyte = *indata;
if (inbyte&0x01) bdata|=mask;
if (inbyte&0x02) mdata|=mask;
if (inbyte&0x04) ydata|=mask;
if (inbyte&0x08) cdata|=mask;
mask>>=1;
if (!mask){
*(bdatap++) = bdata;
*(mdatap++) = mdata;
*(cdatap++) = cdata;
*(ydatap++) = ydata;
bdata=0;
mdata=0;
cdata=0;
ydata=0;
mask=0x80;
}
}
if (mask!=0x80){
*bdatap = bdata;
*mdatap = mdata;
*cdatap = cdata;
*ydatap = ydata;
}
line_blank=1;
for (color_plane=0;color_plane<4;color_plane++){
outdata=indataend+(color_plane*(color_line_size+1));
outdataend=outdata+color_line_size;
*outdata=0xff;
while (!(*outdataend)) outdataend--;
if (num_bytes=(outdataend-outdata)){
line_blank=0;
if (blank_lines){
pending_micro_lines=((out_line+blank_lines+1)/4)-
(out_line/4);
for (micro_line=0;micro_line<pending_micro_lines;
micro_line++){
fputs("\033A",prn_stream);
}
out_line+=blank_lines;
blank_lines=0;
}
fprintf(prn_stream,"\033I%c%03d",'0'+(out_line%4)+
4*color_plane,num_bytes);
fwrite(outdata+1,1,num_bytes,prn_stream);
}
}
if (line_blank&&roll_paper){
if (out_line) blank_lines++;
}
else{
if (out_line%4==3){
fputs("\033A",prn_stream);
}
out_line++;
}
}
if (out_line%4){
fputs("\033A",prn_stream);
}
if (roll_paper){
fputs("\n\n\n\n\n",prn_stream);
}
else{
fputs("\f",prn_stream);
}
free(indata1);
return 0;
}