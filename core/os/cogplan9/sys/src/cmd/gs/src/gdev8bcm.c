#include "gx.h"
#include "gxdevice.h"
#include "gdev8bcm.h"
void
gx_8bit_map_init(gx_8bit_color_map * pcm, int max_count)
{
int i;
pcm->count = 0;
pcm->max_count = max_count;
for (i = 0; i < gx_8bit_map_size; i++)
pcm->map[i].rgb = gx_8bit_no_rgb;
}
int
gx_8bit_map_rgb_color(const gx_8bit_color_map * pcm, gx_color_value r,
gx_color_value g, gx_color_value b)
{
ushort rgb = gx_8bit_rgb_key(r, g, b);
const gx_8bit_map_entry *pme =
&pcm->map[(rgb * gx_8bit_map_spreader) % gx_8bit_map_size];
for (;; pme++) {
if (pme->rgb == rgb)
return pme->index;
else if (pme->rgb == gx_8bit_no_rgb)
break;
}
if (pme != &pcm->map[gx_8bit_map_size])
return pme - &pcm->map[gx_8bit_map_size];
pme = &pcm->map[0];
for (;; pme++) {
if (pme->rgb == rgb)
return pme->index;
else if (pme->rgb == gx_8bit_no_rgb)
return pme - &pcm->map[gx_8bit_map_size];
}
}
int
gx_8bit_add_rgb_color(gx_8bit_color_map * pcm, gx_color_value r,
gx_color_value g, gx_color_value b)
{
int index;
gx_8bit_map_entry *pme;
if (gx_8bit_map_is_full(pcm))
return -1;
index = gx_8bit_map_rgb_color(pcm, r, g, b);
if (index >= 0)
return index;
pme = &pcm->map[-index];
pme->rgb = gx_8bit_rgb_key(r, g, b);
return (pme->index = pcm->count++);
}