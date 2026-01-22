#ifndef _RAID0_H
#define _RAID0_H
struct strip_zone
{
int zone_offset;
int dev_offset;
int size;
int nb_dev;
struct real_dev *dev[MAX_REAL];
};
struct raid0_hash
{
struct strip_zone *zone0, *zone1;
};
struct raid0_data
{
struct raid0_hash *hash_table;
struct strip_zone *strip_zone;
int nr_strip_zones;
struct strip_zone *smallest;
int nr_zones;
};
#endif