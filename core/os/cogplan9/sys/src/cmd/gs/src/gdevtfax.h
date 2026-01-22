#ifndef gdevtfax_INCLUDED
#  define gdevtfax_INCLUDED
int gdev_fax_print_page_stripped(gx_device_printer *pdev, FILE *prn_stream,
stream_CFE_state *ss, long rows_per_strip);
#endif