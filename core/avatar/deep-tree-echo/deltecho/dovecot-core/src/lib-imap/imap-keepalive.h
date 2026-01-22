#ifndef IMAP_KEEPALIVE_H
#define IMAP_KEEPALIVE_H
unsigned int
imap_keepalive_interval_msecs(const char *username, const struct ip_addr *ip,
unsigned int interval_secs);
#endif