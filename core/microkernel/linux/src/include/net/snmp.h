#ifndef _SNMP_H
#define _SNMP_H
struct ip_mib
{
unsigned long	IpForwarding;
unsigned long	IpDefaultTTL;
unsigned long	IpInReceives;
unsigned long	IpInHdrErrors;
unsigned long	IpInAddrErrors;
unsigned long	IpForwDatagrams;
unsigned long	IpInUnknownProtos;
unsigned long	IpInDiscards;
unsigned long	IpInDelivers;
unsigned long	IpOutRequests;
unsigned long	IpOutDiscards;
unsigned long	IpOutNoRoutes;
unsigned long	IpReasmTimeout;
unsigned long	IpReasmReqds;
unsigned long	IpReasmOKs;
unsigned long	IpReasmFails;
unsigned long	IpFragOKs;
unsigned long	IpFragFails;
unsigned long	IpFragCreates;
};
struct icmp_mib
{
unsigned long	IcmpInMsgs;
unsigned long	IcmpInErrors;
unsigned long	IcmpInDestUnreachs;
unsigned long	IcmpInTimeExcds;
unsigned long	IcmpInParmProbs;
unsigned long	IcmpInSrcQuenchs;
unsigned long	IcmpInRedirects;
unsigned long	IcmpInEchos;
unsigned long	IcmpInEchoReps;
unsigned long	IcmpInTimestamps;
unsigned long	IcmpInTimestampReps;
unsigned long	IcmpInAddrMasks;
unsigned long	IcmpInAddrMaskReps;
unsigned long	IcmpOutMsgs;
unsigned long	IcmpOutErrors;
unsigned long	IcmpOutDestUnreachs;
unsigned long	IcmpOutTimeExcds;
unsigned long	IcmpOutParmProbs;
unsigned long	IcmpOutSrcQuenchs;
unsigned long	IcmpOutRedirects;
unsigned long	IcmpOutEchos;
unsigned long	IcmpOutEchoReps;
unsigned long	IcmpOutTimestamps;
unsigned long	IcmpOutTimestampReps;
unsigned long	IcmpOutAddrMasks;
unsigned long	IcmpOutAddrMaskReps;
};
struct tcp_mib
{
unsigned long	TcpRtoAlgorithm;
unsigned long	TcpRtoMin;
unsigned long	TcpRtoMax;
unsigned long	TcpMaxConn;
unsigned long	TcpActiveOpens;
unsigned long	TcpPassiveOpens;
unsigned long	TcpAttemptFails;
unsigned long	TcpEstabResets;
unsigned long	TcpCurrEstab;
unsigned long	TcpInSegs;
unsigned long	TcpOutSegs;
unsigned long	TcpRetransSegs;
};
struct udp_mib
{
unsigned long	UdpInDatagrams;
unsigned long	UdpNoPorts;
unsigned long	UdpInErrors;
unsigned long	UdpOutDatagrams;
};
#endif