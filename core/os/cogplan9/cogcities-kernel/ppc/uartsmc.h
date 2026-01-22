typedef struct UartData UartData;
struct UartData
{
int	smcno;
SMC	*smc;
Uartsmc	*usmc;
char	*rxbuf;
char	*txbuf;
BD*	rxb;
BD*	txb;
int	initialized;
int	enabled;
} uartdata[Nuart];
int baudgen(int);
void smcsetup(Uart *uart);