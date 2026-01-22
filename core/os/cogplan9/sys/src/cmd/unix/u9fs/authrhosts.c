#include <plan9.h>
#include <fcall.h>
#include <u9fs.h>
static char*
rhostsauth(Fcall *rx, Fcall *tx)
{
USED(rx);
USED(tx);
return "u9fs rhostsauth: no authentication required";
}
static char*
rhostsattach(Fcall *rx, Fcall *tx)
{
USED(tx);
if(ruserok(remotehostname, 0, rx->uname, rx->uname) < 0){
fprint(2, "ruserok(%s, %s) not okay\n", remotehostname, rx->uname);
return "u9fs: rhosts authentication failed";
}
return 0;
}
Auth authrhosts = {
"rhosts",
rhostsauth,
rhostsattach,
};