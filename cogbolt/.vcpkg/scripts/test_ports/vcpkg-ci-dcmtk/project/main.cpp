#include <dcmtk/dcmdata/dcjson.h>
#include <dcmtk/dcmtls/tlslayer.h>
int main()
{
auto djfp = DcmJsonFormatPretty(OFTrue);
DcmTLSTransportLayer::initializeOpenSSL();
return 0;
}