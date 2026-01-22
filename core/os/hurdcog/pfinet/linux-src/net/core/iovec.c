#include <linux/errno.h>
#include <linux/sched.h>
#include <linux/kernel.h>
#include <linux/mm.h>
#include <linux/malloc.h>
#include <linux/net.h>
#include <linux/in6.h>
#include <asm/uaccess.h>
#include <asm/byteorder.h>
#include <net/checksum.h>
int verify_iovec(struct msghdr *m, struct iovec *iov, char *address, int mode)
{
int size, err, ct;
if(m->msg_namelen)
{
if(mode==VERIFY_READ)
{
err=move_addr_to_kernel(m->msg_name, m->msg_namelen, address);
if(err<0)
goto out;
}
m->msg_name = address;
} else
m->msg_name = NULL;
err = -EFAULT;
size = m->msg_iovlen * sizeof(struct iovec);
if (copy_from_user(iov, m->msg_iov, size))
goto out;
m->msg_iov=iov;
for (err = 0, ct = 0; ct < m->msg_iovlen; ct++) {
err += iov[ct].iov_len;
if (err < 0)
return -EMSGSIZE;
}
out:
return err;
}
int memcpy_toiovec(struct iovec *iov, unsigned char *kdata, int len)
{
int err = -EFAULT;
while(len>0)
{
if(iov->iov_len)
{
int copy = min(iov->iov_len, len);
if (copy_to_user(iov->iov_base, kdata, copy))
goto out;
kdata+=copy;
len-=copy;
iov->iov_len-=copy;
iov->iov_base+=copy;
}
iov++;
}
err = 0;
out:
return err;
}
void memcpy_tokerneliovec(struct iovec *iov, unsigned char *kdata, int len)
{
while(len>0)
{
if(iov->iov_len)
{
int copy = min(iov->iov_len, len);
memcpy(iov->iov_base, kdata, copy);
kdata+=copy;
len-=copy;
iov->iov_len-=copy;
iov->iov_base+=copy;
}
iov++;
}
}
int memcpy_fromiovec(unsigned char *kdata, struct iovec *iov, int len)
{
int err = -EFAULT;
while(len>0)
{
if(iov->iov_len)
{
int copy = min(len, iov->iov_len);
if (copy_from_user(kdata, iov->iov_base, copy))
goto out;
len-=copy;
kdata+=copy;
iov->iov_base+=copy;
iov->iov_len-=copy;
}
iov++;
}
err = 0;
out:
return err;
}
int memcpy_fromiovecend(unsigned char *kdata, struct iovec *iov, int offset,
int len)
{
int err = -EFAULT;
while(offset >= iov->iov_len)
{
offset -= iov->iov_len;
iov++;
}
while (len > 0)
{
u8 *base = iov->iov_base + offset;
int copy = min(len, iov->iov_len - offset);
offset = 0;
if (copy_from_user(kdata, base, copy))
goto out;
len -= copy;
kdata += copy;
iov++;
}
err = 0;
out:
return err;
}
int csum_partial_copy_fromiovecend(unsigned char *kdata, struct iovec *iov,
int offset, unsigned int len, int *csump)
{
int csum = *csump;
int partial_cnt = 0, err = 0;
while (offset >= iov->iov_len)
{
offset -= iov->iov_len;
iov++;
}
while (len > 0)
{
u8 *base = iov->iov_base + offset;
unsigned int copy = min(len, iov->iov_len - offset);
offset = 0;
if (partial_cnt)
{
int par_len = 4 - partial_cnt;
if (par_len > copy) {
if (copy_from_user(kdata, base, copy))
goto out_fault;
kdata += copy;
base += copy;
partial_cnt += copy;
len -= copy;
iov++;
if (len)
continue;
*csump = csum_partial(kdata - partial_cnt,
partial_cnt, csum);
goto out;
}
if (copy_from_user(kdata, base, par_len))
goto out_fault;
csum = csum_partial(kdata - partial_cnt, 4, csum);
kdata += par_len;
base += par_len;
copy -= par_len;
len -= par_len;
partial_cnt = 0;
}
if (len > copy)
{
partial_cnt = copy % 4;
if (partial_cnt)
{
copy -= partial_cnt;
if (copy_from_user(kdata + copy, base + copy,
partial_cnt))
goto out_fault;
}
}
if (copy) {
csum = csum_and_copy_from_user(base, kdata, copy,
csum, &err);
if (err)
goto out;
}
len -= copy + partial_cnt;
kdata += copy + partial_cnt;
iov++;
}
*csump = csum;
out:
return err;
out_fault:
err = -EFAULT;
goto out;
}