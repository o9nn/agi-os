#ifndef __LINUX_FILE_H
#define __LINUX_FILE_H
extern void __fput(struct file *);
extern inline struct file * fcheck_task(struct task_struct *p, unsigned int fd)
{
struct file * file = NULL;
if (p->files && fd < p->files->max_fds)
file = p->files->fd[fd];
return file;
}
extern inline struct file * fcheck(unsigned int fd)
{
struct file * file = NULL;
if (fd < current->files->max_fds)
file = current->files->fd[fd];
return file;
}
extern inline struct file * fget(unsigned int fd)
{
struct file * file = fcheck(fd);
if (file)
file->f_count++;
return file;
}
extern inline void fd_install(unsigned int fd, struct file *file)
{
current->files->fd[fd] = file;
}
extern void fput(struct file *file);
extern void put_filp(struct file *file);
#endif