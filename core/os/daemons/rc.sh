#!/bin/bash
PATH=/bin:/sbin
swapon -a
if [ -r /fastboot ]
then
	rm -f /fastboot
	echo Fast boot ... skipping disk checks
elif [ $1x = autobootx ]
then
	echo Automatic boot in progress...
	date
	/sbin/fsck --preen --writable
	case $? in
	0)
		;;
	1)
		;;
	2 | 3)
		/sbin/reboot
		;;
	4 | 5 | 8 | 9)
		echo "Automatic boot failed... help!"
		exit 1
		;;
	20 | 130 | 131)
		echo "Boot interrupted"
		exit 1
		;;
	12)
		echo "Boot interrupted (filesystem checks complete)"
		exit 1
		;;
	*)
		echo "Unknown error during fsck (exit status $?)"
		exit 1
		;;
	esac
fi
echo -n cleaning up left over files...
rm -f /etc/nologin
rm -f /var/lock/LCK.*
if test -d /tmp; then
  function remove_translators() {
    local f
    for f; do
      settrans -pagfS "$f"
      if [ -L "$f" ] || [ ! -d "$f" ]; then
	rm "$f"
      else
	remove_translators "$f"/* "$f"/.[!.] "$f"/.??*
	rmdir "$f"
      fi
    done
  }
  (cd /tmp
   shopt -s nullglob
   for f in * .[!.] .??*; do
     case "$f" in
     'lost+found'|'quotas') ;;
     *) remove_translators "./$f"
     esac
   done)
  unset -f remove_translators
fi
if test -d /var/run; then
  (cd /var/run && { rm -rf -- *; cp /dev/null utmp; chmod 644 utmp; })
fi
echo done
touch /var/run/mtab
chmod 664 /etc/motd
echo -n starting daemons:
/sbin/syslogd	&& echo -n ' syslogd'
/sbin/inetd	&& echo -n ' inetd'
if test -x /sbin/sendmail -a -r /etc/sendmail.cf; then
  /sbin/sendmail -bd -q30m	&& echo -n ' sendmail'
fi
echo .
date