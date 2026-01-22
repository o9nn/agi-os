#!/dis/sh.dis -n
load std
listen 'tcp!*!rstyx'  {runas $user auxi/rstyxd&}