#! /bin/bash
if [[ `tty` == "not a tty" ]]
then
	script -c $0 /dev/null
	exit 0
fi
export LD_LIBRARY_PATH=/usr/local/lib/opencog/modules
if [ $
then 
  echo "Usage: ./run-multiple-terminals.sh <mode> <language> <db_name> [<username>] [<password>]"
  exit 0
fi
source ./config/det-port-num.sh $1 $2
launcher=launch-cogserver.scm
byobu new-session -d -n 'cntl' '$SHELL'
case $
   3)
      byobu new-window -n 'cogsrv' "nice guile -l $launcher -- --mode $1 --lang $2 --db $3; $SHELL"
      ;;
   4)
      byobu new-window -n 'cogsrv' "nice guile -l $launcher -- --mode $1 --lang $2 --db $3 --user $4; $SHELL"
      ;;
   *)
      byobu new-window -n 'cogsrv' "nice guile -l $launcher -- --mode $1 --lang $2 --db $3 --user $4 --password $5; $SHELL"
      ;;
esac
sleep 2;
tmux new-window -n 'telnet' "rlwrap telnet localhost $PORT; $SHELL"
tmux new-window -n 'parse' '$SHELL'
tmux new-window -n 'spare' '$SHELL'
echo "tmux_left=\"session\"" > $HOME/.byobu/status
echo "tmux_right=\"load_average disk_io date time\"" >> $HOME/.byobu/status
tmux attach
echo "Started"