#! /bin/bash
if [[ `tty` == "not a tty" ]]
then
	script -c $0 /dev/null
	exit 0
fi
if [ -z $MASTER_CONFIG_FILE ]; then
	echo "MASTER_CONFIG_FILE not defined!"
	exit -1
fi
if [ -r $MASTER_CONFIG_FILE ]; then
	. $MASTER_CONFIG_FILE
else
	echo "Cannot find master configuration file!"
	exit -1
fi
byobu new-session -d -n 'cntl' \
	'echo -e "\nControl shell; you might want to run top here.\n"; $SHELL'
byobu new-window -n 'cogsrv' \
	'echo -e "\nGuile shell; e.g. run guile -l cogserver.scm here. \n"; $SHELL'
tmux new-window -n 'telnet' \
	'echo -e "\nTelnet shell; e.g. run rlwrap telnet $HOSTNAME $PORT\n"; $SHELL'
tmux new-window -n 'submit' \
	'echo -e "\nYou might want to run ./pair-submit.sh here.\n"; $SHELL'
tmux new-window -n 'code' 'echo -e "\nCoding shell.\n"; $SHELL'
tmux new-window -n 'spare' 'echo -e "\nSpare-use shell.\n"; $SHELL'
echo "tmux_left=\"session\"" > $HOME/.byobu/status
echo "tmux_right=\"load_average disk_io date time\"" >> $HOME/.byobu/status
tmux attach