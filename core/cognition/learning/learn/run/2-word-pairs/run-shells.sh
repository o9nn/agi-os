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
	source $MASTER_CONFIG_FILE
else
	echo "Cannot find master configuration file!"
	exit -1
fi
if [ -r ${PAIR_CONF_FILE} ]; then
	source ${PAIR_CONF_FILE}
else
	echo "Cannot find pair-counting configuration file!"
	exit -1
fi
byobu new-session -d -s 'pair-count' -n 'cntl' \
	'echo -e "\nControl shell; you might want to run 'top' here.\n"; $SHELL'
byobu new-window -n 'cogsrv' 'nice guile -l ${COMMON_DIR}/cogserver-pair.scm ; $SHELL'
while ! nc -z $HOSTNAME $PORT ; do
	echo "Wating for cogserver at $HOSTNAME $PORT ..."
	sleep 2
done
echo "Found CogServer at $HOSTNAME $PORT"
echo -e "(wait-gate startup-gate)\n.\n." | nc $HOSTNAME $PORT >> /dev/null
tmux new-window -n 'telnet' 'rlwrap telnet $HOSTNAME $PORT; $SHELL'
tmux new-window -n 'submit' \
	'echo -e "\nYou might want to run ./pair-submit.sh here.\n"; $SHELL'
tmux new-window -n 'spare' 'echo -e "\nSpare-use shell.\n"; $SHELL'
echo "tmux_left=\"session\"" > $HOME/.byobu/status
echo "tmux_right=\"load_average disk_io date time\"" >> $HOME/.byobu/status
tmux attach