#! /bin/bash
if [[ `tty` == "not a tty" ]]
then
	script -c $0 /dev/null
	exit 0
fi
byobu new-session -d -n 'cntl' \
	'echo -e "\nControl shell; you might want to run 'top' here.\n"; $SHELL'
byobu new-window -n 'cogsrv' 'nice guile -l x-test.scm; $SHELL'
sleep 3;
tmux new-window -n 'telnet' 'rlwrap telnet localhost 19905; $SHELL'
tmux new-window -n 'spare' 'echo -e "\nSpare-use shell.\n"; $SHELL'
echo "tmux_left=\"session\"" > $HOME/.byobu/status
echo "tmux_right=\"load_average disk_io date time\"" >> $HOME/.byobu/status
tmux attach