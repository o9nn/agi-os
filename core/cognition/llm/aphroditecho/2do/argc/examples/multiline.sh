cmd() { :; }
eval "$(TERM_WIDTH=${TERM_WIDTH:-`tput cols`} argc --argc-eval "$0" "$@")"