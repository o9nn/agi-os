#! /bin/bash
lxc-ls --active | xargs -r -n1 lxc-stop -n