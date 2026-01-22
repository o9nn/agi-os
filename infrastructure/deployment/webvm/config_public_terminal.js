export const diskImageUrl = "wss://disks.webvm.io/debian_large_20230522_5044875331_2.ext2";
export const diskImageType = "cloud";
export const printIntro = true;
export const needsDisplay = false;
export const cmd = "/bin/bash";
export const args = ["--login"];
export const opts = {
env: ["HOME=/home/user", "TERM=xterm", "USER=user", "SHELL=/bin/bash", "EDITOR=vim", "LANG=en_US.UTF-8", "LC_ALL=C"],
cwd: "/home/user",
uid: 1000,
gid: 1000
};