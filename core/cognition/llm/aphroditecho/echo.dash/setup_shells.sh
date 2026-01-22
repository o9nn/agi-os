#!/bin/bash
set -e
echo "Setting up Deep Tree Echo environment on Shells.com..."
sudo tee /etc/security/limits.d/deepecho.conf << EOF
deepecho soft nproc 2048
deepecho hard nproc 4096
deepecho soft nofile 8192
deepecho hard nofile 16384
EOF
sudo apt-get update
sudo apt-get install -y \
python3.10 \
python3-pip \
python3-venv \
firefox \
git \
vim \
nodejs \
npm \
xvfb \
x11vnc \
curl \
wget \
htop \
tmux \
screen \
gnome-keyring \
libsecret-1-0 \
libsecret-1-dev \
dbus-x11
echo "alias python=python3" >> ~/.bashrc
echo "alias pip=pip3" >> ~/.bashrc
source ~/.bashrc
if [ -z "$DBUS_SESSION_BUS_ADDRESS" ]; then
eval $(dbus-launch --sh-syntax)
fi
echo "D33ptr333ch0" | gnome-keyring-daemon --unlock
export $(gnome-keyring-daemon --start)
sudo fallocate -l 4G /swapfile
sudo chmod 600 /swapfile
sudo mkswap /swapfile
sudo swapon /swapfile
echo '/swapfile none swap sw 0 0' | sudo tee -a /etc/fstab
if ! id -u deepecho &>/dev/null; then
sudo useradd -m -s /bin/bash deepecho
sudo usermod -aG sudo deepecho
echo "deepecho ALL=(ALL) NOPASSWD:ALL" | sudo tee /etc/sudoers.d/deepecho
fi
sudo mkdir -p /opt/deepecho
sudo chown -R deepecho:deepecho /opt/deepecho
sudo -u deepecho python3 -m venv /opt/deepecho/venv
source /opt/deepecho/venv/bin/activate
pip install --upgrade pip
pip install -r requirements.txt
pip install keyring secretstorage dbus-python
if [ ! -d "/opt/deepecho/windsurf-project" ]; then
cd /opt/deepecho
sudo -u deepecho git clone https://github.com/EchoCog/windsurf-project.git
fi
sudo chown -R deepecho:deepecho /opt/deepecho/windsurf-project
cat > /opt/deepecho/.screenrc << EOF
startup_message off
defscrollback 10000
hardstatus alwayslastline
hardstatus string '%{= kG}[ %{G}%H %{g}][%= %{= kw}%?%-Lw%?%{r}(%{W}%n*%f%t%?(%u)%?%{r})%{w}%?%+Lw%?%?%= %{g}][%{B} %m-%d %{W}%c %{g}]'
screen -t monitor 0 python3 monitor.py
screen -t logs 1 tail -f /var/log/syslog
screen -t htop 2 htop
EOF
sudo chown deepecho:deepecho /opt/deepecho/.screenrc
echo 'export $(gnome-keyring-daemon --start)' >> ~/.bashrc
echo 'export SSH_AUTH_SOCK' >> ~/.bashrc
echo "Deep Tree Echo environment setup complete!"
echo ""
echo "Keyring is initialized with password: D33ptr333ch0"
echo ""
echo "To start monitoring, run:"
echo "screen -S monitor python3 monitor.py"
echo ""
echo "Or use the pre-configured screen session:"
echo "screen -c /opt/deepecho/.screenrc"