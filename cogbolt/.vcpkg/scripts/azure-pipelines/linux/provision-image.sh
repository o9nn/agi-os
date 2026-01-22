#!/bin/bash
export DEBIAN_FRONTEND=noninteractive
UBUNTU_VERSION_ID=$(. /etc/os-release && echo "$VERSION_ID")
NVIDIA_REPO_VERSION=$(echo "$UBUNTU_VERSION_ID" | sed 's/\.//')
apt-get -y update
apt-get --no-install-recommends -y install ca-certificates curl apt-transport-https lsb-release gnupg software-properties-common
curl -L -o /etc/apt/preferences.d/cuda-repository-pin-600 "https://developer.download.nvidia.com/compute/cuda/repos/ubuntu${NVIDIA_REPO_VERSION}/x86_64/cuda-ubuntu${NVIDIA_REPO_VERSION}.pin"
apt-key adv --fetch-keys "https://developer.download.nvidia.com/compute/cuda/repos/ubuntu${NVIDIA_REPO_VERSION}/x86_64/3bf863cc.pub"
add-apt-repository "deb https://developer.download.nvidia.com/compute/cuda/repos/ubuntu${NVIDIA_REPO_VERSION}/x86_64/ /"
curl -L -o packages-microsoft-prod.deb https://packages.microsoft.com/config/ubuntu/${UBUNTU_VERSION_ID}/packages-microsoft-prod.deb
dpkg -i packages-microsoft-prod.deb
rm -f packages-microsoft-prod.deb
add-apt-repository universe
mkdir -p /etc/apt/keyrings
curl -sLS https://packages.microsoft.com/keys/microsoft.asc |
gpg --dearmor |
tee /etc/apt/keyrings/microsoft.gpg > /dev/null
chmod go+r /etc/apt/keyrings/microsoft.gpg
AZ_DIST=$(lsb_release -cs)
echo "deb [arch=`dpkg --print-architecture` signed-by=/etc/apt/keyrings/microsoft.gpg] https://packages.microsoft.com/repos/azure-cli/ $AZ_DIST main" |
tee /etc/apt/sources.list.d/azure-cli.list
apt-get -y update
apt-get -y upgrade
APT_PACKAGES="git curl zip unzip tar"
APT_PACKAGES="$APT_PACKAGES \
autoconf autoconf-archive \
autopoint \
build-essential \
cmake \
gcc g++ gfortran \
libnuma1 libnuma-dev \
libtool libtool-bin libltdl-dev \
libudev-dev \
"
APT_PACKAGES="$APT_PACKAGES \
bison libbison-dev \
flex \
gperf \
nasm \
ninja-build \
pkg-config \
python3 \
ruby-full \
swig \
yasm \
"
APT_PACKAGES="$APT_PACKAGES \
mesa-common-dev libgl1-mesa-dev libglu1-mesa-dev libgles2-mesa-dev \
libx11-dev \
libxaw7-dev \
libxcursor-dev \
libxi-dev \
libxinerama-dev \
libxkbcommon-x11-dev \
libxrandr-dev \
libxt-dev \
libxxf86vm-dev \
xutils-dev \
"
APT_PACKAGES="$APT_PACKAGES libxext-dev libxfixes-dev libxrender-dev \
libxcb1-dev libx11-xcb-dev libxcb-glx0-dev libxcb-util0-dev"
APT_PACKAGES="$APT_PACKAGES libxkbcommon-dev libxcb-keysyms1-dev \
libxcb-image0-dev libxcb-shm0-dev libxcb-icccm4-dev libxcb-sync-dev \
libxcb-xfixes0-dev libxcb-shape0-dev libxcb-randr0-dev \
libxcb-render-util0-dev libxcb-xinerama0-dev libxcb-xkb-dev libxcb-xinput-dev"
APT_PACKAGES="$APT_PACKAGES libxcb-cursor-dev"
APT_PACKAGES="$APT_PACKAGES libkrb5-dev"
APT_PACKAGES="$APT_PACKAGES libxcb-res0-dev"
APT_PACKAGES="$APT_PACKAGES libxcb-keysyms1-dev libxcb-xkb-dev libxcb-record0-dev"
APT_PACKAGES="$APT_PACKAGES python3-setuptools python3-mako libxcb-dri3-dev libxcb-present-dev"
APT_PACKAGES="$APT_PACKAGES python3-pip python3-venv python3-jinja2"
APT_PACKAGES="$APT_PACKAGES nodejs"
APT_PACKAGES="$APT_PACKAGES libwayland-dev"
APT_PACKAGES="$APT_PACKAGES python-is-python3"
APT_PACKAGES="$APT_PACKAGES guile-2.2-dev"
APT_PACKAGES="$APT_PACKAGES libxdamage-dev libselinux1-dev"
APT_PACKAGES="$APT_PACKAGES libxtst-dev"
APT_PACKAGES="$APT_PACKAGES golang-go"
APT_PACKAGES="$APT_PACKAGES wayland-protocols"
APT_PACKAGES="$APT_PACKAGES libbluetooth-dev"
APT_PACKAGES="$APT_PACKAGES libtirpc-dev"
APT_PACKAGES="$APT_PACKAGES cuda-cccl-12-9 cuda-compat-12-9 cuda-compiler-12-9 cuda-crt-12-9 \
cuda-cudart-dev-12-9 cuda-cuobjdump-12-9 cuda-cupti-dev-12-9 cuda-cuxxfilt-12-9 \
cuda-driver-dev-12-9 cuda-libraries-dev-12-9 cuda-minimal-build-12-9 cuda-nvcc-12-9 \
cuda-nvml-dev-12-9 cuda-nvrtc-dev-12-9 cuda-nvtx-12-9 cuda-nvvm-12-9 cuda-opencl-dev-12-9 \
cuda-sanitizer-12-9 cuda-toolkit-12-9-config-common cudnn9-cuda-12-9 gds-tools-12-9 \
libcublas-12-9 libcudnn9-dev-cuda-12 libcufft-dev-12-9 libcurand-dev-12-9 libcusolver-dev-12-9 \
libcusparse-dev-12-9 libnccl-dev libnpp-dev-12-9 libnvfatbin-dev-12-9 libnvjitlink-dev-12-9 \
libnvjpeg-dev-12-9"
APT_PACKAGES="$APT_PACKAGES powershell azcopy azure-cli"
if [[ $(grep microsoft /proc/version) ]]; then
echo "Skipping install of ADO prerequisites on WSL."
else
APT_PACKAGES="$APT_PACKAGES libkrb5-3 zlib1g libicu70 debsums liblttng-ust1"
fi
apt-get --no-install-recommends -y install $APT_PACKAGES
az --version