#!/bin/bash
set -euo pipefail
clean=false
while getopts "c" opt; do
  case "$opt" in
    c) clean=true ;;
    *) echo "Usage: $0 [-c]"; exit 1 ;;
  esac
done
checkout_repo() {
    local repo_url="$1"
    local target_dir="$2"
    local branch="$3"
    if [ -d "$target_dir" ]; then
        echo "Directory $target_dir already exists. Checking out branch $branch..."
        cd "$target_dir"
        git checkout "$branch"
        cd ..
    else
        echo "Cloning repository $repo_url into $target_dir..."
        git clone "$repo_url" "$target_dir"
        cd "$target_dir"
        git checkout "$branch"
        cd ..
    fi
}
download() {
    echo "==== Download third party code from $1 at branch $2 to $3"
    if $clean; then
        rm -rf "$3"
    fi
    checkout_repo "$1" "$3" "$2"
}
download https://github.com/libscran/umappp.git e6a0ad380594e8b67c3b5f1ba4b1e201c1cf33a7 umappp
download https://github.com/knncolle/knncolle.git 513dc09b8a681274da83df97c3d5b353a2b61c4b knncolle
download https://github.com/LTLA/CppKmeans.git v3.1.2 CppKmeans
download https://github.com/LTLA/subpar.git v0.3.2 subpar
download https://github.com/LTLA/aarand.git v1.0.2 aarand
download https://github.com/LTLA/CppIrlba.git v2.0.2 CppIrlba
download https://gitlab.com/libeigen/eigen.git 3.4.0 Eigen
download https://github.com/knncolle/knncolle_hnsw.git 024b9dda025079c8c988a79f8b69e1f54b94b507 knncolle_hnsw
download https://github.com/nmslib/hnswlib.git v0.8.0 hnswlib
download https://github.com/brj0/nndescent.git 514275f263be010712530a95e56ffc9b81b9110b nndescent
pushd nndescent
git apply ../nndescent.patch
popd