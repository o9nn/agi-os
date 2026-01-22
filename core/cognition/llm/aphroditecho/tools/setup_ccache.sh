#!/bin/bash
cd "$(dirname "$0")/.."
echo "Setting up ccache for Aphrodite Engine development..."
bin/micromamba run -r conda -n aphrodite-runtime ccache --set-config=max_size=10G
bin/micromamba run -r conda -n aphrodite-runtime ccache --set-config=compression=true
bin/micromamba run -r conda -n aphrodite-runtime ccache --set-config=compression_level=6
bin/micromamba run -r conda -n aphrodite-runtime ccache --set-config=sloppiness=file_macro,locale,time_macros
echo "Current ccache configuration:"
bin/micromamba run -r conda -n aphrodite-runtime ccache --show-config | grep -E "(cache_dir|max_size|compression)"
echo ""
echo "Current ccache statistics:"
bin/micromamba run -r conda -n aphrodite-runtime ccache --show-stats
echo ""
echo "ccache setup complete!"
echo ""
echo "To build with ccache enabled, simply run:"
echo "  ./runtime.sh pip install -ve . --force-reinstall"
echo ""
echo "To check ccache performance:"
echo "  ./runtime.sh ccache --show-stats"
echo ""
echo "To clear ccache:"
echo "  ./runtime.sh ccache --clear"