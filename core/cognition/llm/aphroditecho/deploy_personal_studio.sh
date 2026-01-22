#!/bin/bash
set -e
echo "🏠 Personal Developer Studio - Aphrodite Engine"
echo "=============================================="
echo "Account Type: Personal Pro Developer"
echo "Cost Optimization: Enabled"
echo "Auto-shutdown: 30 minutes idle"
source .env.personal_studio
export LIGHTNING_ACCOUNT_TYPE="personal"
export LIGHTNING_STUDIO_TIER="developer_pro"
export MAX_JOBS=8
export CCACHE_MAXSIZE="10G"
echo "📦 Cloning repository..."
git clone https://github.com/EchoCog/aphroditecho.git /workspace/aphroditecho
cd /workspace/aphroditecho
echo "🔨 Building Aphrodite Engine (Personal Studio Optimized)..."
export APHRODITE_TARGET_DEVICE=cuda
pip install --timeout 3600 -e .
echo "📊 Setting up personal studio monitoring..."
cat > monitor_personal_studio.py << 'EOF'
import time
import psutil
import os
def monitor_usage():
    """Monitor resource usage for personal studio cost optimization"""
    while True:
        cpu_percent = psutil.cpu_percent(interval=1)
        memory = psutil.virtual_memory()
        if cpu_percent < 5 and memory.percent < 50:
            idle_time = getattr(monitor_usage, 'idle_time', 0) + 1
            monitor_usage.idle_time = idle_time
            if idle_time > 30:
                print("💰 Auto-shutdown triggered for cost optimization")
                os.system("lightning stop")
                break
        else:
            monitor_usage.idle_time = 0
        print(f"💻 CPU: {cpu_percent}%, Memory: {memory.percent}%")
        time.sleep(60)
if __name__ == "__main__":
    monitor_usage()
EOF
python monitor_personal_studio.py &
echo "✅ Personal Developer Studio setup complete!"
echo "💡 Instance will auto-shutdown after 30 minutes of inactivity"