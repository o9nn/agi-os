import os
import json
from datetime import datetime
from pathlib import Path
class PersonalStudioManager:
    def __init__(self):
        self.config_file = Path.home() / '.lightning_personal_config.json'
        self.config = self.load_config()
        self.is_personal_account = True
    def load_config(self):
        default_config = {'account_type': 'personal_pro', 'studio_tier': 'developer_pro', 'compute_limits': {'gpu_hours_monthly': 100, 'max_concurrent_instances': 3, 'storage_gb': 500}, 'preferred_compute': 'gpu-rtx', 'cost_optimization': True, 'auto_shutdown_minutes': 30}
        if self.config_file.exists():
            with open(self.config_file, 'r') as f:
                loaded_config = json.load(f)
                default_config.update(loaded_config)
                return default_config
        return default_config
    def save_config(self):
        with open(self.config_file, 'w') as f:
            json.dump(self.config, f, indent=2)
    def setup_personal_environment(self):
        env_vars = {'LIGHTNING_ACCOUNT_TYPE': 'personal', 'LIGHTNING_STUDIO_TIER': 'developer_pro', 'LIGHTNING_COST_OPTIMIZATION': 'true', 'LIGHTNING_AUTO_SHUTDOWN': '30', 'APHRODITE_TARGET_DEVICE': 'cuda', 'PERSONAL_STUDIO_MODE': 'true'}
        env_file = Path('.env.personal_studio')
        with open(env_file, 'w') as f:
            f.write('# Personal Developer Studio Configuration\n')
            f.write(f'# Generated: {datetime.now().isoformat()}\n\n')
            for key, value in env_vars.items():
                f.write(f'{key}={value}\n')
        print(f'✅ Created personal studio environment file: {env_file}')
        return env_file
    def create_personal_deployment_script(self):
        script_content = f'''#!/bin/bash\n# Personal Developer Studio Deployment Script\n# Optimized for individual developer pro subscription\n# Generated: {datetime.now().isoformat()}\n\nset -e\n\necho "🏠 Personal Developer Studio - Aphrodite Engine"\necho "=============================================="\necho "Account Type: Personal Pro Developer"\necho "Cost Optimization: Enabled"\necho "Auto-shutdown: 30 minutes idle"\n\n# Load personal studio environment\nsource .env.personal_studio\n\n# Personal studio optimized build\nexport LIGHTNING_ACCOUNT_TYPE="personal"\nexport LIGHTNING_STUDIO_TIER="developer_pro"\nexport MAX_JOBS=8  # Conservative for personal tier\nexport CCACHE_MAXSIZE="10G"  # Reduced for cost optimization\n\n# Clone and setup\necho "📦 Cloning repository..."\ngit clone https://github.com/EchoCog/aphroditecho.git /workspace/aphroditecho\ncd /workspace/aphroditecho\n\n# Personal studio build (cost-optimized)\necho "🔨 Building Aphrodite Engine (Personal Studio Optimized)..."\nexport APHRODITE_TARGET_DEVICE=cuda\npip install --timeout 3600 -e .\n\n# Setup personal studio monitoring\necho "📊 Setting up personal studio monitoring..."\ncat > monitor_personal_studio.py << 'EOF'\n#!/usr/bin/env python3\nimport time\nimport psutil\nimport os\n\ndef monitor_usage():\n    """Monitor resource usage for personal studio cost optimization"""\n    while True:\n        cpu_percent = psutil.cpu_percent(interval=1)\n        memory = psutil.virtual_memory()\n        \n        # Auto-shutdown if idle for too long (cost optimization)\n        if cpu_percent < 5 and memory.percent < 50:\n            idle_time = getattr(monitor_usage, 'idle_time', 0) + 1\n            monitor_usage.idle_time = idle_time\n            \n            if idle_time > 30:  # 30 minutes idle\n                print("💰 Auto-shutdown triggered for cost optimization")\n                os.system("lightning stop")\n                break\n        else:\n            monitor_usage.idle_time = 0\n        \n        print(f"💻 CPU: {{cpu_percent}}%, Memory: {{memory.percent}}%")\n        time.sleep(60)\n\nif __name__ == "__main__":\n    monitor_usage()\nEOF\n\npython monitor_personal_studio.py &\n\necho "✅ Personal Developer Studio setup complete!"\necho "💡 Instance will auto-shutdown after 30 minutes of inactivity"\n'''
        script_file = Path('deploy_personal_studio.sh')
        with open(script_file, 'w') as f:
            f.write(script_content)
        os.chmod(script_file, 493)
        print('✅ Created personal studio deployment script: {script_file}')
        return script_file
    def create_cost_optimized_config(self):
        config_content = {'name': 'aphrodite-personal-studio', 'compute': {'type': 'gpu-rtx', 'disk_size': 50, 'auto_shutdown': 30}, 'environment': {'python_version': '3.11', 'requirements': ['torch>=2.0.0', 'transformers>=4.30.0']}, 'cost_optimization': {'enabled': True, 'max_runtime_hours': 4, 'alert_threshold_usd': 10}, 'personal_studio': {'account_type': 'developer_pro', 'tier': 'personal', 'budget_limit_monthly': 100}}
        config_file = Path('lightning_personal.yaml')
        import yaml
        try:
            with open(config_file, 'w') as f:
                yaml.dump(config_content, f, default_flow_style=False, indent=2)
        except ImportError:
            config_file = Path('lightning_personal.json')
            with open(config_file, 'w') as f:
                json.dump(config_content, f, indent=2)
        print(f'✅ Created cost-optimized config: {config_file}')
        return config_file
    def setup_personal_studio_complete(self):
        print('🏠 Setting up Personal Developer Studio Configuration...')
        print('=' * 60)
        env_file = self.setup_personal_environment()
        deploy_script = self.create_personal_deployment_script()
        config_file = self.create_cost_optimized_config()
        self.config['setup_date'] = datetime.now().isoformat()
        self.config['files_created'] = [str(env_file), str(deploy_script), str(config_file)]
        self.save_config()
        print('\n✅ Personal Developer Studio setup complete!')
        print('📁 Configuration saved to: {self.config_file}')
        print('\n📋 Next Steps:')
        print('1. Install Lightning CLI: pip install lightning')
        print('2. Login to your PERSONAL account: lightning login')
        print('3. Deploy: lightning run app ./deploy_personal_studio.sh')
        print('\n💰 Cost Optimization Features:')
        print('- Auto-shutdown after 30 minutes idle')
        print('- Conservative resource limits')
        print('- Budget monitoring and alerts')
        print('- gpu-rtx compute for cost efficiency')
        return {{'env_file': env_file, 'deploy_script': deploy_script, 'config_file': config_file}}
if __name__ == '__main__':
    manager = PersonalStudioManager()
    result = manager.setup_personal_studio_complete()
    print('\n🎉 Personal Developer Studio ready! Files: {list(result.values())}')