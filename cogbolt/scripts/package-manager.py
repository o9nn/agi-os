import sys
import os
import subprocess
import json
import argparse
import platform
from pathlib import Path
from typing import Dict, List, Optional, Tuple
class PackageManager:
    def __init__(self, project_root: Path):
        self.project_root = project_root
    def is_available(self) -> bool:
        raise NotImplementedError
    def install_dependencies(self) -> bool:
        raise NotImplementedError
    def get_name(self) -> str:
        raise NotImplementedError
class VcpkgManager(PackageManager):
    def __init__(self, project_root: Path):
        super().__init__(project_root)
        self.vcpkg_root = os.environ.get('VCPKG_ROOT', '/usr/local/share/vcpkg')
        self.vcpkg_exe = Path(self.vcpkg_root) / 'vcpkg'
        self.manifest_file = project_root / 'vcpkg.json'
    def is_available(self) -> bool:
        return self.vcpkg_exe.exists() and self.manifest_file.exists()
    def install_dependencies(self) -> bool:
        if not self.is_available():
            return False
        try:
            result = subprocess.run([str(self.vcpkg_exe), 'install', '--triplet', self._get_triplet(), '--x-manifest-root', str(self.project_root)], check=True, capture_output=True, text=True)
            print(f'vcpkg install output: {result.stdout}')
            return True
        except subprocess.CalledProcessError as e:
            print(f'vcpkg install failed: {e.stderr}')
            return False
    def _get_triplet(self) -> str:
        system = platform.system().lower()
        machine = platform.machine().lower()
        if system == 'linux':
            if machine in ['x86_64', 'amd64']:
                return 'x64-linux'
            elif machine in ['aarch64', 'arm64']:
                return 'arm64-linux'
        elif system == 'darwin':
            if machine in ['x86_64', 'amd64']:
                return 'x64-osx'
            elif machine in ['arm64']:
                return 'arm64-osx'
        elif system == 'windows':
            return 'x64-windows'
        return 'x64-linux'
    def get_name(self) -> str:
        return 'vcpkg'
class ConanManager(PackageManager):
    def __init__(self, project_root: Path):
        super().__init__(project_root)
        self.conanfile = project_root / 'conanfile.txt'
    def is_available(self) -> bool:
        try:
            subprocess.run(['conan', '--version'], check=True, capture_output=True, text=True)
            return self.conanfile.exists()
        except (subprocess.CalledProcessError, FileNotFoundError):
            return False
    def install_dependencies(self) -> bool:
        if not self.is_available():
            return False
        try:
            build_dir = self.project_root / 'build'
            build_dir.mkdir(exist_ok=True)
            result = subprocess.run(['conan', 'install', str(self.project_root), '--build=missing', f'--install-folder={build_dir}', '-s', 'build_type=Release'], check=True, capture_output=True, text=True, cwd=build_dir)
            print(f'Conan install output: {result.stdout}')
            return True
        except subprocess.CalledProcessError as e:
            print(f'Conan install failed: {e.stderr}')
            return False
    def get_name(self) -> str:
        return 'conan'
class SystemManager(PackageManager):
    def __init__(self, project_root: Path):
        super().__init__(project_root)
        self.system_type = self._detect_system()
    def _detect_system(self) -> str:
        if self._command_exists('apt-get'):
            return 'apt'
        elif self._command_exists('yum'):
            return 'yum'
        elif self._command_exists('pacman'):
            return 'pacman'
        elif self._command_exists('brew'):
            return 'brew'
        else:
            return 'unknown'
    def _command_exists(self, command: str) -> bool:
        try:
            subprocess.run(['which', command], check=True, capture_output=True, text=True)
            return True
        except subprocess.CalledProcessError:
            return False
    def is_available(self) -> bool:
        return self.system_type != 'unknown'
    def install_dependencies(self) -> bool:
        if not self.is_available():
            return False
        packages = self._get_required_packages()
        if not packages:
            return True
        try:
            if self.system_type == 'apt':
                subprocess.run(['sudo', 'apt-get', 'update'], check=True)
                subprocess.run(['sudo', 'apt-get', 'install', '-y'] + packages, check=True)
            elif self.system_type == 'yum':
                subprocess.run(['sudo', 'yum', 'install', '-y'] + packages, check=True)
            elif self.system_type == 'pacman':
                subprocess.run(['sudo', 'pacman', '-S', '--needed'] + packages, check=True)
            elif self.system_type == 'brew':
                subprocess.run(['brew', 'install'] + packages, check=True)
            return True
        except subprocess.CalledProcessError as e:
            print(f'System package install failed: {e}')
            return False
    def _get_required_packages(self) -> List[str]:
        if self.system_type == 'apt':
            return ['build-essential', 'cmake', 'ninja-build', 'pkg-config', 'libx11-dev', 'libxrandr-dev', 'libxinerama-dev', 'libxcursor-dev', 'libxi-dev', 'libgl1-mesa-dev', 'libglu1-mesa-dev', 'xorg-dev', 'git', 'curl', 'wget']
        elif self.system_type == 'yum':
            return ['gcc-c++', 'cmake', 'ninja-build', 'pkgconfig', 'libX11-devel', 'libXrandr-devel', 'libXinerama-devel', 'libXcursor-devel', 'libXi-devel', 'mesa-libGL-devel', 'mesa-libGLU-devel', 'git', 'curl', 'wget']
        elif self.system_type == 'pacman':
            return ['base-devel', 'cmake', 'ninja', 'pkgconf', 'libx11', 'libxrandr', 'libxinerama', 'libxcursor', 'libxi', 'mesa', 'glu', 'git', 'curl', 'wget']
        elif self.system_type == 'brew':
            return ['cmake', 'ninja', 'pkg-config', 'git', 'curl', 'wget']
        return []
    def get_name(self) -> str:
        return f'system-{self.system_type}'
class PackageManagerIntegration:
    def __init__(self, project_root: Path):
        self.project_root = project_root
        self.managers = [VcpkgManager(project_root), ConanManager(project_root), SystemManager(project_root)]
    def detect_available_managers(self) -> List[PackageManager]:
        return [manager for manager in self.managers if manager.is_available()]
    def install_all_dependencies(self, preferred_manager: Optional[str]=None) -> bool:
        available = self.detect_available_managers()
        if not available:
            print('No package managers available')
            return False
        if preferred_manager:
            for manager in available:
                if manager.get_name() == preferred_manager:
                    return self._install_with_manager(manager)
        for manager in available:
            if manager.get_name().startswith('vcpkg'):
                if self._install_with_manager(manager):
                    return True
        for manager in available:
            if manager.get_name().startswith('conan'):
                if self._install_with_manager(manager):
                    return True
        for manager in available:
            if manager.get_name().startswith('system'):
                return self._install_with_manager(manager)
        return False
    def _install_with_manager(self, manager: PackageManager) -> bool:
        print(f'Installing dependencies with {manager.get_name()}...')
        return manager.install_dependencies()
    def generate_status_report(self) -> Dict:
        available = self.detect_available_managers()
        return {'project_root': str(self.project_root), 'available_managers': [m.get_name() for m in available], 'platform': {'system': platform.system(), 'machine': platform.machine(), 'python_version': platform.python_version()}, 'files': {'vcpkg_json': (self.project_root / 'vcpkg.json').exists(), 'conanfile_txt': (self.project_root / 'conanfile.txt').exists(), 'cmake_presets': (self.project_root / 'CMakePresets.json').exists()}}
def main():
    parser = argparse.ArgumentParser(description='Bolt C++ Package Manager Integration')
    parser.add_argument('--install', action='store_true', help='Install project dependencies')
    parser.add_argument('--status', action='store_true', help='Show package manager status')
    parser.add_argument('--manager', choices=['vcpkg', 'conan', 'system'], help='Preferred package manager')
    parser.add_argument('--project-root', type=Path, default='.', help='Project root directory (default: current directory)')
    args = parser.parse_args()
    project_root = Path(args.project_root).resolve()
    if not project_root.exists():
        print(f'Project root does not exist: {project_root}')
        sys.exit(1)
    integration = PackageManagerIntegration(project_root)
    if args.status:
        status = integration.generate_status_report()
        print(json.dumps(status, indent=2))
    if args.install:
        success = integration.install_all_dependencies(args.manager)
        if success:
            print('Dependencies installed successfully!')
            sys.exit(0)
        else:
            print('Failed to install dependencies')
            sys.exit(1)
    if not args.status and (not args.install):
        parser.print_help()
if __name__ == '__main__':
    main()