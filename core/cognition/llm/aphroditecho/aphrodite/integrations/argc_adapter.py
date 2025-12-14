"""
Argc Adapter - Integrates the argc Rust component with AAR system

This adapter provides structured CLI command schema parsing and converts
argc command definitions into function registry entries for unified tool access.
"""

import asyncio
import logging
import subprocess
import tempfile
from typing import Dict, List, Any, Optional
from dataclasses import dataclass, field
from pathlib import Path

from ..function_registry import (
    FunctionRegistry, FunctionSpec, ParameterSpec, SafetyClass
)

logger = logging.getLogger(__name__)


@dataclass
class ArgcCommand:
    """Represents a command parsed from argc."""
    name: str
    description: str
    alias: Optional[str] = None
    flags: List[Dict[str, Any]] = field(default_factory=list)
    options: List[Dict[str, Any]] = field(default_factory=list)
    args: List[Dict[str, Any]] = field(default_factory=list)
    subcommands: List['ArgcCommand'] = field(default_factory=list)
    script_path: str = ""
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class ArgcSchema:
    """Complete schema parsed from an argc script."""
    script_name: str
    description: str
    commands: List[ArgcCommand]
    global_flags: List[Dict[str, Any]] = field(default_factory=list)
    global_options: List[Dict[str, Any]] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)


class ArgcAdapter:
    """
    Adapter for integrating argc structured CLI commands with AAR system.
    
    Provides:
    - Parsing argc command definitions from shell scripts
    - Converting argc commands to function specifications
    - Command execution through argc runtime
    - Schema validation and normalization
    """

    def __init__(self, function_registry: FunctionRegistry):
        self.function_registry = function_registry
        self.argc_binary = self._find_argc_binary()
        self.schemas: Dict[str, ArgcSchema] = {}
        self.commands: Dict[str, ArgcCommand] = {}
        self.available = False
        
        if self.argc_binary:
            self._initialize_argc()
        else:
            self._setup_compatibility_mode()

    def _find_argc_binary(self) -> Optional[str]:
        """Find the argc binary in the system or build it."""
        # Check if argc is in PATH
        try:
            result = subprocess.run(
                ["which", "argc"], 
                capture_output=True, 
                text=True, 
                timeout=5
            )
            if result.returncode == 0:
                return result.stdout.strip()
        except Exception:
            pass
        
        # Check if we can build from source
        argc_src = Path(__file__).parent.parent.parent / "2do" / "argc"
        if argc_src.exists():
            try:
                # Build argc binary
                logger.info("Building argc from source...")
                result = subprocess.run(
                    ["cargo", "build", "--release"],
                    cwd=argc_src,
                    capture_output=True,
                    text=True,
                    timeout=300  # 5 minute timeout
                )
                
                if result.returncode == 0:
                    binary_path = argc_src / "target" / "release" / "argc"
                    if binary_path.exists():
                        logger.info(f"Successfully built argc at {binary_path}")
                        return str(binary_path)
                else:
                    logger.warning(f"Failed to build argc: {result.stderr}")
            except Exception as e:
                logger.warning(f"Could not build argc: {e}")
        
        return None

    def _initialize_argc(self):
        """Initialize argc and test functionality."""
        try:
            # Test if argc is working
            if self.argc_binary is None:
                raise RuntimeError("Argc binary path is None")
            result = subprocess.run(
                [self.argc_binary, "--version"],
                capture_output=True,
                text=True,
                timeout=10
            )
            
            if result.returncode == 0:
                self.available = True
                self._load_example_schemas()
                logger.info(
                    f"Argc adapter initialized with binary: {self.argc_binary}"
                )
            else:
                logger.error(f"Argc binary test failed: {result.stderr}")
                self._setup_compatibility_mode()
                
        except Exception as e:
            logger.error(f"Failed to initialize argc: {e}")
            self._setup_compatibility_mode()

    def _load_example_schemas(self):
        """Load example argc schemas from the source directory."""
        argc_examples = (
            Path(__file__).parent.parent.parent / "2do" / "argc" / "examples"
        )
        if argc_examples.exists():
            for script_file in argc_examples.glob("*.sh"):
                try:
                    self.parse_argc_script(script_file)
                except Exception as e:
                    logger.warning(
                        f"Failed to parse example {script_file}: {e}"
                    )

    def _setup_compatibility_mode(self):
        """Setup production mode with robust argc binary management."""
        self.available = False
        
        logger.error(
            "Argc binary not found. Attempting automatic installation..."
        )
        
        if self._install_argc_binary():
            self._initialize_argc()
        else:
            raise RuntimeError(
                "Argc binary is required for production operation. "
                "Please install argc manually or ensure it's available in "
                "PATH. "
                "See installation instructions at: "
                "https://github.com/sigoden/argc"
            )

    def _install_argc_binary(self) -> bool:
        """Attempt to install argc binary automatically."""
        try:
            import subprocess
            import platform
            
            system = platform.system().lower()
            
            if system == "linux":
                result = subprocess.run(
                    ["cargo", "install", "argc"],
                    capture_output=True,
                    text=True,
                    timeout=300
                )
                
                if result.returncode == 0:
                    import os
                    cargo_bin = os.path.expanduser("~/.cargo/bin/argc")
                    if os.path.exists(cargo_bin):
                        self.argc_binary = cargo_bin
                        logger.info(
                            f"Successfully installed argc at {cargo_bin}"
                        )
                        return True
            
            argc_src = Path(__file__).parent.parent.parent / "2do" / "argc"
            if argc_src.exists():
                result = subprocess.run(
                    ["cargo", "build", "--release"],
                    cwd=argc_src,
                    capture_output=True,
                    text=True,
                    timeout=300
                )
                
                if result.returncode == 0:
                    binary_path = argc_src / "target" / "release" / "argc"
                    if binary_path.exists():
                        self.argc_binary = str(binary_path)
                        logger.info(f"Successfully built argc at {binary_path}")
                        return True
            
            return False
            
        except Exception as e:
            logger.error(f"Failed to install argc binary: {e}")
            return False

    def parse_argc_script(self, script_path: Path) -> Optional[ArgcSchema]:
        """Parse an argc script and extract command definitions."""
        if not script_path.exists():
            logger.error(f"Script file not found: {script_path}")
            return None
        
        try:
            content = script_path.read_text()
            
            # Parse the script content for argc annotations
            schema = self._parse_script_content(content, script_path)
            
            if schema:
                self.schemas[script_path.name] = schema
                
                # Register all commands with function registry
                for command in schema.commands:
                    command_name = f"{script_path.stem}_{command.name}"
                    self.commands[command_name] = command
                    self._register_command_as_function(command_name, command)
                
                logger.info(
                    f"Parsed argc script {script_path.name} with "
                    f"{len(schema.commands)} commands"
                )
                return schema
            
        except Exception as e:
            logger.error(f"Failed to parse argc script {script_path}: {e}")
            return None

    def _parse_script_content(
        self, content: str, script_path: Path
    ) -> Optional[ArgcSchema]:
        """Parse script content to extract argc command definitions."""
        lines = content.split('\n')
        
        # Find script description
        script_description = ""
        commands = []
        global_flags = []
        global_options = []
        
        current_command = None
        in_function = False
        
        for line in lines:
            line = line.strip()
            
            # Parse argc annotations
            if line.startswith('# @describe'):
                script_description = line.replace('# @describe', '').strip()
            
            elif line.startswith('# @cmd'):
                # Start of a new command
                command_desc = line.replace('# @cmd', '').strip()
                current_command = ArgcCommand(
                    name="",  # Will be set when function is found
                    description=command_desc,
                    script_path=str(script_path)
                )
            
            elif line.startswith('# @alias') and current_command:
                alias = line.replace('# @alias', '').strip()
                current_command.alias = alias
            
            elif line.startswith('# @flag') and current_command:
                flag_def = self._parse_flag_definition(line)
                if flag_def:
                    current_command.flags.append(flag_def)
            
            elif line.startswith('# @option') and current_command:
                option_def = self._parse_option_definition(line)
                if option_def:
                    current_command.options.append(option_def)
            
            elif line.startswith('# @arg') and current_command:
                arg_def = self._parse_arg_definition(line)
                if arg_def:
                    current_command.args.append(arg_def)
            
            elif line.endswith('() {') and not in_function:
                # Function definition found
                func_name = line.replace('() {', '').strip()
                if current_command and not current_command.name:
                    current_command.name = func_name
                    commands.append(current_command)
                    current_command = None
                in_function = True
            
            elif line == '}' and in_function:
                in_function = False
        
        if script_description or commands:
            return ArgcSchema(
                script_name=script_path.name,
                description=script_description,
                commands=commands,
                global_flags=global_flags,
                global_options=global_options,
                metadata={"source_path": str(script_path)}
            )
        
        return None

    def _parse_flag_definition(self, line: str) -> Optional[Dict[str, Any]]:
        """Parse a flag definition line."""
        # Example: # @flag     -f --force              Override existing file
        parts = line.replace('# @flag', '').strip().split(None, 2)
        
        if len(parts) >= 2:
            flag_info = {"type": "flag"}
            
            # Parse flag names
            if parts[0].startswith('-'):
                if parts[0].startswith('--'):
                    flag_info["name"] = parts[0][2:]
                else:
                    flag_info["short"] = parts[0][1:]
            
            if len(parts) > 1 and parts[1].startswith('--'):
                flag_info["name"] = parts[1][2:]
            elif len(parts) > 1 and parts[1].startswith('-'):
                flag_info["short"] = parts[1][1:]
            
            # Description
            if len(parts) > 2:
                flag_info["description"] = parts[2]
            
            return flag_info
        
        return None

    def _parse_option_definition(self, line: str) -> Optional[Dict[str, Any]]:
        """Parse an option definition line."""
        # Example: # @option   -t --tries <NUM>        Set number of retries
        parts = line.replace('# @option', '').strip().split(None, 3)
        
        if len(parts) >= 2:
            option_info = {"type": "option"}
            
            # Parse option names
            part_idx = 0
            if parts[part_idx].startswith('-'):
                if parts[part_idx].startswith('--'):
                    option_info["name"] = parts[part_idx][2:]
                else:
                    option_info["short"] = parts[part_idx][1:]
                part_idx += 1
            
            if part_idx < len(parts) and parts[part_idx].startswith('--'):
                option_info["name"] = parts[part_idx][2:]
                part_idx += 1
            elif part_idx < len(parts) and parts[part_idx].startswith('-'):
                option_info["short"] = parts[part_idx][1:]
                part_idx += 1
            
            # Parse type information
            if part_idx < len(parts) and parts[part_idx].startswith('<'):
                type_info = parts[part_idx].strip('<>')
                option_info["value_type"] = type_info.lower()
                part_idx += 1
            
            # Description
            if part_idx < len(parts):
                option_info["description"] = ' '.join(parts[part_idx:])
            
            return option_info
        
        return None

    def _parse_arg_definition(self, line: str) -> Optional[Dict[str, Any]]:
        """Parse an argument definition line."""
        # Example: # @arg      source!                 Url to download from
        parts = line.replace('# @arg', '').strip().split(None, 2)
        
        if len(parts) >= 1:
            arg_info = {"type": "arg"}
            
            # Parse argument name and requirement
            arg_name = parts[0]
            if arg_name.endswith('!'):
                arg_info["name"] = arg_name[:-1]
                arg_info["required"] = "true"
            else:
                arg_info["name"] = arg_name
                arg_info["required"] = "false"
            
            # Description
            if len(parts) > 1:
                arg_info["description"] = ' '.join(parts[1:])
            
            return arg_info
        
        return None

    def _register_command_as_function(
        self, command_name: str, command: ArgcCommand
    ):
        """Register an argc command as a function in the function registry."""
        try:
            # Build parameters from argc command definition
            parameters = {}
            
            # Add arguments as parameters
            for arg in command.args:
                parameters[arg["name"]] = ParameterSpec(
                    type="string",  # Default type for argc args
                    description=arg.get(
                        "description", f"Argument: {arg['name']}"
                    ),
                    required=arg.get("required", False)
                )
            
            # Add flags as boolean parameters
            for flag in command.flags:
                param_name = flag.get("name", flag.get("short", "flag"))
                parameters[param_name] = ParameterSpec(
                    type="boolean",
                    description=flag.get("description", f"Flag: {param_name}"),
                    required=False,
                    default=False
                )
            
            # Add options as parameters
            for option in command.options:
                param_name = option.get("name", option.get("short", "option"))
                param_type = option.get("value_type", "string")
                parameters[param_name] = ParameterSpec(
                    type=param_type,
                    description=option.get(
                        "description", f"Option: {param_name}"
                    ),
                    required=False
                )
            
            # Create function specification
            func_spec = FunctionSpec(
                name=f"argc_{command_name}",
                description=command.description or 
                f"Argc command: {command.name}",
                parameters=parameters,
                safety_class=SafetyClass.MEDIUM,
                cost_unit=0.5,
                implementation_ref=f"argc.command.{command_name}",
                tags=["argc", "cli", "command"],
                metadata={
                    "argc_command": command.name,
                    "script_path": command.script_path,
                    "alias": command.alias
                }
            )
            
            # Register implementation
            async def argc_command_impl(**kwargs):
                return await self.execute_command(command_name, kwargs)
            
            # Register with function registry
            success = self.function_registry.register_function(
                func_spec, argc_command_impl
            )
            if success:
                self.function_registry.activate_function(func_spec.name)
                logger.info(
                    f"Registered argc command as function: {func_spec.name}"
                )
            
        except Exception as e:
            logger.error(f"Failed to register command {command_name}: {e}")

    async def execute_command(
        self, command_name: str, arguments: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Execute an argc command with the provided arguments."""
        command = self.commands.get(command_name)
        if not command:
            return {"error": f"Command {command_name} not found"}
        
        if not self.available:
            raise RuntimeError(
                f"Argc binary not available. Command {command_name} cannot be "
                "executed. "
                "Please ensure argc is properly installed and initialized."
            )
        
        try:
            # Build argc command line
            script_path = command.script_path
            cmd_args = [self.argc_binary, script_path, command.name]
            
            # Add flags
            for flag in command.flags:
                flag_name = flag.get("name", flag.get("short"))
                if flag_name in arguments and arguments[flag_name]:
                    if flag.get("name"):
                        cmd_args.append(f"--{flag['name']}")
                    elif flag.get("short"):
                        cmd_args.append(f"-{flag['short']}")
            
            # Add options
            for option in command.options:
                option_name = option.get("name", option.get("short"))
                if (option_name in arguments and 
                    arguments[option_name] is not None):
                    value = arguments[option_name]
                    if option.get("name"):
                        cmd_args.extend([f"--{option['name']}", str(value)])
                    elif option.get("short"):
                        cmd_args.extend([f"-{option['short']}", str(value)])
            
            # Add positional arguments
            for arg in command.args:
                arg_name = arg["name"]
                if arg_name in arguments:
                    cmd_args.append(str(arguments[arg_name]))
                elif arg.get("required", False):
                    return {"error": f"Required argument {arg_name} missing"}
            
            # Execute command
            process = await asyncio.create_subprocess_exec(
                *cmd_args,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )
            
            stdout, stderr = await process.communicate()
            
            result = {
                "command": command.name,
                "exit_code": process.returncode,
                "stdout": stdout.decode().strip(),
                "stderr": stderr.decode().strip(),
                "arguments": arguments
            }
            
            if process.returncode == 0:
                result["success"] = True
            else:
                result["success"] = False
                result["error"] = stderr.decode().strip()
            
            return result
            
        except Exception as e:
            logger.error(f"Failed to execute argc command {command_name}: {e}")
            return {"error": str(e), "command": command.name}

    def get_command_schema(self, command_name: str) -> Optional[Dict[str, Any]]:
        """Get the schema for a specific command."""
        command = self.commands.get(command_name)
        if not command:
            return None
        
        return {
            "name": command.name,
            "description": command.description,
            "alias": command.alias,
            "flags": command.flags,
            "options": command.options,
            "args": command.args,
            "script_path": command.script_path,
            "metadata": command.metadata
        }

    def list_commands(self) -> List[Dict[str, Any]]:
        """List all available argc commands."""
        return [
            {
                "name": name,
                "command": cmd.name,
                "description": cmd.description,
                "alias": cmd.alias,
                "script": cmd.script_path,
                "parameters": len(cmd.args) + len(cmd.flags) + len(cmd.options)
            }
            for name, cmd in self.commands.items()
        ]

    def parse_command_string(
        self, command_string: str
    ) -> Optional[Dict[str, Any]]:
        """Parse a command string and extract structured data."""
        if not self.available:
            raise RuntimeError(
                "Argc binary not available. Cannot parse command string. "
                "Please ensure argc is properly installed and initialized."
            )
        
        try:
            # Use argc to parse command string
            with tempfile.NamedTemporaryFile(
                mode='w', suffix='.sh', delete=False
            ) as f:
                # Create a temporary argc script for parsing
                f.write('''#!/bin/bash
# @describe Parse command
# @arg input! Input to parse
parse_cmd() {
    echo "$argc_input"
}
eval "$(argc --argc-eval "$0" "$@")"
''')
                temp_script = f.name
            
            # Execute parsing
            if self.argc_binary is None:
                raise RuntimeError("Argc binary path is None")
            result = subprocess.run(
                [self.argc_binary, temp_script, "parse_cmd", command_string],
                capture_output=True,
                text=True,
                timeout=10
            )
            
            # Clean up temp file
            Path(temp_script).unlink()
            
            if result.returncode == 0:
                return {
                    "parsed": True,
                    "result": result.stdout.strip(),
                    "command_string": command_string
                }
            else:
                return {
                    "parsed": False,
                    "error": result.stderr.strip(),
                    "command_string": command_string
                }
                
        except Exception as e:
            logger.error(f"Failed to parse command string: {e}")
            return {"parsed": False, "error": str(e)}

    def health_check(self) -> Dict[str, Any]:
        """Perform health check of the Argc adapter."""
        return {
            "status": "healthy" if self.available else "compatibility_mode",
            "argc_available": self.available,
            "binary_path": self.argc_binary,
            "schemas_loaded": len(self.schemas),
            "commands_registered": len(self.commands),
            "functions_registered": len([
                f for f in self.function_registry.functions.values()
                if f.implementation_ref.startswith("argc.command.")
            ])
        }
