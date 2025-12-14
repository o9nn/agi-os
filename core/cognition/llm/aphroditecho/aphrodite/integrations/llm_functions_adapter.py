"""
LLM Functions Adapter - Integrates the llm-functions component with AAR system

This adapter provides function calling and MCP-style tool layer integration,
discovering and registering tools from the llm-functions directory structure.
"""

import asyncio
import json
import logging
from typing import Dict, List, Any, Optional, Callable
from dataclasses import dataclass, field
from pathlib import Path
import importlib.util
import ast
import re

from ..function_registry import FunctionRegistry, FunctionSpec, ParameterSpec, SafetyClass

logger = logging.getLogger(__name__)


@dataclass
class LLMFunction:
    """Represents a function from the llm-functions collection."""
    name: str
    description: str
    file_path: str
    language: str  # python, javascript, shell, etc.
    parameters: Dict[str, Any]
    safety_level: str = "medium"
    category: str = "general"
    tags: List[str] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class MCPServer:
    """Represents an MCP (Model Context Protocol) server configuration."""
    name: str
    command: List[str]
    env: Dict[str, str] = field(default_factory=dict)
    description: str = ""
    tools: List[str] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)


class LLMFunctionsAdapter:
    """
    Adapter for integrating llm-functions with the AAR system.
    
    Provides:
    - Discovery of Python, JavaScript, and shell function tools
    - MCP server integration
    - Function execution in sandboxed environments
    - Parameter validation and type conversion
    - Safety classification and permission management
    """

    def __init__(self, function_registry: FunctionRegistry):
        self.function_registry = function_registry
        self.functions: Dict[str, LLMFunction] = {}
        self.mcp_servers: Dict[str, MCPServer] = {}
        self.function_implementations: Dict[str, Callable] = {}
        
        # Set up paths
        self.llm_functions_root = Path(__file__).parent.parent.parent / "2do" / "llm-functions"
        
        if self.llm_functions_root.exists():
            self._discover_functions()
            self._discover_mcp_servers()
        else:
            logger.warning("llm-functions directory not found, running in compatibility mode")
            self._setup_compatibility_mode()

    def _discover_functions(self):
        """Discover functions from the llm-functions directory."""
        try:
            # Scan tools directory
            tools_dir = self.llm_functions_root / "tools"
            if tools_dir.exists():
                self._scan_tools_directory(tools_dir)
            
            # Scan agents directory
            agents_dir = self.llm_functions_root / "agents"
            if agents_dir.exists():
                self._scan_agents_directory(agents_dir)
            
            logger.info(f"Discovered {len(self.functions)} functions from llm-functions")
            
            # Register all discovered functions
            for func_name, func_spec in self.functions.items():
                self._register_llm_function(func_name, func_spec)
                
        except Exception as e:
            logger.error(f"Failed to discover llm-functions: {e}")

    def _scan_tools_directory(self, tools_dir: Path):
        """Scan the tools directory for function files."""
        for file_path in tools_dir.iterdir():
            if file_path.is_file():
                try:
                    if file_path.suffix == '.py':
                        self._parse_python_function(file_path)
                    elif file_path.suffix == '.js':
                        self._parse_javascript_function(file_path)
                    elif file_path.suffix == '.sh':
                        self._parse_shell_function(file_path)
                except Exception as e:
                    logger.warning(f"Failed to parse function file {file_path}: {e}")

    def _scan_agents_directory(self, agents_dir: Path):
        """Scan the agents directory for specialized agent functions."""
        for agent_dir in agents_dir.iterdir():
            if agent_dir.is_dir():
                # Each agent directory may contain multiple tools
                for file_path in agent_dir.rglob("*"):
                    if file_path.is_file() and file_path.suffix in ['.py', '.js', '.sh']:
                        try:
                            if file_path.suffix == '.py':
                                self._parse_python_function(file_path, category=agent_dir.name)
                            elif file_path.suffix == '.js':
                                self._parse_javascript_function(file_path, category=agent_dir.name)
                            elif file_path.suffix == '.sh':
                                self._parse_shell_function(file_path, category=agent_dir.name)
                        except Exception as e:
                            logger.warning(f"Failed to parse agent function {file_path}: {e}")

    def _parse_python_function(self, file_path: Path, category: str = "general"):
        """Parse a Python function file."""
        try:
            content = file_path.read_text(encoding='utf-8')
            
            # Parse the AST to extract function information
            tree = ast.parse(content)
            
            for node in ast.walk(tree):
                if isinstance(node, ast.FunctionDef) and node.name == 'run':
                    # Extract function metadata
                    func_spec = self._extract_python_function_spec(node, file_path, content, category)
                    if func_spec:
                        self.functions[func_spec.name] = func_spec
                        break
                        
        except Exception as e:
            logger.warning(f"Failed to parse Python function {file_path}: {e}")

    def _extract_python_function_spec(self, func_node: ast.FunctionDef, file_path: Path, 
                                     content: str, category: str) -> Optional[LLMFunction]:
        """Extract function specification from Python AST node."""
        try:
            # Get docstring
            docstring = ast.get_docstring(func_node) or f"Function from {file_path.name}"
            
            # Extract type hints and parameters
            parameters = {}
            
            for arg in func_node.args.args:
                param_name = arg.arg
                if param_name == 'self':
                    continue
                
                # Get type annotation
                param_type = "any"
                required = True
                
                if arg.annotation:
                    annotation_str = self._ast_to_string(arg.annotation)
                    param_type = self._convert_python_type_to_json(annotation_str)
                    if annotation_str.startswith("Optional["):
                        required = False
                
                # Get default value
                defaults = func_node.args.defaults
                num_defaults = len(defaults)
                len(func_node.args.args)
                len(func_node.args.args) - num_defaults
                
                if len(func_node.args.args) - len(defaults) <= func_node.args.args.index(arg):
                    required = False
                
                # Extract parameter description from docstring
                param_description = self._extract_param_description(docstring, param_name)
                
                parameters[param_name] = {
                    "type": param_type,
                    "description": param_description or f"Parameter: {param_name}",
                    "required": required
                }
            
            # Determine safety level based on file content
            safety_level = self._determine_safety_level(content)
            
            return LLMFunction(
                name=file_path.stem,
                description=docstring.split('\n')[0] if docstring else f"Function from {file_path.name}",
                file_path=str(file_path),
                language="python",
                parameters=parameters,
                safety_level=safety_level,
                category=category,
                tags=self._extract_tags_from_path(file_path),
                metadata={
                    "docstring": docstring,
                    "function_name": "run"
                }
            )
            
        except Exception as e:
            logger.error(f"Failed to extract Python function spec: {e}")
            return None

    def _parse_javascript_function(self, file_path: Path, category: str = "general"):
        """Parse a JavaScript function file."""
        try:
            content = file_path.read_text(encoding='utf-8')
            
            # Look for JSDoc comments and exports.run function
            func_spec = self._extract_javascript_function_spec(content, file_path, category)
            if func_spec:
                self.functions[func_spec.name] = func_spec
                
        except Exception as e:
            logger.warning(f"Failed to parse JavaScript function {file_path}: {e}")

    def _extract_javascript_function_spec(self, content: str, file_path: Path, 
                                        category: str) -> Optional[LLMFunction]:
        """Extract function specification from JavaScript content."""
        try:
            # Extract JSDoc comment
            jsdoc_match = re.search(r'/\*\*(.*?)\*/', content, re.DOTALL)
            description = "JavaScript function"
            parameters = {}
            
            if jsdoc_match:
                jsdoc = jsdoc_match.group(1)
                
                # Extract description
                desc_match = re.search(r'\* (.+?)(?:\n\s*\*\s*@|\n\s*\*\s*$)', jsdoc, re.DOTALL)
                if desc_match:
                    description = desc_match.group(1).strip()
                
                # Extract parameters from @property tags
                property_matches = re.findall(r'\*\s*@property\s+\{([^}]+)\}\s+(\w+)(?:\s*-\s*(.+))?', jsdoc)
                for prop_match in property_matches:
                    prop_type, prop_name, prop_desc = prop_match
                    required = not prop_name.startswith('[')
                    if not required:
                        prop_name = prop_name.strip('[]')
                    
                    parameters[prop_name] = {
                        "type": self._convert_jsdoc_type_to_json(prop_type),
                        "description": prop_desc.strip() if prop_desc else f"Parameter: {prop_name}",
                        "required": required
                    }
            
            # Determine safety level
            safety_level = self._determine_safety_level(content)
            
            return LLMFunction(
                name=file_path.stem,
                description=description,
                file_path=str(file_path),
                language="javascript",
                parameters=parameters,
                safety_level=safety_level,
                category=category,
                tags=self._extract_tags_from_path(file_path),
                metadata={"function_name": "run"}
            )
            
        except Exception as e:
            logger.error(f"Failed to extract JavaScript function spec: {e}")
            return None

    def _parse_shell_function(self, file_path: Path, category: str = "general"):
        """Parse a shell script function file."""
        try:
            content = file_path.read_text(encoding='utf-8')
            
            # Extract function metadata from comments
            func_spec = self._extract_shell_function_spec(content, file_path, category)
            if func_spec:
                self.functions[func_spec.name] = func_spec
                
        except Exception as e:
            logger.warning(f"Failed to parse shell function {file_path}: {e}")

    def _extract_shell_function_spec(self, content: str, file_path: Path, 
                                   category: str) -> Optional[LLMFunction]:
        """Extract function specification from shell script content."""
        try:
            lines = content.split('\n')
            description = f"Shell script: {file_path.name}"
            parameters = {}
            
            # Look for comment-based documentation
            for line in lines[:20]:  # Check first 20 lines
                line = line.strip()
                if line.startswith('# @desc'):
                    description = line.replace('# @desc', '').strip()
                elif line.startswith('# @param'):
                    # Parse parameter definition
                    param_match = re.match(r'# @param (\w+) (.+)', line)
                    if param_match:
                        param_name, param_desc = param_match.groups()
                        parameters[param_name] = {
                            "type": "string",
                            "description": param_desc,
                            "required": True
                        }
            
            # If no explicit parameters, look for environment variable usage
            if not parameters:
                env_vars = re.findall(r'\$\{?([A-Z_][A-Z0-9_]*)\}?', content)
                for var in set(env_vars):
                    if not var.startswith(('PATH', 'HOME', 'USER', 'PWD')):  # Skip common env vars
                        parameters[var.lower()] = {
                            "type": "string",
                            "description": f"Environment variable: {var}",
                            "required": False
                        }
            
            # Shell scripts are generally higher risk
            safety_level = "high"
            
            return LLMFunction(
                name=file_path.stem,
                description=description,
                file_path=str(file_path),
                language="shell",
                parameters=parameters,
                safety_level=safety_level,
                category=category,
                tags=self._extract_tags_from_path(file_path),
                metadata={"executable": True}
            )
            
        except Exception as e:
            logger.error(f"Failed to extract shell function spec: {e}")
            return None

    def _discover_mcp_servers(self):
        """Discover MCP server configurations."""
        try:
            mcp_dir = self.llm_functions_root / "mcp"
            if mcp_dir.exists():
                # Look for MCP server configurations
                for server_dir in mcp_dir.iterdir():
                    if server_dir.is_dir():
                        self._parse_mcp_server(server_dir)
                        
        except Exception as e:
            logger.error(f"Failed to discover MCP servers: {e}")

    def _parse_mcp_server(self, server_dir: Path):
        """Parse an MCP server configuration."""
        try:
            # Look for configuration files
            config_files = ['config.json', 'package.json', 'pyproject.toml']
            
            for config_file in config_files:
                config_path = server_dir / config_file
                if config_path.exists():
                    if config_file == 'config.json':
                        config = json.loads(config_path.read_text())
                        mcp_server = MCPServer(
                            name=server_dir.name,
                            command=config.get('command', []),
                            env=config.get('env', {}),
                            description=config.get('description', ''),
                            tools=config.get('tools', []),
                            metadata=config
                        )
                        self.mcp_servers[server_dir.name] = mcp_server
                        break
                        
        except Exception as e:
            logger.warning(f"Failed to parse MCP server {server_dir}: {e}")

    def _register_llm_function(self, func_name: str, llm_func: LLMFunction):
        """Register an LLM function with the function registry."""
        try:
            # Convert parameters
            parameters = {}
            for param_name, param_spec in llm_func.parameters.items():
                parameters[param_name] = ParameterSpec(
                    type=param_spec["type"],
                    description=param_spec["description"],
                    required=param_spec["required"]
                )
            
            # Map safety level
            safety_map = {
                "low": SafetyClass.LOW,
                "medium": SafetyClass.MEDIUM,
                "high": SafetyClass.HIGH,
                "critical": SafetyClass.CRITICAL
            }
            safety_class = safety_map.get(llm_func.safety_level, SafetyClass.MEDIUM)
            
            # Create function specification
            func_spec = FunctionSpec(
                name=f"llmfunc_{func_name}",
                description=llm_func.description,
                parameters=parameters,
                safety_class=safety_class,
                cost_unit=self._calculate_cost_unit(llm_func),
                implementation_ref=f"llm-functions.{func_name}",
                tags=["llm-functions", llm_func.language, llm_func.category] + llm_func.tags,
                allow_network=self._requires_network(llm_func),
                metadata={
                    "language": llm_func.language,
                    "file_path": llm_func.file_path,
                    "category": llm_func.category,
                    "original_name": func_name
                }
            )
            
            # Create implementation wrapper
            impl_func = self._create_implementation_wrapper(llm_func)
            
            # Register with function registry
            success = self.function_registry.register_function(func_spec, impl_func)
            if success:
                self.function_registry.activate_function(func_spec.name)
                logger.info(f"Registered llm-function: {func_spec.name}")
                
        except Exception as e:
            logger.error(f"Failed to register llm-function {func_name}: {e}")

    def _create_implementation_wrapper(self, llm_func: LLMFunction) -> Callable:
        """Create a wrapper function for executing the LLM function."""
        
        async def wrapper(**kwargs):
            return await self.execute_llm_function(llm_func.name, kwargs)
        
        return wrapper

    async def execute_llm_function(self, func_name: str, arguments: Dict[str, Any]) -> Dict[str, Any]:
        """Execute an LLM function with the provided arguments."""
        llm_func = self.functions.get(func_name)
        if not llm_func:
            return {"error": f"Function {func_name} not found"}
        
        try:
            if llm_func.language == "python":
                return await self._execute_python_function(llm_func, arguments)
            elif llm_func.language == "javascript":
                return await self._execute_javascript_function(llm_func, arguments)
            elif llm_func.language == "shell":
                return await self._execute_shell_function(llm_func, arguments)
            else:
                return {"error": f"Unsupported language: {llm_func.language}"}
                
        except Exception as e:
            logger.error(f"Failed to execute function {func_name}: {e}")
            return {"error": str(e)}

    async def _execute_python_function(self, llm_func: LLMFunction, arguments: Dict[str, Any]) -> Dict[str, Any]:
        """Execute a Python function."""
        try:
            # Import the module
            spec = importlib.util.spec_from_file_location("llm_func_module", llm_func.file_path)
            module = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(module)
            
            # Get the run function
            run_func = getattr(module, 'run', None)
            if not run_func:
                return {"error": "No 'run' function found in Python module"}
            
            # Execute the function
            if asyncio.iscoroutinefunction(run_func):
                result = await run_func(**arguments)
            else:
                result = run_func(**arguments)
            
            return {"result": result, "function": llm_func.name, "language": "python"}
            
        except Exception as e:
            return {"error": f"Python execution failed: {str(e)}"}

    async def _execute_javascript_function(self, llm_func: LLMFunction, arguments: Dict[str, Any]) -> Dict[str, Any]:
        """Execute a JavaScript function."""
        try:
            # Create a temporary script that loads and executes the function
            script_content = f"""
const func = require('{llm_func.file_path}');
const args = {json.dumps(arguments)};
const result = func.run(args);
console.log(JSON.stringify({{result: result}}));
"""
            
            # Execute with Node.js
            process = await asyncio.create_subprocess_exec(
                'node', '-e', script_content,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )
            
            stdout, stderr = await process.communicate()
            
            if process.returncode == 0:
                result = json.loads(stdout.decode())
                return {"result": result["result"], "function": llm_func.name, "language": "javascript"}
            else:
                return {"error": f"JavaScript execution failed: {stderr.decode()}"}
                
        except Exception as e:
            return {"error": f"JavaScript execution error: {str(e)}"}

    async def _execute_shell_function(self, llm_func: LLMFunction, arguments: Dict[str, Any]) -> Dict[str, Any]:
        """Execute a shell script function."""
        try:
            # Set up environment variables from arguments
            env = {**os.environ}
            for key, value in arguments.items():
                env[key.upper()] = str(value)
            
            # Execute the shell script
            process = await asyncio.create_subprocess_exec(
                'bash', llm_func.file_path,
                env=env,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )
            
            stdout, stderr = await process.communicate()
            
            return {
                "result": stdout.decode().strip(),
                "exit_code": process.returncode,
                "stderr": stderr.decode().strip(),
                "function": llm_func.name,
                "language": "shell"
            }
            
        except Exception as e:
            return {"error": f"Shell execution error: {str(e)}"}

    def _setup_compatibility_mode(self):
        """Setup compatibility mode with basic function examples."""
        # Create some basic function examples
        basic_functions = [
            LLMFunction(
                name="echo_demo",
                description="Echo back the input text",
                file_path="<compatibility>",
                language="python",
                parameters={"text": {"type": "string", "description": "Text to echo", "required": True}},
                safety_level="low",
                category="demo"
            ),
            LLMFunction(
                name="math_demo", 
                description="Perform basic mathematical operations",
                file_path="<compatibility>",
                language="python",
                parameters={
                    "operation": {"type": "string", "description": "Math operation (+, -, *, /)", "required": True},
                    "a": {"type": "number", "description": "First number", "required": True},
                    "b": {"type": "number", "description": "Second number", "required": True}
                },
                safety_level="low",
                category="demo"
            )
        ]
        
        for func in basic_functions:
            self.functions[func.name] = func
            self._register_llm_function(func.name, func)
        
        logger.info("LLM Functions adapter running in compatibility mode")

    # Utility methods
    def _ast_to_string(self, node) -> str:
        """Convert AST node to string."""
        import ast
        return ast.unparse(node)

    def _convert_python_type_to_json(self, type_str: str) -> str:
        """Convert Python type annotation to JSON schema type."""
        type_map = {
            "str": "string",
            "int": "integer", 
            "float": "number",
            "bool": "boolean",
            "list": "array",
            "List": "array",
            "dict": "object",
            "Dict": "object"
        }
        
        # Handle complex types
        if "List[" in type_str:
            return "array"
        elif "Dict[" in type_str:
            return "object"
        elif "Optional[" in type_str:
            inner_type = type_str.replace("Optional[", "").rstrip("]")
            return self._convert_python_type_to_json(inner_type)
        elif "Literal[" in type_str:
            return "string"  # Enum types
        
        return type_map.get(type_str, "string")

    def _convert_jsdoc_type_to_json(self, type_str: str) -> str:
        """Convert JSDoc type to JSON schema type."""
        type_map = {
            "string": "string",
            "String": "string",
            "number": "number",
            "Number": "number",
            "Integer": "integer",
            "boolean": "boolean",
            "Boolean": "boolean",
            "Array": "array",
            "Object": "object"
        }
        
        # Handle array types
        if type_str.endswith("[]"):
            return "array"
        
        return type_map.get(type_str, "string")

    def _extract_param_description(self, docstring: str, param_name: str) -> Optional[str]:
        """Extract parameter description from docstring."""
        if not docstring:
            return None
        
        # Look for Args: section
        args_match = re.search(r'Args:\s*\n(.*?)(?:\n\s*\n|\Z)', docstring, re.DOTALL)
        if args_match:
            args_section = args_match.group(1)
            param_match = re.search(rf'{param_name}:\s*(.+?)(?:\n\s*\w+:|$)', args_section, re.DOTALL)
            if param_match:
                return param_match.group(1).strip()
        
        return None

    def _determine_safety_level(self, content: str) -> str:
        """Determine safety level based on content analysis."""
        high_risk_patterns = [
            r'subprocess', r'os\.system', r'exec', r'eval',
            r'rm\s+', r'delete', r'format', r'DROP TABLE',
            r'curl', r'wget', r'requests\.get', r'http'
        ]
        
        medium_risk_patterns = [
            r'open\(', r'file', r'write', r'read',
            r'import\s+os', r'sys\.', r'env'
        ]
        
        content_lower = content.lower()
        
        for pattern in high_risk_patterns:
            if re.search(pattern, content_lower):
                return "high"
        
        for pattern in medium_risk_patterns:
            if re.search(pattern, content_lower):
                return "medium"
        
        return "low"

    def _requires_network(self, llm_func: LLMFunction) -> bool:
        """Check if function requires network access."""
        network_indicators = ['http', 'url', 'curl', 'wget', 'requests', 'fetch', 'api']
        
        func_content = llm_func.name.lower() + " " + llm_func.description.lower()
        return any(indicator in func_content for indicator in network_indicators)

    def _calculate_cost_unit(self, llm_func: LLMFunction) -> float:
        """Calculate cost unit based on function characteristics."""
        base_cost = 1.0
        
        if llm_func.language == "shell":
            base_cost *= 2.0  # Shell scripts are more expensive
        
        if llm_func.safety_level == "high":
            base_cost *= 1.5
        elif llm_func.safety_level == "critical":
            base_cost *= 3.0
        
        if self._requires_network(llm_func):
            base_cost *= 2.0
        
        return base_cost

    def _extract_tags_from_path(self, file_path: Path) -> List[str]:
        """Extract tags from file path."""
        tags = []
        
        # Add category from parent directory
        if file_path.parent.name not in ['tools', 'agents']:
            tags.append(file_path.parent.name)
        
        # Add tags based on filename
        filename_lower = file_path.stem.lower()
        
        if 'web' in filename_lower or 'http' in filename_lower:
            tags.append('web')
        if 'search' in filename_lower:
            tags.append('search')
        if 'file' in filename_lower or 'fs_' in filename_lower:
            tags.append('filesystem')
        if 'sql' in filename_lower or 'db' in filename_lower:
            tags.append('database')
        if 'mail' in filename_lower or 'email' in filename_lower:
            tags.append('communication')
        
        return tags

    def get_function(self, func_name: str) -> Optional[LLMFunction]:
        """Get function specification by name."""
        return self.functions.get(func_name)

    def list_functions(self, category: Optional[str] = None, 
                      language: Optional[str] = None) -> List[LLMFunction]:
        """List functions with optional filtering."""
        functions = list(self.functions.values())
        
        if category:
            functions = [f for f in functions if f.category == category]
        
        if language:
            functions = [f for f in functions if f.language == language]
        
        return functions

    def get_mcp_server(self, server_name: str) -> Optional[MCPServer]:
        """Get MCP server configuration by name."""
        return self.mcp_servers.get(server_name)

    def list_mcp_servers(self) -> List[MCPServer]:
        """List all MCP servers."""
        return list(self.mcp_servers.values())

    def health_check(self) -> Dict[str, Any]:
        """Perform health check of the LLM Functions adapter."""
        return {
            "status": "healthy",
            "llm_functions_root_exists": self.llm_functions_root.exists(),
            "functions_discovered": len(self.functions),
            "mcp_servers_discovered": len(self.mcp_servers),
            "functions_by_language": {
                lang: len([f for f in self.functions.values() if f.language == lang])
                for lang in ["python", "javascript", "shell"]
            },
            "functions_registered": len([
                f for f in self.function_registry.functions.values()
                if f.implementation_ref.startswith("llm-functions.")
            ])
        }