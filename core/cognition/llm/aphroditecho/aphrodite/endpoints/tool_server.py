from abc import ABC, abstractmethod
from contextlib import AbstractAsyncContextManager, asynccontextmanager
from typing import Any, Optional
from loguru import logger
from openai_harmony import ToolNamespaceConfig
from aphrodite.endpoints.tool import HarmonyBrowserTool, HarmonyPythonTool, Tool
class ToolServer(ABC):
    @abstractmethod
    def has_tool(self, tool_name: str) -> bool:
        pass
    @abstractmethod
    def get_tool_description(self, tool_name: str) -> Optional[ToolNamespaceConfig]:
        pass
    @abstractmethod
    def new_session(self, tool_name: str) -> AbstractAsyncContextManager[Any]:
        ...
class DemoToolServer(ToolServer):
    def __init__(self):
        self.tools: dict[str, Tool] = {}
        browser_tool = HarmonyBrowserTool()
        if browser_tool.enabled:
            self.tools['browser'] = browser_tool
        python_tool = HarmonyPythonTool()
        if python_tool.enabled:
            self.tools['python'] = python_tool
        logger.info('DemoToolServer initialized with tools: %s', list(self.tools.keys()))
    def has_tool(self, tool_name: str) -> bool:
        return tool_name in self.tools
    def get_tool_description(self, tool_name: str) -> Optional[ToolNamespaceConfig]:
        if tool_name not in self.tools:
            return None
        if tool_name == 'browser':
            return ToolNamespaceConfig.browser()
        elif tool_name == 'python':
            return ToolNamespaceConfig.python()
        else:
            raise ValueError(f'Unknown tool {tool_name}')
    @asynccontextmanager
    async def new_session(self, tool_name: str):
        yield self.tools[tool_name]