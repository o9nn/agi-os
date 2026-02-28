"""
KoboldCpp-Cog: Python interface for cognitive LLM inference.

Bridges KoboldCpp's OpenAI-compatible API with OpenCog AtomSpace,
providing context-aware text generation for cognitive agents.

Usage:
    from opencog.koboldcpp_cog import CognitiveInference

    engine = CognitiveInference("http://localhost:5001")
    result = engine.infer("What is the relationship between X and Y?")
    print(result.text)
"""

import json
import requests
from typing import List, Dict, Optional, Any
from dataclasses import dataclass, field


@dataclass
class GenerationParams:
    """Parameters for LLM text generation."""
    temperature: float = 0.7
    top_p: float = 0.9
    top_k: int = 40
    rep_pen: float = 1.1
    rep_pen_range: int = 512
    max_tokens: int = 256
    stop_sequences: List[str] = field(default_factory=list)
    stream: bool = False


@dataclass
class GenerationResult:
    """Result from LLM inference."""
    text: str = ""
    prompt_tokens: int = 0
    completion_tokens: int = 0
    finish_reason: str = ""
    success: bool = False
    error: str = ""


@dataclass
class CognitiveResult:
    """Result from cognitive inference pipeline."""
    response_text: str = ""
    raw_result: Optional[GenerationResult] = None
    atoms_created: int = 0
    atoms_updated: int = 0
    confidence: float = 0.0
    inference_mode: str = "query"


class KoboldCppClient:
    """HTTP client for KoboldCpp inference server."""

    def __init__(self, endpoint: str = "http://localhost:5001", timeout: int = 30):
        self.endpoint = endpoint.rstrip("/")
        self.timeout = timeout

    def is_connected(self) -> bool:
        """Check if KoboldCpp server is reachable."""
        try:
            r = requests.get(f"{self.endpoint}/api/v1/info", timeout=5)
            return r.status_code == 200
        except Exception:
            return False

    def get_info(self) -> Dict[str, Any]:
        """Get server information."""
        try:
            r = requests.get(f"{self.endpoint}/api/v1/info", timeout=5)
            return r.json()
        except Exception:
            return {"connected": False}

    def generate(self, prompt: str, params: Optional[GenerationParams] = None) -> GenerationResult:
        """Text completion via OpenAI-compatible API."""
        if params is None:
            params = GenerationParams()

        result = GenerationResult()
        try:
            payload = {
                "prompt": prompt,
                "max_tokens": params.max_tokens,
                "temperature": params.temperature,
                "top_p": params.top_p,
            }
            if params.stop_sequences:
                payload["stop"] = params.stop_sequences

            r = requests.post(
                f"{self.endpoint}/v1/completions",
                json=payload,
                timeout=self.timeout,
            )
            data = r.json()
            result.text = data["choices"][0]["text"]
            result.finish_reason = data["choices"][0].get("finish_reason", "")
            result.success = True
            if "usage" in data:
                result.prompt_tokens = data["usage"].get("prompt_tokens", 0)
                result.completion_tokens = data["usage"].get("completion_tokens", 0)
        except Exception as e:
            result.error = str(e)
            result.success = False

        return result

    def chat(self, messages: List[Dict[str, str]],
             params: Optional[GenerationParams] = None) -> GenerationResult:
        """Chat completion via OpenAI-compatible API."""
        if params is None:
            params = GenerationParams()

        result = GenerationResult()
        try:
            payload = {
                "messages": messages,
                "max_tokens": params.max_tokens,
                "temperature": params.temperature,
            }

            r = requests.post(
                f"{self.endpoint}/v1/chat/completions",
                json=payload,
                timeout=self.timeout,
            )
            data = r.json()
            result.text = data["choices"][0]["message"]["content"]
            result.success = True
        except Exception as e:
            result.error = str(e)
            result.success = False

        return result

    def generate_native(self, prompt: str,
                        params: Optional[GenerationParams] = None) -> GenerationResult:
        """Native KoboldAI generation API."""
        if params is None:
            params = GenerationParams()

        result = GenerationResult()
        try:
            payload = {
                "prompt": prompt,
                "max_length": params.max_tokens,
                "temperature": params.temperature,
                "top_p": params.top_p,
                "top_k": params.top_k,
                "rep_pen": params.rep_pen,
                "rep_pen_range": params.rep_pen_range,
            }

            r = requests.post(
                f"{self.endpoint}/api/v1/generate",
                json=payload,
                timeout=self.timeout,
            )
            data = r.json()
            result.text = data["results"][0]["text"]
            result.success = True
        except Exception as e:
            result.error = str(e)
            result.success = False

        return result


class AtomSpaceContextBuilder:
    """Extracts relevant AtomSpace context for LLM prompts."""

    def __init__(self, max_atoms: int = 100, max_tokens: int = 2048):
        self.max_atoms = max_atoms
        self.max_tokens = max_tokens

    def extract(self, query: str, atomspace=None) -> str:
        """Extract context from AtomSpace for a query."""
        # TODO: Connect to actual AtomSpace via Python bindings
        if atomspace is None:
            return f"# No AtomSpace connected\n# Query: {query}\n"

        # Placeholder for AtomSpace context extraction
        context_parts = []
        # Would iterate over atoms, filter by attention/relevance
        return "\n".join(context_parts)

    def extract_around(self, atom_ids: List[str], atomspace=None) -> str:
        """Extract context around specific atoms."""
        if atomspace is None:
            return f"# Focus atoms: {', '.join(atom_ids)}\n"
        return ""


class CognitiveInference:
    """
    Full cognitive inference pipeline combining LLM + AtomSpace.

    This is the primary Python interface for cognitive LLM operations.
    """

    def __init__(self, endpoint: str = "http://localhost:5001",
                 atomspace=None):
        self.client = KoboldCppClient(endpoint)
        self.context_builder = AtomSpaceContextBuilder()
        self.atomspace = atomspace
        self.conversation_history: List[Dict[str, str]] = []

    def infer(self, query: str, mode: str = "query",
              params: Optional[GenerationParams] = None) -> CognitiveResult:
        """
        Execute cognitive inference.

        Args:
            query: The user query or task
            mode: One of "query", "generate", "reason", "classify", "extract", "converse"
            params: Generation parameters

        Returns:
            CognitiveResult with response and metadata
        """
        result = CognitiveResult(inference_mode=mode)

        # Step 1: Extract context
        context = self.context_builder.extract(query, self.atomspace)

        # Step 2: Build prompt
        system_prompt = "You are a cognitive agent with access to a knowledge graph."
        full_prompt = f"{system_prompt}\n\n{context}\n\nQuery: {query}\n\nResponse:"

        # Step 3: Execute inference
        if mode == "converse":
            self.conversation_history.append({"role": "user", "content": query})
            messages = [{"role": "system", "content": system_prompt + "\n\n" + context}]
            messages.extend(self.conversation_history)
            raw = self.client.chat(messages, params)
            if raw.success:
                self.conversation_history.append(
                    {"role": "assistant", "content": raw.text})
        else:
            raw = self.client.generate(full_prompt, params)

        result.raw_result = raw
        result.response_text = raw.text
        result.confidence = 0.7 if raw.success else 0.0

        return result

    def reset_conversation(self):
        """Clear conversation history."""
        self.conversation_history.clear()

    def is_ready(self) -> bool:
        """Check if the inference engine is ready."""
        return self.client.is_connected()
