import os
import sys
import json
import logging
import argparse
from typing import List, Dict, Any, Union, AsyncGenerator
from datetime import datetime
from contextlib import asynccontextmanager
import torch
import tiktoken
import requests
from fastapi import FastAPI, HTTPException, Depends, Request, status
from fastapi.responses import StreamingResponse, JSONResponse
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel, Field
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from model import GPTConfig, GPT
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s', handlers=[logging.StreamHandler()])
logger = logging.getLogger('nanocog')
class ModelConfig:
    def __init__(self, model_path: str, device: str='cuda', max_tokens: int=2048):
        self.model_path = model_path
        self.device = device
        self.max_tokens = max_tokens
        self.model = None
        self.tokenizer = None
    def load_model(self):
        try:
            logger.info(f'Loading model from {self.model_path}')
            checkpoint = torch.load(self.model_path, map_location=self.device)
            gptconf = GPTConfig(**checkpoint['model_args'])
            self.model = GPT(gptconf)
            state_dict = checkpoint['model']
            unwanted_prefix = '_orig_mod.'
            for k, v in list(state_dict.items()):
                if k.startswith(unwanted_prefix):
                    state_dict[k[len(unwanted_prefix):]] = state_dict.pop(k)
            self.model.load_state_dict(state_dict)
            self.model.eval()
            self.model.to(self.device)
            self.tokenizer = tiktoken.get_encoding('gpt2')
            logger.info('Model loaded successfully')
            return True
        except Exception as e:
            logger.error(f'Failed to load model: {str(e)}')
            raise RuntimeError(f'Failed to load model: {str(e)}')
    def generate(self, prompt: str, max_new_tokens: int=500, temperature: float=0.7, top_k: int=200, stream: bool=False) -> Union[str, AsyncGenerator[str, None]]:
        if not self.model or not self.tokenizer:
            raise RuntimeError('Model not loaded. Call load_model() first.')
        input_ids = self.tokenizer.encode(prompt, allowed_special={'<|endoftext|>'})
        if len(input_ids) > self.max_tokens - max_new_tokens:
            logger.warning(f'Prompt too long ({len(input_ids)} tokens), truncating')
            input_ids = input_ids[-(self.max_tokens - max_new_tokens):]
        x = torch.tensor(input_ids, dtype=torch.long, device=self.device)[None, ...]
        if stream:
            return self._stream_generate(x, max_new_tokens, temperature, top_k)
        else:
            return self._batch_generate(x, max_new_tokens, temperature, top_k)
    def _batch_generate(self, x, max_new_tokens, temperature, top_k):
        with torch.no_grad():
            with torch.amp.autocast(device_type='cuda' if 'cuda' in self.device else 'cpu'):
                y = self.model.generate(x, max_new_tokens, temperature=temperature, top_k=top_k)
                generated_text = self.tokenizer.decode(y[0].tolist())
                prompt_text = self.tokenizer.decode(x[0].tolist())
                if generated_text.startswith(prompt_text):
                    generated_text = generated_text[len(prompt_text):]
                return generated_text
    async def _stream_generate(self, x, max_new_tokens, temperature, top_k):
        with torch.no_grad():
            with torch.amp.autocast(device_type='cuda' if 'cuda' in self.device else 'cpu'):
                x.shape[1]
                past = None
                for token_index in range(max_new_tokens):
                    if past is None:
                        outputs = self.model(x, use_cache=True)
                        logits = outputs.logits
                        past = outputs.past_key_values
                    else:
                        outputs = self.model(x[:, -1:], use_cache=True, past_key_values=past)
                        logits = outputs.logits
                        past = outputs.past_key_values
                    next_token_logits = logits[:, -1, :]
                    if temperature > 0:
                        next_token_logits = next_token_logits / temperature
                    if top_k > 0:
                        v, _ = torch.topk(next_token_logits, min(top_k, next_token_logits.size(-1)))
                        next_token_logits[next_token_logits < v[:, [-1]]] = float('-inf')
                    probs = torch.nn.functional.softmax(next_token_logits, dim=-1)
                    next_token = torch.multinomial(probs, num_samples=1)
                    x = torch.cat((x, next_token), dim=1)
                    new_token_text = self.tokenizer.decode([next_token[0].item()])
                    yield new_token_text
                    if next_token[0].item() == self.tokenizer.eot_token:
                        break
class AtomSpaceClient:
    def __init__(self, endpoint: str):
        self.endpoint = endpoint
        self.session = requests.Session()
    def test_connection(self) -> bool:
        try:
            response = self.session.get(f'{self.endpoint}/status', timeout=5)
            response.raise_for_status()
            return True
        except Exception as e:
            logger.error(f'Failed to connect to AtomSpace at {self.endpoint}: {str(e)}')
            return False
    def get_atom_count(self) -> int:
        try:
            response = self.session.get(f'{self.endpoint}/atoms/count', timeout=5)
            response.raise_for_status()
            return response.json().get('count', 0)
        except Exception as e:
            logger.error(f'Failed to get atom count: {str(e)}')
            return 0
    def get_atoms_by_type(self, atom_type: str, limit: int=100) -> List[Dict[str, Any]]:
        try:
            response = self.session.get(f'{self.endpoint}/atoms/type/{atom_type}', params={'limit': limit}, timeout=10)
            response.raise_for_status()
            return response.json().get('atoms', [])
        except Exception as e:
            logger.error(f'Failed to get atoms by type {atom_type}: {str(e)}')
            return []
    def get_high_sti_atoms(self, threshold: float=0.5, limit: int=100) -> List[Dict[str, Any]]:
        try:
            response = self.session.get(f'{self.endpoint}/atoms/sti', params={'threshold': threshold, 'limit': limit}, timeout=10)
            response.raise_for_status()
            return response.json().get('atoms', [])
        except Exception as e:
            logger.error(f'Failed to get high STI atoms: {str(e)}')
            return []
    def get_active_goals(self, limit: int=20) -> List[Dict[str, Any]]:
        try:
            response = self.session.get(f'{self.endpoint}/goals/active', params={'limit': limit}, timeout=10)
            response.raise_for_status()
            return response.json().get('goals', [])
        except Exception as e:
            logger.error(f'Failed to get active goals: {str(e)}')
            return []
    def get_attention_allocation_summary(self) -> Dict[str, Any]:
        try:
            response = self.session.get(f'{self.endpoint}/attention/summary', timeout=10)
            response.raise_for_status()
            return response.json()
        except Exception as e:
            logger.error(f'Failed to get attention allocation summary: {str(e)}')
            return {}
    def get_agent_introspection_data(self) -> Dict[str, Any]:
        data = {'timestamp': datetime.now().isoformat(), 'atom_count': self.get_atom_count(), 'active_goals': self.get_active_goals(), 'attention_summary': self.get_attention_allocation_summary(), 'high_sti_atoms': self.get_high_sti_atoms()}
        return data
class ChatMessage(BaseModel):
    role: str = Field(..., description='The role of the message sender (user or assistant)')
    content: str = Field(..., description='The content of the message')
class ChatRequest(BaseModel):
    messages: List[ChatMessage] = Field(..., description='The conversation history')
    max_tokens: int = Field(500, description='Maximum number of tokens to generate')
    temperature: float = Field(0.7, description='Sampling temperature')
    top_k: int = Field(200, description='Top-k sampling parameter')
    stream: bool = Field(False, description='Whether to stream the response')
class DiagnosticRequest(BaseModel):
    atomspace_endpoint: str = Field(..., description='The AtomSpace REST API endpoint')
    focus_areas: List[str] = Field(default=['attention', 'goals', 'patterns'], description='Areas to focus the diagnostic on')
    max_tokens: int = Field(1000, description='Maximum number of tokens to generate')
    temperature: float = Field(0.6, description='Sampling temperature')
    stream: bool = Field(False, description='Whether to stream the response')
class ChatResponse(BaseModel):
    text: str = Field(..., description='The generated text')
    model: str = Field(..., description='The model used for generation')
    created_at: str = Field(..., description='Timestamp of the response')
    tokens_generated: int = Field(..., description='Number of tokens generated')
class DiagnosticResponse(BaseModel):
    analysis: str = Field(..., description='The diagnostic analysis')
    raw_data: Dict[str, Any] = Field(..., description='The raw data used for the analysis')
    recommendations: List[str] = Field(..., description='List of recommendations')
    model: str = Field(..., description='The model used for generation')
    created_at: str = Field(..., description='Timestamp of the response')
@asynccontextmanager
async def lifespan(app: FastAPI):
    try:
        app.state.model_config.load_model()
    except Exception as e:
        logger.error(f'Failed to load model: {str(e)}')
    yield
    if hasattr(app.state, 'model_config') and app.state.model_config.model:
        logger.info('Cleaning up model resources')
app = FastAPI(title='NanoCog API', description='API for interacting with a CogPrime-trained nanoGPT model', version='0.1.0', lifespan=lifespan)
app.add_middleware(CORSMiddleware, allow_origins=['*'], allow_credentials=True, allow_methods=['*'], allow_headers=['*'])
def get_model_config(request: Request) -> ModelConfig:
    if not hasattr(request.app.state, 'model_config') or not request.app.state.model_config.model:
        raise HTTPException(status_code=status.HTTP_503_SERVICE_UNAVAILABLE, detail='Model not loaded or initialization failed')
    return request.app.state.model_config
def get_atomspace_client(atomspace_endpoint: str) -> AtomSpaceClient:
    client = AtomSpaceClient(atomspace_endpoint)
    if not client.test_connection():
        raise HTTPException(status_code=status.HTTP_503_SERVICE_UNAVAILABLE, detail=f'Failed to connect to AtomSpace at {atomspace_endpoint}')
    return client
@app.get('/')
async def root():
    return {'name': 'NanoCog API', 'description': 'API for interacting with a CogPrime-trained nanoGPT model', 'status': 'operational', 'version': '0.1.0'}
@app.get('/status')
async def status(request: Request):
    model_loaded = hasattr(request.app.state, 'model_config') and request.app.state.model_config.model is not None
    return {'status': 'operational' if model_loaded else 'initializing', 'model_loaded': model_loaded, 'model_path': request.app.state.model_config.model_path if model_loaded else None, 'device': request.app.state.model_config.device if model_loaded else None, 'timestamp': datetime.now().isoformat()}
@app.post('/chat', response_model=ChatResponse)
async def chat(request: ChatRequest, model_config: ModelConfig=Depends(get_model_config)):
    try:
        prompt = format_chat_prompt(request.messages)
        generated_text = model_config.generate(prompt=prompt, max_new_tokens=request.max_tokens, temperature=request.temperature, top_k=request.top_k, stream=False)
        tokens_generated = len(model_config.tokenizer.encode(generated_text))
        return ChatResponse(text=generated_text, model=os.path.basename(model_config.model_path), created_at=datetime.now().isoformat(), tokens_generated=tokens_generated)
    except Exception as e:
        logger.error(f'Error in chat endpoint: {str(e)}')
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=f'Error generating response: {str(e)}')
@app.post('/chat/stream')
async def chat_stream(request: ChatRequest, model_config: ModelConfig=Depends(get_model_config)):
    try:
        prompt = format_chat_prompt(request.messages)
        async def generate_stream():
            try:
                async for token in model_config.generate(prompt=prompt, max_new_tokens=request.max_tokens, temperature=request.temperature, top_k=request.top_k, stream=True):
                    yield f"data: {json.dumps({'token': token})}\n\n"
            except Exception as e:
                logger.error(f'Error in streaming generation: {str(e)}')
                yield f"data: {json.dumps({'error': str(e)})}\n\n"
            yield 'data: [DONE]\n\n'
        return StreamingResponse(generate_stream(), media_type='text/event-stream')
    except Exception as e:
        logger.error(f'Error in chat stream endpoint: {str(e)}')
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=f'Error generating streaming response: {str(e)}')
@app.post('/diagnostics', response_model=DiagnosticResponse)
async def run_diagnostics(request: DiagnosticRequest, model_config: ModelConfig=Depends(get_model_config)):
    try:
        atomspace_client = get_atomspace_client(request.atomspace_endpoint)
        introspection_data = atomspace_client.get_agent_introspection_data()
        prompt = format_diagnostic_prompt(introspection_data, request.focus_areas)
        analysis = model_config.generate(prompt=prompt, max_new_tokens=request.max_tokens, temperature=request.temperature, top_k=50, stream=False)
        recommendations = extract_recommendations(analysis)
        return DiagnosticResponse(analysis=analysis, raw_data=introspection_data, recommendations=recommendations, model=os.path.basename(model_config.model_path), created_at=datetime.now().isoformat())
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f'Error in diagnostics endpoint: {str(e)}')
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=f'Error generating diagnostic: {str(e)}')
@app.post('/diagnostics/stream')
async def diagnostics_stream(request: DiagnosticRequest, model_config: ModelConfig=Depends(get_model_config)):
    try:
        atomspace_client = get_atomspace_client(request.atomspace_endpoint)
        introspection_data = atomspace_client.get_agent_introspection_data()
        prompt = format_diagnostic_prompt(introspection_data, request.focus_areas)
        async def generate_stream():
            yield f"data: {json.dumps({'type': 'metadata', 'raw_data': introspection_data})}\n\n"
            try:
                async for token in model_config.generate(prompt=prompt, max_new_tokens=request.max_tokens, temperature=request.temperature, top_k=50, stream=True):
                    yield f"data: {json.dumps({'type': 'token', 'token': token})}\n\n"
            except Exception as e:
                logger.error(f'Error in streaming diagnostics: {str(e)}')
                yield f"data: {json.dumps({'type': 'error', 'error': str(e)})}\n\n"
            yield 'data: [DONE]\n\n'
        return StreamingResponse(generate_stream(), media_type='text/event-stream')
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f'Error in diagnostics stream endpoint: {str(e)}')
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=f'Error streaming diagnostic: {str(e)}')
def format_chat_prompt(messages: List[ChatMessage]) -> str:
    prompt = ''
    for message in messages:
        if message.role.lower() == 'user':
            prompt += f'\nUser: {message.content}\n'
        elif message.role.lower() == 'assistant':
            prompt += f'\nNanoCog: {message.content}\n'
        elif message.role.lower() == 'system':
            prompt += f'\n# System Instruction: {message.content}\n'
    prompt += '\nNanoCog: '
    return prompt
def format_diagnostic_prompt(introspection_data: Dict[str, Any], focus_areas: List[str]) -> str:
    summary = []
    summary.append(f"Total atoms: {introspection_data.get('atom_count', 'unknown')}")
    if 'active_goals' in introspection_data and introspection_data['active_goals']:
        summary.append(f"Active goals: {len(introspection_data['active_goals'])}")
        for i, goal in enumerate(introspection_data['active_goals'][:5]):
            goal_name = goal.get('name', 'Unnamed')
            goal_sti = goal.get('sti', 0.0)
            summary.append(f'  Goal {i + 1}: {goal_name} (STI: {goal_sti:.2f})')
    if 'attention_summary' in introspection_data:
        att_summary = introspection_data['attention_summary']
        if isinstance(att_summary, dict):
            summary.append('Attention allocation:')
            for key, value in att_summary.items():
                if isinstance(value, (int, float)):
                    summary.append(f'  {key}: {value}')
    if 'high_sti_atoms' in introspection_data:
        high_sti = introspection_data['high_sti_atoms']
        summary.append(f'High STI atoms: {len(high_sti)}')
        atom_types = {}
        for atom in high_sti:
            atom_type = atom.get('type', 'unknown')
            atom_types[atom_type] = atom_types.get(atom_type, 0) + 1
        for atom_type, count in atom_types.items():
            summary.append(f'  {atom_type}: {count}')
    prompt = f"# System Instruction: You are NanoCog, an AI assistant specialized in CogPrime architecture and OpenCog systems. You're analyzing a live CogPrime agent's AtomSpace. Provide detailed introspective diagnostics based on the data below, focusing on: {', '.join(focus_areas)}.\n\n## AtomSpace Data ({datetime.now().strftime('%Y-%m-%d %H:%M:%S')})\n{chr(10).join(summary)}\n\n## Raw Data (JSON)\n```json\n{json.dumps(introspection_data, indent=2)}\n```\n\n## Task\nAnalyze the above AtomSpace data and provide:\n1. A summary of the agent's current cognitive state\n2. Identification of any bottlenecks or issues\n3. Specific recommendations for optimization\n4. Relevant CogPrime principles that apply\n\nNanoCog (Diagnostic Analysis): \n"
    return prompt
def extract_recommendations(analysis: str) -> List[str]:
    recommendations = []
    for line in analysis.split('\n'):
        line = line.strip()
        if line.startswith(('1.', '2.', '3.', '4.', '5.', '6.', '7.', '8.', '9.', '0.')) and len(line) > 3 and (line[2] == ' '):
            recommendations.append(line[3:].strip())
        elif line.startswith(('- ', '* ', '• ')):
            recommendations.append(line[2:].strip())
        elif line.lower().startswith('recommendation:'):
            recommendations.append(line[14:].strip())
    if not recommendations:
        suggestive_words = ['should', 'recommend', 'consider', 'try', 'increase', 'decrease', 'optimize']
        for line in analysis.split('\n'):
            for word in suggestive_words:
                if word in line.lower():
                    recommendations.append(line.strip())
                    break
    return recommendations
@app.exception_handler(HTTPException)
async def http_exception_handler(request, exc):
    return JSONResponse(status_code=exc.status_code, content={'error': exc.detail})
@app.exception_handler(Exception)
async def general_exception_handler(request, exc):
    logger.error(f'Unhandled exception: {str(exc)}')
    return JSONResponse(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, content={'error': 'An unexpected error occurred'})
def main():
    parser = argparse.ArgumentParser(description='NanoCog Server')
    parser.add_argument('--model_path', type=str, required=True, help='Path to the model checkpoint')
    parser.add_argument('--device', type=str, default='cuda' if torch.cuda.is_available() else 'cpu', help='Device to run the model on (cuda, cpu, mps)')
    parser.add_argument('--max_tokens', type=int, default=2048, help='Maximum number of tokens in context')
    parser.add_argument('--host', type=str, default='0.0.0.0', help='Host to run the server on')
    parser.add_argument('--port', type=int, default=8000, help='Port to run the server on')
    args = parser.parse_args()
    model_config = ModelConfig(model_path=args.model_path, device=args.device, max_tokens=args.max_tokens)
    app.state.model_config = model_config
    import uvicorn
    uvicorn.run(app, host=args.host, port=args.port)
if __name__ == '__main__':
    main()