import base64
import os
from openai import OpenAI
audio_path = os.path.join(os.path.dirname(os.path.realpath(__file__)), '..', 'audio', 'mary_had_lamb.ogg')
openai_api_key = 'EMPTY'
openai_api_base = 'http://localhost:2242/v1'
client = OpenAI(api_key=openai_api_key, base_url=openai_api_base)
models = client.models.list()
model = models.data[0].id
def encode_audio_base64_from_file(file_path: str) -> str:
    with open(file_path, 'rb') as f:
        return base64.b64encode(f.read()).decode('utf-8')
audio_base64 = encode_audio_base64_from_file(audio_path)
chat_completion = client.chat.completions.create(messages=[{'role': 'user', 'content': [{'type': 'text', 'text': "What's in this audio?"}, {'type': 'audio_url', 'audio_url': {'url': f'data:audio/ogg;base64,{audio_base64}'}}]}], model=model, max_tokens=128)
result = chat_completion.choices[0].message.content
print(f'Chat completion output: {result}')