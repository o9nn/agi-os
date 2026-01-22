import base64
import os
from openai import OpenAI
openai_api_key = 'EMPTY'
openai_api_base = 'http://localhost:2242/v1'
client = OpenAI(api_key=openai_api_key, base_url=openai_api_base)
models = client.models.list()
model = models.data[0].id
def encode_image_base64_from_file(image_path: str) -> str:
    with open(image_path, 'rb') as image_file:
        result = base64.b64encode(image_file.read()).decode('utf-8')
    return result
image_path = os.path.join(os.path.dirname(os.path.realpath(__file__)), '../vision/burg.jpg')
image_base64 = encode_image_base64_from_file(image_path=image_path)
chat_completion_from_base64 = client.chat.completions.create(messages=[{'role': 'user', 'content': [{'type': 'text', 'text': "What's in this image?"}, {'type': 'image_url', 'image_url': {'url': f'data:image/jpeg;base64,{image_base64}'}}]}], model=model)
result = chat_completion_from_base64.choices[0].message.content
print(f'Chat completion output:{result}')