from openai import OpenAI
openai_api_key = 'EMPTY'
openai_api_base = 'http://localhost:2242/v1'
client = OpenAI(api_key=openai_api_key, base_url=openai_api_base)
models = client.models.list()
model = models.data[0].id
responses = client.embeddings.create(input=['Hello my name is', 'The weather is nice today'], model=model)
for data in responses.data:
    print(data.embedding)