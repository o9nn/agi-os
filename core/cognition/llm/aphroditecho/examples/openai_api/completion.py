from openai import OpenAI
openai_api_key = 'EMPTY'
openai_api_base = 'http://localhost:2242/v1'
client = OpenAI(api_key=openai_api_key, base_url=openai_api_base)
models = client.models.list()
model = models.data[0].id
stream = False
completion = client.completions.create(model=model, prompt='A robot may not injure a human being', echo=False, n=2, stream=stream, logprobs=3)
print('Completion results:')
if stream:
    for c in completion:
        print(c)
else:
    print(completion)