import requests
ENDPOINT = 'http://localhost:5001/api'
payload = {'prompt': 'Niko the kobold stalked carefully down the alley, his small scaly figure obscured by a dusky cloak that fluttered lightly in the cold winter breeze.', 'max_context_length': 4096, 'max_length': 128, 'rep_pen': 1.1, 'rep_pen_range': 512, 'rep_pen_slope': 0.7, 'temperature': 0.8, 'top_k': 100, 'top_p': 0.9}
try:
    response = requests.post(f'{ENDPOINT}/v1/generate', json=payload)
    if response.status_code == 200:
        results = response.json()['results']
        text = results[0]['text']
        print(text)
except Exception as e:
    print(f'An error occurred: {e}')