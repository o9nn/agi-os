import urllib.request
def get_ipinfo():
    with urllib.request.urlopen('https://httpbin.org/ip') as response:
        data = response.read()
        return data.decode('utf-8')