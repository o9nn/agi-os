def factorial():
    f, n = (1, 1)
    while True:
        yield f
        f, n = (f * n, n + 1)
for index, factorial_number in zip(range(51), factorial()):
    print('{i:3}!= {f:65}'.format(i=index, f=factorial_number))