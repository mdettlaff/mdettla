import sys

fib = lambda x: fib(x-2) + fib(x-1) if x > 1 else x

if len(sys.argv) == 2:
    n = int(sys.argv[1])
else:
    n = 10
for i in range(0, n):
    print fib(i),
