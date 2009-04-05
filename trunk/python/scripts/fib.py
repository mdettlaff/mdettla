import sys

def fib(x):
    if x == 0:
        return 0
    elif x == 1:
        return 1
    else:
        return fib(x-2) + fib(x-1)

if len(sys.argv) == 2:
    n = sys.argv[1]
else:
    n = 10
for i in range(0, int(n)):
    print fib(i),
