#!/usr/bin/python

from sys import stdin

print "Number in Fibonacci sequence:"
input = raw_input('> ')
m = int(input)
i = 0
f1 = 0
f2 = 1


while i < m:
	result = f1 + f2
	if i == m-1:
		print(result)
		i = i+ 1
	else:
		f1 = f2
		f2 = result
		i = i + 1
