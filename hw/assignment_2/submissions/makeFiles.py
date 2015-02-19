import sys
import random
import csv
o_file=[]
f_lin = ''
with open('WB.tsv') as fl:
	f_lin = fl.readline()
	for line in fl:
		o_file.append(line)
for line in sys.stdin:
	name = line.rstrip('\n\r')+'.tsv'
	random.shuffle(o_file)
	with open(name,'w') as fl:
		fl.write(f_lin)
		for i in o_file:
			fl.write(i)


