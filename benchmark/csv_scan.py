import csv

with open('sw-benchmark.csv') as csvfile:
    reader = csv.reader(csvfile, delimiter=' ')
    for rowf in reader:
        row = list(filter(None, rowf))
        if(len(row) < 2):
            continue
        if(row[0] == 'TYPE'):
            continue
        print(str(int(row[4]) / int(row[1])))