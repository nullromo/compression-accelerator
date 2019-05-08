import csv

with open('sw_results.csv') as csvfile:
    reader = csv.reader(csvfile, delimiter=';')
    real_dict = dict()
    repeat_dict = dict()
    random_dict = dict()
    for rowf in reader:
        row = list(filter(None, rowf))
        if(len(row) < 2):
            continue
        if(row[0] == 'type'):
            continue
        typ = row[0]
        length = row[1]
        cycles = row[2]
        compressedLength = row[3]
        ratio = float(length) / float(compressedLength)
        eff = float(cycles) / float(length)
        print((typ + '\t' + length + '\t %02.4f  \t %02.3f') % (ratio, eff))
        if(typ == 'real'):
            real_dict[length] = (ratio, eff)
        if(typ == 'random'):
            random_dict[length] = (ratio, eff)
        if(typ == 'repeat'):
            repeat_dict[length] = (ratio, eff)

    # make LaTeX table
    table = list()
    for random, real, repeat in zip(random_dict.items(), real_dict.items(), repeat_dict.items()):
        table.append((int(random[0]), (str(random[0]) + " & %2.4f & %3.3f & %2.4f & %3.3f & %2.4f & %3.3f \\\\") % (random[1][0], random[1][1], real[1][0], real[1][1], repeat[1][0], repeat[1][1])))
    print("\nLaTeX table\n\n")
    for line in sorted(table, key=lambda x: x[0]):
        print(line[1])
        print("\\hhline{-||--||--||--|}")

    # make Excel data
    excel = list()
    def print_dict(d, index):
        for size, value in sorted(d.items(), key=lambda x: int(x[0])):
            print(("%2.4f") % (value[index]))
    print("\nExcel Data\n\n")
    print('\nrandom ratio\n')
    print_dict(random_dict, 0)
    print('\nrandom eff\n')
    print_dict(random_dict, 1)
    print('\nreal ratio\n')
    print_dict(real_dict, 0)
    print('\nreal eff\n')
    print_dict(real_dict, 1)
    print('\nrepeat ratio\n')
    print_dict(repeat_dict, 0)
    print('\nrepeat eff\n')
    print_dict(repeat_dict, 1)
