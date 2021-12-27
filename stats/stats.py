files = ["stats_map", "stats_not_map", "stats_thread", "stats_not_thread"]

for file in files :
    f = open(file + ".txt", "r")
    lines = f.readlines()
    f.close()
    numbers = []
    for line in lines :
        numbers.append(float(line))
    print("Moyenne pour le fichier \"" + file + "\" : " + str(round(sum(numbers)/len(numbers), 2)))