import sys

texts = sys.argv[1:] if len(sys.argv) >= 2 else ["in_mlp.txt", "in_mlp2.txt"]

samples = []
for txt_in in texts:
    comparisons = 0
    iterations = 1
    particles = None
    
    with open(txt_in, "r") as f:
        txt = f.read()


        
        for line in txt.split('\n'):
            if (len(line) > 0):
                if '"' in line:
                    comparisons += 1
                elif '#' == line[0]:
                    iterations += 1
                else:
                    #print(line)
                    particles = int(line.split()[-1])

        #print(comparisons, iterations, particles)
        samples.append((particles, comparisons, iterations))
#print(samples)


for s in samples:
    print("{}\t{}".format(s[0], s[1]))
