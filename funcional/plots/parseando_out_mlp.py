import sys

texts = sys.argv[1:] if len(sys.argv) >= 2 else ["in_mlp.txt"]

samples = []
for txt_in in texts:
    comparisons = 0
    iterations = 0
    particles = None
    
    with open(txt_in, "r") as f:
        txt = f.read()


        
        for line in txt.split('\n'):
            #print(line)
            if (len(line) > 0):
                if '#' in line:
                    iterations += 1
                elif '$' in line:
                    samples.append((particles, comparisons, iterations))
                    particles = None
                    iterations = 0
                    comparisons = 0
                elif '"' in line:
                    comparisons += 1
                else:
                    particles = int(line.split()[-1])

        
#print(samples)


for s in samples:
    print("{}\t{}".format(s[0], s[1]/s[2]))
