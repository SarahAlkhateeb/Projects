#script that help in counting number of classes we have in images after labelling 

import os

zeros = 0
ones = 0
directory = 'obj'
for filename in os.listdir(directory):
    if filename.endswith(".txt"):
        file_path = os.path.join(directory, filename)
        file = open(file_path, 'r') 
        Lines = file.readlines() 
        for line in Lines: 
            if len(line.strip()) != 0 :
                first_num = line.split(None, 1)[0]
                if first_num == '1':
                    ones+=1
                else:
                    zeros+=1
        file.close
    else:
        continue

print("number of ones= ", ones)
print("number of zeros= ", zeros)
