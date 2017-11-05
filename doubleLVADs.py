import io
import re

questions = open("unweightedLVAD-fullDates.csv", 'r')
answers = open("unweightedLVAD-fullDates-CountLVAD.csv", 'w')

count = 0
for line in questions:
    count = count + 1
    if(count == 1):
        answers.write(line.strip() + ",numLVADs\n")
    else:
        print(line.count("3766"))
        answers.write(line.strip() + "," + str(line.count("3766")) + "\n")

questions.close()
answers.close()
