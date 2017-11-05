import io
import re

### import all the labels

#       directory = "C:\\Users\\David\\Documents\\NIS-Small\\NIS-2013-2014\\"
directory = "C:\\Users\\David\\Dropbox\\NIS-Small\\NIS-2013-2014\\"

pattern = re.compile("\"\\w[\\d ]*\" = \"\\w[\\d ]*:.*\"")
code = re.compile("\"\\w[\\d ]*\"")

#boundedString = re.compile("'[...!']*'")
boundedString = re.compile("'[\w\s+-.\\\\\[\]]*'")

counts = {}
header = "code"
counts["OHT"] = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

counts["yearSum"] = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

relevantDiags = []

relevantProcs = []

relevantProcs.append("375") #heart replacement procedures
relevantProcs.append("3751") #heart transplant
relevantProcs.append("336") #combined heart lung transplant


#### Make File to pull demographics

Question = open("OHT-121314.ASC", 'w')
BadLines = open("OHTBadLines-121314.ASC", 'w')
comorb = open("OHTComorbidities-121314.ASC", 'w')


############### 12 - 14 #############


for i in range(24,27):


	year = 1988 + i

	print(year)

	header = header + "," + str(year)

	FormatFileName = directory + "Format" + str(year) + ".csv"
	fileFormat = open(FormatFileName, 'r')

	DXstart = 0
	DXend = 0
	PRstart = 0
	PRend = 0
	WTstart = 0
	WTend = 0
	keyStart = 0
	keyEnd = 0
	
	for line in fileFormat:
		items = line.strip().split(',')
		if(items[0] == "DX1"):
			DXstart = int(items[1]) - 1
		if(items[0] == "DX15"):
			DXend = int(items[2]) - 0
		if(items[0] == "PR1"):
			PRstart = int(items[1]) - 1
		if(items[0] == "PR15"): 
			PRend = int(items[2]) - 0
		if(items[0] == "DISCWT"):
			WTstart = int(items[1]) - 1
			WTend = int(items[2])
		if(items[0].__contains__("KEY") | items[0].__contains__("SEQ") ):
			keyStart = int(items[1]) - 1
			keyEnd = int(items[2])


	fileFormat.close()
	
	#print DXstart, DXend, PRstart, PRend, (DXend-DXstart)/15, (PRend-PRstart)/15, WTstart, WTend
		
	yearFileName = directory + "\\NIS_" + str(year) + "_CORE.asc"
	yearFile = open(yearFileName, "r")

	lineCount = 0
	
	for line in yearFile:
                
		lineCount += 1

		#print line
		#print len(line)
		
		diag = line[DXstart:DXend]
		proc = line[PRstart:PRend]
		
		key = line[keyStart:keyEnd]

		#print "D:" + diag
		#print "P:" + proc
		#print "W:" + line[WTstart:WTend]

		counts["yearSum"][i]  = counts["yearSum"][i] + float(line[WTstart:WTend])
		
		#print line[WTstart:WTend]
		
		allCodes = {}
		allDiags = {}
		allProcs = {}
		
		for fifteen in range(0,15):
			thisDiag = diag[(fifteen*5):(fifteen*5+5)].strip("\"\' ")
			thisProc = proc[(fifteen*4):(fifteen*4+4)].strip("\"\' ")

			allCodes[thisDiag] = 1
			allCodes[thisProc] = 1

			allDiags[thisDiag] = 1
			allProcs[thisProc] = 1
			
		   #print thisDiag,thisProc, diag, proc


		hasSurgery = bool(0)


		for procedure in relevantProcs:
			if procedure in allProcs:
				hasSurgery = bool(1)

		for diagnosis in relevantDiags:
			if diagnosis in allDiags:
				hasSurgery = bool(1)

			
			
		if((hasSurgery)):
			#print("BINGO")
			counts["OHT"][i] = counts["OHT"][i] + float(line[WTstart:WTend])
			Question.write(str(year) + " " + line)
			comorb.write(key + ",D," + str(allDiags.keys()).strip("[] ") + ",P," + str(allProcs.keys()).strip("[] ") + "\n")
			#print("yes")




	print counts["yearSum"][i]
	print counts["OHT"]



BadLines.close()
Question.close()
comorb.close()

#"I:\Inpatient Database Studies\NIS_" + str(year) + "\Data\NIS_" + str(year) + "_CORE.asc"
print counts["OHT"]

		
### print the dictionary
