import io
import re

Questions = open("percutaneous-vad.ASC", 'r')
unweightedData = open("unweightedPVADAll.csv", 'w')


unweightedData.write("weight,year,age,died,pay1,pay2,sex,hospital,los,race,charges,zip,atype,disp,,d1,d2,d3,d4,d5,d6,d7,d8,d9,d10"
                     ",d11,d12,d13,d14,d15,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15"
                     ",p1day,p2day,p3day,p4day,p5day,p6day,p7day,p8day,p9day,p10day,p11day,p12day,p13day,p14day,p15day\n") #,IDENTIFIER


counter = 0

for line in Questions:
    year = line[0:4]
    restOfLine = line[5:len(line)]

    #if(int(year)>1997):
    #    restOfLine = " " + restOfLine[0:len(restOfLine)]

    #if(int(year)<1993):
    #    restOfLine = restOfLine[1:len(restOfLine)]
    
    #if(str(year) in range(1993,1998)):
    #    restOfLine = line[5:len(line)]
    #else:
    #    restOfLine = line[6:len(line)]
        ###weird formatting error from PullAllYearsFromTLEandEpilepsy such that years 1993 - 1997 has only one space after year

    fileFormat = open("C:\\Users\\David\\Dropbox\\NIS-Small\\Database\\Format\\Format" + str(year) + ".csv")

    ageStart = 0
    ageEnd = 0
    diedStart = 0
    diedEnd = 0
    pay1Start = 0
    pay1End = 0
    pay2Start = 0
    pay2End = 0
    femaleStart = 0
    femaleEnd = 0
    hospitalStart = 0
    hospitalEnd = 0
    weightStart = 0
    weightEnd = 0
    losStart = 0
    losEnd = 0
    raceStart = 0
    raceEnd = 0
    chargesStart = 0
    chargesEnd = 0
    zipIncStart = 0
    zipIncEnd = 0
    atypeStart = 0
    atypeEnd = 0
    dispStart = 0
    dispEnd = 0
    diagStart = 0
    diagEnd = 0
    procStart = 0
    procEnd = 0
    
    diags = ""
    procs = ""

    procDayStart = 0
    procDayEnd = 0
    procDays = ""
    
    for Fline in fileFormat:
        items = Fline.strip().split(',')
        if(items[0] == "AGE"):
            ageStart = int(items[1]) - 1
            ageEnd = int(items[2])
            
        if(items[0] == "DIED"):
            diedStart = int(items[1])- 1
            diedEnd = int(items[2])

        if(items[0] == "PAY1"):
            pay1Start = int(items[1])- 1
            pay1End = int(items[2])

        if(items[0] == "PAY2"):
            pay2Start = int(items[1])- 1
            pay2End = int(items[2])

        if((items[0] == "SEX") | (items[0] == "FEMALE")):
            femaleStart = int(items[1])- 1
            femaleEnd = int(items[2])

        if((items[0] == "HOSPID") | (items[0] == "HOSP_NIS")):
            hospitalStart = int(items[1])- 1
            hospitalEnd = int(items[2])

        if((items[0] == "DISCWT") | (items[0] == "DISCWT_U")):
            weightStart = int(items[1])- 1
            weightEnd = int(items[2])

        if(items[0] == "LOS"):
            losStart = int(items[1])- 1
            losEnd = int(items[2])

        if((items[0] == "ZIPINC_QRTL") | (items[0] == "ZIPINC4") | (items[0] == "ZIPINC")):
            zipIncStart = int(items[1])- 1
            zipIncEnd = int(items[2])    


        if(items[0] == "RACE"):
            raceStart = int(items[1])- 1
            raceEnd = int(items[2])

        if(items[0] == "TOTCHG"):
            chargesStart = int(items[1])- 1
            chargesEnd = int(items[2])

        if(items[0] == "ATYPE"):
            atypeStart = int(items[1])- 1
            atypeEnd = int(items[2])

        if((items[0] == "DISP") | (items[0] == "DISPUNIFORM")):
            dispStart = int(items[1])- 1
            dispEnd = int(items[2])

        if(int(year) <= 1992 or int(year) >= 1998):
            if(items[0] == "DX1"):
                diagStart = int(items[1])- 1
            if(items[0] == "DX15"):
                diagEnd = int(items[2])
            if(items[0] == "PR1"):
                procStart = int(items[1])- 1
            if(items[0] == "PR15"):
                procEnd = int(items[2])
            if(items[0] == "PRDAY1"):
                procDayStart = int(items[1])- 1
            if(items[0] == "PRDAY15"):
                procDayEnd = int(items[2])

            
            

            


    ### Hopefully this is not needed
    #if(int(year) > 1997):
    #    ageStart -= 14
    #    ageEnd -= 14
    #    diedStart -= 14
    #    diedEnd -= 15
    #    pay1Start -= 15
    #    pay1End -= 15
    #    pay2Start -= 15
    #    pay2End -= 15
    #    femaleStart -= 15
    #    femaleEnd -= 15
    #    hospitalStart -= 15
    #    hospitalEnd -= 15
    #    weightStart -= 15
    #    weightEnd -= 15
    #    losStart -= 15
    #    losEnd -= 15
    #    raceStart -= 15
    #    raceEnd -= 15
    #    chargesStart -= 15
    #    chargesEnd -= 15
        
    age = restOfLine[ageStart:ageEnd]
    died = restOfLine[diedStart:diedEnd]
    pay1 = restOfLine[pay1Start:pay1End]
    pay2 = restOfLine[pay2Start:pay2End]
    female = restOfLine[femaleStart:femaleEnd]
    disp = restOfLine[dispStart:dispEnd]

    ### Inconsistency between FEMALE and SEX
    #if(int(year) < 1998 and int(year) != 1993 ):
    #    if(len(female.strip(" -']")) != 0):
    #        female = " " + str((int(female.strip(" -'],")) - 1))


    #### 1993 - 1998 does not have a gender in file
        
    
    hospital = restOfLine[hospitalStart:hospitalEnd]
    weight = restOfLine[weightStart:weightEnd]
    los = restOfLine[losStart:losEnd]
    race = restOfLine[raceStart:raceEnd]
    charges = restOfLine[chargesStart:chargesEnd]
    zipInc = restOfLine[zipIncStart:zipIncEnd]
    atype = restOfLine[atypeStart:atypeEnd]


    if(int(year) <= 1992 or int(year) >= 1998):
        diagnoses = restOfLine[diagStart:diagEnd]
        procedures = restOfLine[procStart:procEnd]
        procedureDays = restOfLine[procDayStart:procDayEnd]
    
        for i in range(0,15):
            diags = diags + "," + diagnoses[(i*5):(i*5+5)]
            procs = procs + "," + procedures[(i*4):(i*4+4)]
            procDays = procDays + "," + procedureDays[(i*3):(i*3+3)]

        
    else:
        relevantCodes = line.split("[")[1]
        diagnoses = relevantCodes.split(',')[0].strip("'")
        procedures = relevantCodes.split(',')[1].strip("'")


        for i in range(0,len(diagnoses)/5):
            diags = diags + "," + diagnoses[(i*5):(i*5+5)]
        for i in range(0,len(procedures)/4):
            procs = procs + "," + procedures[(i*4):(i*4+4)]

    #print procedures



    #print diags    
    #print procs
    
    #identifier = restOfLine.split("COMORBS: ")[1].split(",D ,")[0]
    
    summary =  year + "," + age+ "," +died+ "," +pay1+ "," +pay2+ "," +female + "," + hospital + "," + los + "," + race + "," + charges + "," + zipInc + "," + atype + "," + disp + "," + diags + procs + procDays#+ "," + identifier
    #print procDays, procedureDays, procDayStart, procDayEnd, year
    #if(year == "1998"):
    #    weight = "A 3"


    # Not specified on diagnosis for this study
    isRelevant = bool(1)




    if(isRelevant and int(year) > 1997):        
        unweightedData.write(weight + "," + summary + "\n")
        #print(weight + "," + summary + "\n")

    fileFormat.close()

    if(counter % 1000 == 0):
        print counter
    counter = counter + 1
    
Questions.close()
unweightedData.close()
