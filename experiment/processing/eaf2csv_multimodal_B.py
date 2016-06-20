# For multimodal experiment
# Take a set of eafs, and create csv files where each line is one signal, with data on where it came from etc.

# Requires pympi.   https://github.com/dopefishh/pympi


import pympi
import glob
import sys

turnmargin = 100



eaffolder = '../data/codedEaf/'
resultsfolder = '../data/csv'


def getTurnData(eaffile, player,turnS,turnE):
	resX = []
	for sigtype in ["Acoustic ","Visual "]:
	# get vocal signs
		signals = eaffile.get_annotation_data_between_times(sigtype+player, 
			turnS-turnmargin, 
			turnE+turnmargin)
		signals.sort()
		for sig_start, sig_end, sig_val in signals:
				resX.append([sigtype.strip(),sig_start,sig_end,sig_end-sig_start,'"'+sig_val+'"'])

	return(resX)

def findStartOfNextTrial(startT):
	for i in range(len(allTrialsStart)):
		if allTrialsStart[i] >= startT:
			if i < (len(allTrialsStart)-1):
				return(allTrialsStart[i+1])
			else:
			# last trial - just give an extra 5 seconds
				return(allTrialsStart[i]+5000)


def list2csv(l):
	return("\n".join([",".join([str(x) for x in line]) for line in l]))


resTitles = """
filename, 
dyadNumber,
game,
trial,
target,
choice,
correct,
signallingPlayer, 
role,
turnNumber,
turnType, 
turnStart, 
turnEnd, 
turnLength,
modality,
signalStart,
signalEnd,
signalLength,
signalType"""

for eafpath in glob.glob(eaffolder+'*.eaf'):

	resTitles = resTitles.replace("\n","").replace(" ","").split(",")

	res = []
	eaffile = pympi.Eaf(eafpath)
	filename = eafpath[(eafpath.rindex("/")+1):]
	
	dyadNumber = filename[:filename.index("_")]
	
	# get times of trial starts, so we can use them to segment the trials
	# (trial ends may be misleading, because they indicate when matcher presses a button -
	#   there could be signals after this, but signals should not occur before director
	#   has been shown target)
	
	allTrials = eaffile.get_annotation_data_for_tier("Trials 1") + eaffile.get_annotation_data_for_tier("Trials 2")
	allTrials.sort()
	allTrialsStart = [x[0] for x in allTrials]

	# director
	for player in ["1","2"]:
		
		trials = eaffile.get_annotation_data_for_tier("Trials "+player)
		
		trials = sorted(trials, key = lambda x: x[0])
	
		for trialIndex in range(len(trials)):
			trialS,trialE,trialV = trials[trialIndex]
			nextTrialStart = findStartOfNextTrial(trialS)
			
			trialBits = trialV.split(" ")
			trialGame = trialBits[0][2:]
			trialNumber = trialBits[1][2:]
			trialTarget = trialBits[2][2:]
			trialChoice = trialBits[-1][2:]
			trialCorrect = {True:"Correct", False:"Incorrect"}[trialTarget==trialChoice]
			
			for role in ["Director","Matcher"]:
				
				signallingPlayer = "1"
				if (role=="Matcher" and player=='1') or (role=="Director" and player=='2'):
					signallingPlayer = "2"
				
				turns = eaffile.get_annotation_data_between_times(
					"Part "+signallingPlayer, 
					trialS-turnmargin,
					nextTrialStart)
				turns.sort()
				turnCount = 1
				for turnS, turnE, turnV in turns:		
			
					baseString = [
							filename, 
							dyadNumber,
							trialGame,
							trialNumber,
							trialTarget,
							trialChoice,
							trialCorrect,
							signallingPlayer, 
							role,
							turnCount,
							'"'+turnV+'"', 
							turnS, 
							turnE, 
							turnE-turnS,
							] 
					signals = getTurnData(eaffile,signallingPlayer,turnS,turnE)
					for x in signals:
						res.append(baseString + x )
							

					turnCount += 1
			

	
	res = sorted(res, key=lambda x: int(x[resTitles.index("signalStart")]))
	res = list2csv([resTitles]+res)
				
	csvfilename = "/"+filename.replace(".eaf",".csv")				 
	o = open(resultsfolder + csvfilename,'w')
	o.write(res)
	o.close()