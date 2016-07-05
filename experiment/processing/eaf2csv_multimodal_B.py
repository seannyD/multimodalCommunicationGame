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

def inside(x,start,end):
	return x > start and x < end

def overlapBetweenIntervals(t1,t2):
	# sort by which one starts first
	t1,t2 = sorted([t1,t2])
	
	# if they don't overlap
	if t1[1] > t2[0]:
		return(0)
	# otherwise, return end of t1 - start of t2
	return(t1[1] - t2[0])
	

def findTurnsThatStartOrEndBetween(eaffile, tier, tstart, tend):
	# find turns that either start or end between the two timepoints
	candTrials = [x for x in eaffile.get_annotation_data_for_tier(tier) if inside(x[0],tstart,tend) or inside(x[1],tstart,tend)]
	
	return(candTrials)
	
# 	if len(candTrials)==0:
# 		return(None)
# 			
# 	if len(candTrials)==1:
# 		return(candTrials[0])
# 
# 	if len(candTrials)>1:
# 		# work out proportion of overlaps
# 		overlaps = [overlapBetweenIntervals(cand, [tstart,tend,'']) for cand in candTrials]
# 		
# 		# find candidate with maximum overlap
# 		maxOverlap = overlaps[0]
# 		overlapId = 0
# 		for i in range(len(overlaps)):
# 			if overlaps[i] > maxOverlap:
# 				maxOverlap = overlaps[i]
# 				overlapId = i
# 		return(candTrials[i])
		
		
resTitles = """
filename, 
dyadNumber,
condition,
game,
trial,
target,
choice,
correct,
trialStart,
trialEnd,
trialValue,
startOfNextTrial,
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

resTitles = resTitles.replace("\n","").replace(" ","").split(",")

for eafpath in glob.glob(eaffolder+'*.eaf'):

	
	print(eafpath)
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
	
	stimOrder = eaffile.get_annotation_data_for_tier("Stimuli")
	stimOrder.sort()
	


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
			
			condition = stimOrder[0][2]
			conditionS = stimOrder[0][0]
			# count trials within stimuli if it starts within 10 seconds
			#  of the second block annotation start
			# (but some files only have one block)
			if len(stimOrder)>1:
				if trialS> stimOrder[1][0]-25000:
					condition = stimOrder[1][2]
					conditionS = stimOrder[1][0]
			
			for role in ["Director","Matcher"]:
				
				signallingPlayer = "2"
				if (role=="Matcher" and player=='1') or (role=="Director" and player=='2'):
					signallingPlayer = "1"
				
				#turns = eaffile.get_annotation_data_between_times(
				#	"Part "+signallingPlayer, 
				#	trialS-turnmargin,
				#	nextTrialStart)
				
				# find turns that start or end within the trial time
				
				turns = findTurnsThatStartOrEndBetween(
					eaffile,
					"Part "+signallingPlayer, 
					trialS-turnmargin,
					trialE+turnmargin)
				
				
				turns.sort()
				
				
				turnCount = 1
				for turnS, turnE, turnV in turns:		
			
					baseString = [
							filename, 
							dyadNumber,
							condition,
							trialGame,
							trialNumber,
							trialTarget,
							trialChoice,
							trialCorrect,
							trialS,
							trialE,
							trialV,
							nextTrialStart,
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

						if x[1] >= (conditionS -200):
							res.append(baseString + x )
							

					turnCount += 1
			

	
	res = sorted(res, key=lambda x: int(x[resTitles.index("signalStart")]))
	res = list2csv([resTitles]+res)
				
	csvfilename = "/"+filename.replace(".eaf",".csv")				 
	o = open(resultsfolder + csvfilename,'w')
	o.write(res)
	o.close()