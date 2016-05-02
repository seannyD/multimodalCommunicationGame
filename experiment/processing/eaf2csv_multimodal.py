# For multimodal experiment
# Take a set of eafs, and create csv files where each line is one signal, with data on where it came from etc.

# Requires pympi.   https://github.com/dopefishh/pympi


import pympi
import glob
import sys

turnmargin = 100



eaffolder = '../data/eaf/'
resultsfolder = '../data/csv'

for eafpath in glob.glob(eaffolder+'*.eaf'):

	res = "file,player,turnID,turnType,turnStart,turnEnd,turnLength,modality,signalStart,signalEnd,signalLength, signalType\n"

	eaffile = pympi.Eaf(eafpath)
	filename = eafpath[eafpath.rindex("/"):]
	
	turnID = 1
	
	for player in ["1","2"]:
		for turns, turne, turnv in eaffile.get_annotation_data_for_tier("Player "+player):
			turnID += 1
			for sigtype in ["Vocal Signs ","Visual Signs "]:
			# get vocal signs
				signals = eaffile.get_annotation_data_between_times(sigtype+player, turns-turnmargin, turne+turnmargin)
				for sig_start, sig_end, sig_val in signals:
					res += ",".join([str(x) for x in [
							filename, 
							player, 
							turnID,
							'"'+turnv+'"', 
							turns, 
							turne, 
							turne-turns,
							sigtype,
							sig_start,
							sig_end,
							sig_end - sig_start,
							'"'+sig_val+'"'
							]]) + "\n"
	csvfilename = filename.replace(".eaf",".csv")				 
	o = open(resultsfolder + csvfilename,'w')
	o.write(res)
	o.close()