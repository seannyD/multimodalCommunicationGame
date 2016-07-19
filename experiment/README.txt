

# Extract the data from the elan files, make csv files

cd processing
python3 eaf2csv_multimodal_B.py

# Extract measurements from 

cd ../analysis/R

R analyseData.R

R analyseTurnLevelData.R

R 