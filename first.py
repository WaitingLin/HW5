import csv as csv 
import numpy as np

# Open up the csv file in to a Python object
csv_file_object = csv.reader(open('train.csv', 'rb')) 
header = csv_file_object.next()  # The next() command just skips the 
                                 # first line which is a header
data=[]                          # Create a variable called 'data'.
for row in csv_file_object:      # Run through each row in the csv file,
    data.append(row)             # adding each row to the data variable
data = np.array(data) 	         # Then convert from a list to an array
								 # Be aware that each item is currently
                                 # a string in this format
							
							
							
# The size() function counts how many elements are in
# in the array and sum() (as you would expects) sums up
# the elements in the array.

number_passengers = np.size(data[0::,1].astype(np.float))
number_survived = np.sum(data[0::,1].astype(np.float))
proportion_survivors = number_survived / number_passengers


fare_ceiling = 40

parch_ceiling = 6#23

sibsp_ceiling = 6#24

# fare:39 parch:5.9
data[ data[0::,9].astype(np.float) >= fare_ceiling, 9 ] = fare_ceiling - 1.0
data[ data[0::,7].astype(np.float) >= parch_ceiling, 7 ] = parch_ceiling - 0.1#23
data[ data[0::,6].astype(np.float) >= sibsp_ceiling, 6 ] = sibsp_ceiling - 0.1#24

fare_bracket_size = 10
number_of_price_brackets = fare_ceiling / fare_bracket_size

parch_bracket_size = 1
number_of_parch_brackets = parch_ceiling / parch_bracket_size#23

sibsp_bracket_size = 1
number_of_sibsp_brackets = sibsp_ceiling / sibsp_bracket_size#24

number_of_classes = len(np.unique(data[0::,2])) 


# Initialize the survival table with all zeros 2x3x5
survival_table = np.zeros((2, number_of_classes, number_of_price_brackets, number_of_parch_brackets, number_of_sibsp_brackets))#23




for i in xrange(number_of_classes):       #loop through each class
 for j in xrange(number_of_price_brackets):   #loop through each price bin
   for k in xrange(number_of_parch_brackets): #23
	for l in xrange(number_of_sibsp_brackets): #24
	 women_only_stats = data[(data[0::,4] == "female")\
	 &(data[0::,2].astype(np.float) == i+1)\
	 &(data[0:,9].astype(np.float) >= j*fare_bracket_size)\
	 &(data[0:,9].astype(np.float) < (j+1)*fare_bracket_size)\
	 &(data[0:,7].astype(np.float) >= k*parch_bracket_size)\
	 &(data[0:,7].astype(np.float) < (k+1)*parch_bracket_size)\
	 &(data[0:,6].astype(np.float) >= l*sibsp_bracket_size)\
	 &(data[0:,6].astype(np.float) < (l+1)*sibsp_bracket_size)\
	 , 1] 
	 men_only_stats = data[(data[0::,4] != "female")\
	 &(data[0::,2].astype(np.float)== i+1)\
	 &(data[0:,9].astype(np.float) >= j*fare_bracket_size)\
	 &(data[0:,9].astype(np.float) < (j+1)*fare_bracket_size)\
	 &(data[0:,6].astype(np.float) >= k*parch_bracket_size)\
	 &(data[0:,6].astype(np.float) < (k+1)*parch_bracket_size)\
	 &(data[0:,6].astype(np.float) >= l*sibsp_bracket_size)\
	 &(data[0:,6].astype(np.float) < (l+1)*sibsp_bracket_size)\
	 , 1]  
	 survival_table[0,i,j,k,l] = np.mean(women_only_stats.astype(np.float))#23
	 survival_table[1,i,j,k,l] = np.mean(men_only_stats.astype(np.float))#23
	 survival_table[ survival_table != survival_table ] = 0.

survival_table[ survival_table < 0.5 ] = 0
survival_table[ survival_table >= 0.5 ] = 1 
print survival_table

#write
test_file = open('test.csv', 'rb')
test_file_object = csv.reader(test_file)
header = test_file_object.next()
predictions_file = open("genderclasspurchmodel.csv", "wb")
p = csv.writer(predictions_file)
p.writerow(["PassengerId", "Survived"])

for row in test_file_object:                  
	for j in xrange(number_of_price_brackets):  # For each passenger we
		try:                                      # Some passengers have no
			row[8] = float(row[8])                  # a float
		except:                                   # If fails: no data, so 
			bin_fare = 3 - float(row[1])            # bin the fare according Pclass
			break                                   # Break from the loop
		if row[8] > fare_ceiling:
			bin_fare = number_of_price_brackets-1   # If so set to highest bin
			break                                   # And then break loop
		if row[8] >= j * fare_bracket_size\
			and row[8] < \
			(j+1) * fare_bracket_size:
			bin_fare = j
			break
	#23
	for k in xrange(number_of_parch_brackets):
		row[6] = float(row[6])  
		if row[6] > parch_ceiling:
			bin_parch = number_of_parch_brackets-0.1
			break 
		if row[6] >= k * parch_bracket_size\
			and row[6] < \
			(k+1) * parch_bracket_size:
			bin_parch = k
			break
	#24
	for l in xrange(number_of_sibsp_brackets):
		row[5] = float(row[5])  
		if row[5] > sibsp_ceiling:
			bin_sibsp = number_of_sibsp_brackets-0.1
			break 
		if row[5] >= k * sibsp_bracket_size\
			and row[5] < \
			(k+1) * sibsp_bracket_size:
			bin_sibsp = l
			break
	#initial all age
	if row[4] == '':
		row[4] = '87'
	row[4] = float(row[4])
	if row[3] == 'female':                             #If the passenger is female
			p.writerow([row[0], "%d" % \
				int(survival_table[0, float(row[1])-1, bin_fare , bin_parch , bin_sibsp])])#23
	else:   
			p.writerow([row[0], "%d" % \
				int(survival_table[1, float(row[1])-1, bin_fare , bin_parch , bin_sibsp])])#23

# Close out the files.
test_file.close() 
predictions_file.close()