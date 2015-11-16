import csv

reader=csv.reader(open('data/subjectivity.csv', 'r'), delimiter=',')
writer=csv.writer(open('data/subjectivitynorepeated.csv', 'w'), delimiter=',')
entries = list()
for row in reader:
   key = (row) # instead of just the last name
   # import pdb; pdb.set_trace()

   if key not in entries:
      writer.writerow(row)
      entries.append(key)
