import csv
sentences = []
with open('data/corpusCommentsStem.csv', 'rb') as csvfile:
    spamreader = csv.reader(csvfile, delimiter=',', quotechar='|')
    for row in spamreader:
        #import pdb; pdb.set_trace()
        sentences.append(row[0].split())

sentences  = filter(None, sentences)
str_list = []
for sentence in sentences:
    str_final = ""
    for word in sentence:
        str_final = str_final + " " + word
    str_list.append(str_final.strip())

myfile = open('data/corpusWithOneSpace.csv', 'wb')
wr = csv.writer(myfile, quoting=csv.QUOTE_NONE)

for sentence in str_list:
    aux_list = []
    aux_list.append(sentence)

    wr.writerow(aux_list)
