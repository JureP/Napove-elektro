import os
import xlrd
import csv
import re

## okolje v katerem so podatki, in kamor se shranijo csv-ji
Okolje = 'C:/Users/Podlogar/Documents/Projekt Elektro/01_Priprava podatkov/Podatki/RNF - Meritve in ARSO podatki'
os.chdir(Okolje)

for exl in os.listdir(Okolje):
    ## preberi samo xlax datoteke v mapi
    if exl[-4:] == 'xlsx':
        vhodnoIme = exl
        kraj = re.search('(?<=ARSO-)(.*)(?=.xlsx)', vhodnoIme)
        ## ime ustvarjenega csv-ja
        imeCSV = kraj.group() + '_vreme.csv'
        print(imeCSV)
        ## branje xlsx-a
        with xlrd.open_workbook(vhodnoIme) as beri:
            ## ime sheeta v excelu
            beriStran = beri.sheet_by_name('RNF - meritve izbranih enot')
            ## pisanje csv-ja
            with open(imeCSV, 'w', newline='') as csvfile:
                wr = csv.writer(csvfile, delimiter=',', quotechar='|', quoting=csv.QUOTE_MINIMAL)
                ## zapise samo napoved vremena (z title) (DODAK SE DATUM IN URO!!)
                for rownum in range(1,beriStran.nrows):
                    wr.writerow(beriStran.row_values(rownum)[2:9])
