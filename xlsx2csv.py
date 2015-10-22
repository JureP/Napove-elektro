import os
import xlrd
import csv
import re
import datetime

## okolje v katerem so podatki, in kamor se shranijo csv-ji
Okolje = 'C:/Users/Podlogar/Documents/Projekt Elektro/01_Priprava podatkov/Podatki/RNF - Meritve in ARSO podatki'
os.chdir(Okolje)

for exl in os.listdir(Okolje):
    ## preberi samo xlax datoteke v mapi
    if exl[-4:] == 'xlsx':
        vhodnoIme = exl
        kraj = re.search('(?<=ARSO-)(.*)(?=.xlsx)', vhodnoIme)
        ## ime ustvarjenega csv-ja
        imeCSV = 'vreme_' + kraj.group() + '.csv'
        print(imeCSV)
        ## branje xlsx-a
        with xlrd.open_workbook(vhodnoIme) as beri:
            ## ime sheeta v excelu
            beriStran = beri.sheet_by_name('RNF - meritve izbranih enot')
            ## pisanje csv-ja
            with open(imeCSV, 'w', newline='') as csvfile:
                wr = csv.writer(csvfile, delimiter=',', quotechar='|', quoting=csv.QUOTE_MINIMAL)
                ## zapis naslovov vrstic
                wr.writerow(beriStran.row_values(1)[0:9])
                ## zapise samo napoved vremena
                for rownum in range(2,beriStran.nrows):
                    ## datum in ura napovedi
                    datumUra = xlrd.xldate_as_tuple(beriStran.row_values(rownum)[0], beri.datemode)
                    vrstica = [datetime.date(*datumUra[0:3]), datetime.time(*datumUra[3:6])] + beriStran.row_values(rownum)[2:9]
                    wr.writerow(vrstica)



