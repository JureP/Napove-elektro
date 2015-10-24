import os
import xlrd
import csv
import re
import datetime

## okolje v katerem so podatki proizvdonji, vremenske napovedi in kamor se shranijo csv-ji
Okolje = 'C:/Users/Podlogar/Documents/Projekt Elektro/01_Priprava podatkov/Podatki/RNF - Meritve in ARSO podatki'
## okolje kjer je xlsx s podatki o elektrarnah
OkoljeElektrarne = 'C:/Users/Podlogar/Documents/Projekt Elektro/01_Priprava podatkov/Podatki'
## okolje v kamor se shrani realizacija proizvodnje elektrarn
OkoljeProizvodnja = 'C:/Users/Podlogar/Documents/Projekt Elektro/01_Priprava podatkov/Podatki/Proizvodnja'
os.chdir(Okolje)

## vremenska napoved
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

## realizacija proizvodnje

## okolje kjer je xlsx s podatki o elektrarnah
os.chdir(OkoljeElektrarne)
## odpiranje xlsx s podatki o elektrarnah
with xlrd.open_workbook('RNF - Osnovni podatki izbranih proizvodnih enot.xlsx') as elek:
    ## podatki o elektrarnah
    elektrarne = elek.sheet_by_name('RNF - izbrane enote')
    ## list id stevilk elektrarn
    iskanjeLege = elektrarne.col_values(0)
    for exl in os.listdir(Okolje):
        os.chdir(Okolje)
        ## prebere samo xlax datoteke v mapi
        if exl[-4:] == 'xlsx':
            vhodnoIme = exl
            kraj = re.search('(?<=ARSO-)(.*)(?=.xlsx)', vhodnoIme)
            print(vhodnoIme)
            ## branje xlsx-a
            with xlrd.open_workbook(vhodnoIme) as beri:
                ## ime sheeta v excelu
                beriStran = beri.sheet_by_name('RNF - meritve izbranih enot')
                ## gre cez vse elektrarne na podrocju
                for idPoz in range(9,beriStran.ncols):
                    ## id elektrarne
                    id = beriStran.row_values(0)[idPoz]
                    ## poisce vrstico v kateri so podatki o elekt
                    lega = iskanjeLege.index(int(id))
                    ## prebere podatke o elekt
                    elektInfo = elektrarne.row_values(lega)
                    ## izbere relevantne podatke o elekt
                    postNum = elektInfo[3]
                    elektTip = elektInfo[-1]
                    elektMoc = elektInfo[-3]
                    ## sestava imena za podatke proizvodnje elektrarne:
                    ## [id] stevilka elektrarne _[K] kraj _[P] postna stevilka _[T] tip _[M] moc
                    imePodElekt = '[id]{}_[K]{}_[P]{}_[T]{}_[M]{}.csv'.\
                                    format(int(id), kraj.group(), postNum, elektTip, elektMoc)
                    os.chdir(OkoljeProizvodnja)
                    ## pisanje csv-ja realizacije proizvodnje elektrarne z id:
                    with open(imePodElekt, 'w', newline='') as ProizvodnjaCSV:
                        wr = csv.writer(ProizvodnjaCSV, delimiter=',', quotechar='|', quoting=csv.QUOTE_MINIMAL)
                        ## zapis naslovov vrstic
                        wr.writerow(['datum', 'cas', 'realizacija'])
                        for i in range(2, beriStran.nrows):
                            ## zapis datuma, ure in realizacije v vsako vrstico
                            datumUra = xlrd.xldate_as_tuple(beriStran.row_values(i)[0], beri.datemode)
                            vrstica = [datetime.date(*datumUra[0:3]), datetime.time(*datumUra[3:6]), beriStran.row_values(i)[idPoz]]
                            wr.writerow(vrstica)