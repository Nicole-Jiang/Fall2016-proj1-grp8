import urllib
import zipfile
import os

to_path = '/Users/pengfeiwang/Desktop/Fall1/data/'
year = 2013  # for year in range(2000, 2015)

def scrapping(i, to_path):
    file_name = to_path + str(i) + '.zip'
    README_path = to_path + 'ACS' + str(i) + '_PUMS_README.pdf'    
    if 2015 > i >= 2007:
        file_path = 'http://www2.census.gov/programs-surveys/acs/data/pums/' + str(i) + '/1-Year/csv_pus.zip' 
    else:
        file_path = 'http://www2.census.gov/programs-surveys/acs/data/pums/' + str(i) + '/csv_pus.zip'
    print "---Start downloading year " + str(i) + " ---"
    urllib.urlretrieve (file_path, file_name)
    print "---Start unzipping year " + str(i) + " ---"
    zip = zipfile.ZipFile(file_name)
    zip.extractall(path = to_path)
    print "---Remove zip ---"
    os.remove(file_name)
    try:
        os.remove(README_path)
    except:
        print "---Year " + str(i) + " has no readme file---"
        
    print "Year " + str(i) + " finished!"