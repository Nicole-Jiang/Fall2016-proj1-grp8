import urllib
import zipfile
import os
for i in range(1996,2015):
    file_path = 'http://www2.census.gov/programs-surveys/acs/data/pums/' + str(i) + '/1-Year/csv_pus.zip'
    to_path = '/Users/pengfeiwang/Desktop/Fall 1 /data/'
    file_name = '/Users/pengfeiwang/Desktop/Fall 1 /data/' + str(i) + '.zip'
    README_path = '/Users/pengfeiwang/Desktop/Fall 1 /data/ACS' + str(i) + '_PUMS_README.pdf'
    
    print "start downloading year " + str(i) + " ..."
    urllib.urlretrieve (file_path, file_name)
    print "start unzipping year " + str(i) + " ..."
    zip = zipfile.ZipFile(file_name)
    zip.extractall(path = to_path)
    print "remove zip ..."
    os.remove(file_name)
    os.remove(README_path)
    print str(i) + " finished!"