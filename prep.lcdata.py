#!/usr/bin/python

# You will need to ensure that the requests package is installed.
# Consult the python documentation for your particular setup.

import string
import sys
import requests
import argparse
from datetime import *

################################################################################

def stringParser(s, q="\"'"):
    # splits on , when not enquoted
    # does not attempt to handle escaped quotes
    in_quote = None

    i = 0
    w = ''
    while i < len(s):
        v = s[i]
        if in_quote == None and (v in q):
            in_quote = v
            i = i + 1
            continue

        elif v == in_quote:
            in_quote = None
            i = i + 1
            continue

        if in_quote != None:
            w = w + v
            
        elif in_quote == None and v == ',':
            yield w
            w = ''

        elif v in string.printable:
            w = w + v

        i += 1
    
    yield w


################################################################################

def downloadData():
    ## Download the data from lending club
    print "Downloading data from Lending Club.  As of Sept. 2013, typical file size is ~160 MB."
    url = 'https://www.lendingclub.com/fileDownload.action?file=LoanStatsNew.csv&type=gen'
    r = requests.get(url, stream=True)

    with open("lc.rawdata", "w") as fout:
        ld = 0
        chunk_size = 1048576
        for data in r.iter_content(chunk_size):
            ld += len(data)
            print "{0: >4} MB transferred".format( ld / chunk_size )
            fout.write(data)


################################################################################

if __name__ == "__main__":

    parser = argparse.ArgumentParser(description='Download and parse a Lending Club CSV dataset.')
    parser.add_argument("--usecsv", metavar='csvfile', 
                        help='skip downloading and parse the specified csv file')
    args = parser.parse_args()

    if args.usecsv:
        csvfile = args.usecsv
    else:
        print "Downloading the data from lending club"
        # downloadData()
        csvfile = "lc.rawdata"

    try:
        f = open(csvfile, "r")
        f.close()
        
    except IOError as e:
        print "File not found: csvfile {0} does not appear to exist".format(args.usecsv)
        sys.exit(1)

    ## Parse the downloaded data
    print "Parsing downloaded data"
    col_idx = None
    extract_col = ('id', 'funded_amnt', 'term', 'int_rate', 'installment', 
                   'grade', 'issue_d', 'total_pymnt', 'loan_status')
    
    line_ctr = 0
    fout = open("lc.csv", "w")

    with open("lc.rawdata", "r") as f:
        for line0 in f:
            line_ctr += 1

            if line_ctr == 1:
                # As of 2013-09-19, the first line of the Lending Club datafile contains
                # Notes offered by Prospectus (https://www.lendingclub.com/info/prospectus.action)
                continue

            line = line0.strip()

            if line_ctr == 2:
                # Line 2 contains the column headers
                col_names = [c for c in stringParser(line)]
                col_idx   = {e: col_names.index(e) for e in extract_col}
                header = "|".join( k for k in extract_col )
                fout.write("{0}\n".format(header))
                continue

            elif len(line) == 0:
                # As of 2013-09-19, Lending Club datafile contains blank lines, 
                # followed by "Loans that do not meet the credit policy"
                break

            else:
                col_vals = [c for c in stringParser(line)]
            
                try:
                    term = int(col_vals[ col_idx['term'] ].strip().replace(" months", ""))
                    col_vals[ col_idx['term'] ] = term
                    
                    int_rate = col_vals[ col_idx['int_rate'] ].strip().replace("%","")
                    col_vals[ col_idx['int_rate'] ] = int_rate

                    issue_d = datetime.strptime(col_vals[ col_idx['issue_d'] ], "%Y-%m-%d")
                    d_start = datetime.strptime("2008-03-04", "%Y-%m-%d")
                    d_end   = datetime.strptime("2010-08-15", "%Y-%m-%d")

                except IndexError as e:
                    sys.stderr.write("Error parsing line {0}:".format(line_ctr))
                    sys.stderr.write("-----{0}-----".format(line0))
                    
                    fout.close()
                    sys.exit(1)

            # The historical criteria
            if term == 36 and d_start <= issue_d <= d_end:
                outline = "|".join( str(col_vals[ col_idx[k] ]) for k in extract_col )
                fout.write("{0}\n".format(outline))
               
fout.close()
