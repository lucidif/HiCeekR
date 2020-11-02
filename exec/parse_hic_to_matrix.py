#!/usr/bin/env python 
import sys
DEBUG = False
if (DEBUG):
    import time
### supported hic format
def nodup(line):
    ligation_product = {}
    positions = line.rstrip().split("\t")
    if (len(positions) > 0):
        ligation_product["chr1"] = positions[1]
        ligation_product["pos1"] = positions[2]
        ligation_product["chr2"] = positions[4]
        ligation_product["pos2"] = positions[5]
    
    return(ligation_product)

def maq(line):
    ligation_product = {}
    positions = line.rstrip().split("\t")
    if (len(positions) > 0):
        ligation_product["chr1"] = positions[1]
        ligation_product["pos1"] = positions[2]
        ligation_product["chr2"] = positions[5]
        ligation_product["pos2"] = positions[6]
    
    return(ligation_product)

def sam(line):
    ligation_product = {}
    positions = line.rstrip().split("\t")
    if(not(line.startswith("@") or len(positions) < 8)):
        ligation_product["chr1"] = positions[2]
        ligation_product["pos1"] = positions[3]
        ligation_product["chr2"] = positions[6]
        ligation_product["pos2"] = positions[7]
        if (ligation_product["chr2"] == "="):
            ligation_product["chr2"] = ligation_product["chr1"]
    return(ligation_product)

#### 
# get_ligation_product
# gets a hic line and a format
# returns the ligation product(hash table): chromosome1, position1, chromosome2, position2
####
def get_ligation_product(line, hic_format):
    f = globals()[hic_format]
    return(f(line))

def generate_key(chr1, chr2):
    return(chr1 + "_" + chr2)

### binary search of the bin that includes the give
def get_index(bins, pos, resolution):
    # first try by resolution
    if (resolution > 0):
        index = pos/resolution
        indices = [index, index-1, index+1]
        for i in indices:
            if (i < len(bins[0])) and (bins[0][i] <= pos) and (bins[1][i] >= pos):
                return(i)
    
    index = -1
    start = 0
    end = len(bins[1])
    while(start < end):
        mid = (start+end)/2
        if (pos > bins[1][mid]):
            start = mid + 1
        else:
            if (bins[0][mid] <= pos):
                return(mid)
            else:
                end = mid
    return(index)

############## main ###########################
# main input
genome_coord_file = sys.argv[1] 
hic_file = sys.argv[2]
hic_format = sys.argv[3]#"nodup" # nodup, sam, maq

# secondary input
output_prefix =sys.argv[4] 
header = int(sys.argv[5]) #False
inclusive = int(sys.argv[6])#False
verbose = int(sys.argv[7])#True
resolution = int(sys.argv[8])#1000000
delim = "\t" # ","
if (DEBUG):
    start = time.time()

######
if (verbose):
    print("creating data structures for matrix build...")

genome_coord_stream = open(genome_coord_file, 'r')
chrs = [] # names of chrs
genome_coords = {} # hash of genomic coordinates by chromosome
overlap = 0
if (inclusive):
    overlap = 1
if (header):
     genome_coord_stream.readline()

curr_chr = ""
# we first identify the chromsomes and generate a hash of coordinates
for line in genome_coord_stream:
    coords = line.split(delim)
    if (len(coords) > 1):
        chr = coords[0]
        if (chr != curr_chr):
            chrs.append(chr)
            genome_coords[chr] = [[],[]]
            curr_chr = chr
        genome_coords[chr][0].append(int(coords[1]))
        genome_coords[chr][1].append(int(coords[2])-overlap)
genome_coord_stream.close()

# then we generate keys for each ordered pairs 
# these will be our keys when extracting the corresponging contact matrix
m = {} # hash table of sparse pairwise contact matrices
n_chrs = len(chrs)
for i in range(0, n_chrs):
    for j in range(i, n_chrs):
        m[generate_key(chrs[i], chrs[j])] = {}

if (DEBUG):
    print(time.time() - start)
    start = time.time()

if (verbose):
    print("parsing hic file...")

# next we read the hic data and generate lists of sparse matrices
hic_stream = open(hic_file, 'r')
for line in hic_stream:
    ligation_product = get_ligation_product(line, hic_format)
    if (len(ligation_product) > 0):
        chr1 = ligation_product["chr1"]
        chr2 = ligation_product["chr2"]
        pos1 = ligation_product["pos1"]
        pos2 = ligation_product["pos2"]
        if (chr1 in chrs and chr2 in chrs):
            key = generate_key(chr1, chr2)
            if (not(m.has_key(key))):
                chr2 = ligation_product["chr1"]
                chr1 = ligation_product["chr2"]
                pos2 = ligation_product["pos1"]
                pos1 = ligation_product["pos2"]
            key =  generate_key(chr1, chr2)	
            # get the pairwise sparse contact matrix and update it 
            pairwise_m = m[key]
            i = get_index(genome_coords[chr1], int(pos1), resolution)
            if (i != -1):
                j = get_index(genome_coords[chr2], int(pos2), resolution)
                if (j != -1):
                    if (not(pairwise_m.has_key(i))):
                        pairwise_m[i] = {}
                    row = pairwise_m[i]
                    if (not(row.has_key(j))):
                        row[j] = 0
                    row[j] = row[j] + 1
                    if (chr1 == chr2) and (i != j):
                        if (not(pairwise_m.has_key(j))):
                            pairwise_m[j] = {}
                        row = pairwise_m[j]
                        if (not(row.has_key(i))):
                            row[i] = 0
                        row[i] = row[i] + 1

hic_stream.close()
if (DEBUG):
    print(time.time() - start)
    start = time.time()

# finally we write the contat matrices
if (verbose):
    print("writing contact matrices...")

for i in range(0, n_chrs):
    for j in range(i, n_chrs):
        key = generate_key(chrs[i], chrs[j])
        m_pairwise = m[key]
        nrow = len(genome_coords[chrs[i]][0])
        ncol = len(genome_coords[chrs[j]][0])
        
        output_file = output_prefix + "_" + key + ".txt"
        output_stream = open(output_file, "w")
        for r in range(nrow):
            row = m_pairwise.get(r)
            if (row == None): # all zeros
                output_stream.write("0\t"*(ncol-1) + "0\n")
            else:
                s_row = ""
                delim = "\t"
                for c in range(ncol):
                    contact_freq = row.get(c)
                    if (contact_freq == None):
                        contact_freq = 0
                    if (c == (ncol-1)):
                        delim = "\n"
                    s_row = s_row + str(contact_freq) + delim
                output_stream.write(s_row)
        output_stream.close()

if (DEBUG):
    print(time.time() - start)
    start = time.time()