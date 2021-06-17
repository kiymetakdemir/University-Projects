#2019400252
#Kiymet Akdemir

import argparse
import hashlib
import os

directories=[]

parser = argparse.ArgumentParser()              #parse inputs

parser.add_argument("-c",action="store_true")
parser.add_argument("-n",action="store_true")
parser.add_argument("-s",action="store_true")
group = parser.add_mutually_exclusive_group()   #-d and -f are mutually exclusive
group.add_argument("-d",action="store_true")
group.add_argument("-f",action="store_true")
args, directories = parser.parse_known_args()   #after taking flags, takes the rest of the input as directory list

if(not args.d and not args.f):                  #if there is no -d or -f, -f by default
    args.f=True
if(not args.c and not args.n):                  #if there is no -c, -n or -cn, default is -c
    args.c=True
if(not directories):                            #if there is no dir in command, default is current dir
    directories.append(os.getcwd())


def duplicates(initdict):               #from given dict, groups file/dir names according to their hash values and returns list of lists
    duplist=[]
    for key1,value1 in list(initdict.items()):
        found=False
        l={}
        for key2,value2 in list(initdict.items()):
            if value1==value2 and not key1==key2:
                l[key2]=value2
                found=True
        if found:
            l[key1]=value1
        if(l):
            l=sorted(l)
        if(l and not l in duplist):
            duplist.append(l)
        duplist=sorted(duplist)
    return duplist

def getcn(list1,list2):                #gets the intersection of -c and -n results to handle -cn option

    common=[]
    for l1 in list1:
        lis=[]
        for l2 in list2:
            lis=list(set(l1)&set(l2))
            lis=sorted(lis)
            if(len(lis)>1):
                common.append(lis)
    
    common=sorted(common)
    return common


def printlist(biglis):                  #prints list of lists by printing newline between each list
    for lis in biglis:
        for path1 in lis:
            print(path1)
        print('\n')

def prints(dup_lis,c_files,size_dict):  #matches each duplicate group with its size and prints in descending order
    dup_sizes={}
    k=0
    for l in dup_lis:
        dup_sizes[k]=size_dict[c_files[l[0]]]
        k=k+1
    hash_sizes={k: v for k, v in sorted(dup_sizes.items(), key=lambda item: item[1],reverse=True)}

    for k,sz in hash_sizes.items():
        for path1 in dup_lis[k]:
            print(path1,end='\t')
            print(sz)
        print('\n')

BLOCK_SIZE = 65536

c_files={}          #stores the filename and hash of file's content as pair
n_files={}          #stores the filename and hash of its name as pair
s_files={}          #stores the name and the size of the file as pair
c_dirs={}           #stores the directory name and hash of dir's content as pair
n_dirs={}           #stores the directory name and hash of its name as pair
s_dirs={}           #stores the name and the size of the directory as pair

for dir1 in directories:

    cfiledict={}    #name and the hash of content of the files in dir1 is collected in cfiledict, then inserted to c_files
    nfiledict={}    #name and size of directories in dir1 is collected in nfiledict, then inserted to n_files
    sfiledict={}    #name and size of files in dir1 is collected in sfiledict, then inserted to s_files

    for root,dirs,files in os.walk(dir1):
        for file in files:

            filepath=os.path.join(os.path.abspath(root),file)

            with open(filepath, 'rb') as f: #read as bytes
                fb = f.read(BLOCK_SIZE)     #read by block
                h=hashlib.sha256()
                if len(fb)==0:
                    h.update(b"")
                while len(fb) > 0:     #update the hash if still there is block
                    h.update(fb)
                    fb = f.read(BLOCK_SIZE)
            cfiledict[filepath]=h.hexdigest()

            filenm=hashlib.sha256(file.encode('utf-8')).hexdigest()
            nfiledict[filepath]=filenm

            sfiledict[cfiledict[filepath]]=os.stat(filepath).st_size


    cdirdict={}     #name and the hash of content of the directories in dir1 is collected in cdirdict, then inserted to c_dirs
    ndirdict={}     #name and hash of name of the directories in dir1 is collected in ndirdict, then inserted to n_dirs 
    sdirdict={}     #name and size of the directories in dir1 is collected in sdirdict, then inserted to s_dirs 

    for root,dirs,files in os.walk(dir1,topdown=False):
        crootdict={}
        nrootdict={}
        srootdict={}
        for file in files:
            filepath=os.path.join(os.path.abspath(root),file)
            crootdict[filepath]=cfiledict[filepath]
            nrootdict[filepath]=nfiledict[filepath]
            srootdict[cfiledict[filepath]]=sfiledict[cfiledict[filepath]]
        for dir in dirs:
            dirpath=os.path.join(os.path.abspath(root),dir)
            crootdict[dirpath]=cdirdict[dirpath]
            nrootdict[dirpath]=ndirdict[dirpath]
            srootdict[cdirdict[dirpath]]=sdirdict[cdirdict[dirpath]]
        crootdict= {k: v for k, v in sorted(crootdict.items(), key=lambda item: item[1])}
        nrootdict= {k: v for k, v in sorted(nrootdict.items(), key=lambda item: item[1])}

        dirvaluec=""
        for key in crootdict:
            dirvaluec=dirvaluec+crootdict[key]    #concatenate sorted hashes of content of the directory
        dirhash=hashlib.sha256(dirvaluec.encode('utf-8')).hexdigest()    #hash the concatenated hashes to get the value of the directory
        cdirdict[root]=dirhash

        dirvaluen=""
        for key in nrootdict:
            dirvaluen=dirvaluen+ nrootdict[key]
        dirname=""
        for c in root:
            if c is '/':
                dirname=""
            else:
                dirname=dirname+c
        dirvaluen=hashlib.sha256(dirname.encode('utf-8')).hexdigest()+dirvaluen
        dirhash=hashlib.sha256(dirvaluen.encode('utf-8')).hexdigest()
        ndirdict[root]=dirhash

        dirvalues=0
        for key in srootdict:
            dirvalues=dirvalues+srootdict[key]
        sdirdict[cdirdict[root]]=dirvalues
    
    c_files.update(cfiledict)           #all dir/file names and their hash values are collected in one dict 
    n_files.update(nfiledict)           #updated with each directory given in command
    c_dirs.update(cdirdict)
    n_dirs.update(ndirdict)
    s_files.update(sfiledict)
    s_dirs.update(sdirdict)

cf=duplicates(c_files)                  #cf,cd,nf,nd store the duplicate(according to name or content) lists of files/dirs
cd=duplicates(c_dirs)
nf=duplicates(n_files)
nd=duplicates(n_dirs)
cnf=getcn(cf,nf)                       #cnf stores the duplicates according to both content and names of the files
cnd=getcn(cd,nd)                         #cnd stores the duplicates according to both content and names of the directories


#print the lists according to given flags
if args.c and args.f and cf and not args.s and not args.n:      #-c -f is given -s is not given
    printlist(cf)

if args.c and args.d and cd and not args.s and not args.n:      #-c -d is given -s is not given
    printlist(cd)

if args.n and args.f and nf and not args.c:      #-n -f is given
    printlist(nf)

if args.n and args.d and nd and not args.c:      #-n -d is given
    printlist(nd)

if args.c and args.n and args.f and cnf and not args.s:         #-cn -f is given -s is not given
    printlist(cnf)

if args.c and args.n and args.d and cnd and not args.s:         #-cn -d is given -s is not given
    printlist(cnd)

if args.c and not args.n and args.f  and cf and args.s:         #-c -f -s is given
    prints(cf,c_files,s_files)

if args.c and not args.n and args.d  and cd and args.s:         #-c -d -s is given
    prints(cd,c_dirs,s_dirs)

if args.c and args.n and args.s and args.f and cnf:             #-cn -f -s is given
    prints(cnf,c_files,s_files)

if args.c and args.n and args.s and args.d and cnd:             #-cn -d -s is given
    prints(cnd,c_dirs,s_dirs)