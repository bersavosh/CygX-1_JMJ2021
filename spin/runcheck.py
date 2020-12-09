#!/usr/bin/python

import os,argparse

def get_args():
    dp=[]
    dp.append('python runcheck.py [options]')
    dp.append('For example: python runcheck.py -p par.txt -f fit_result.log -t ftab.dat')
    dp.append('')
    dp.append('version: v.02 (2020.11)')
    dp.append('author: Zhao Xueshan <xszhao@nao.cas.cn>')
    dp.append(' ')
    usage=""
    for d in dp: usage += d+'\n'
    parser = argparse.ArgumentParser(usage=usage) 
    parser.add_argument('-t','--tname',dest='tname',default='ftab.dat',help='name of the f-table')
    parser.add_argument('-f','--fname',dest='fname',default='fit_result.log',help='name of the log file')

    parser.add_argument('-p','--parameter',dest='parameter_sets_file',default='',help='A file that contains \'mass inclination distance\'')
    args = parser.parse_args()
    return args



def runcheck():
    args = get_args()
    tname = args.tname
    fname = args.fname
    mypspacefile = args.parameter_sets_file
    with open(mypspacefile,'r') as f:
        data = f.read().splitlines()

    cts_t = 0
    cts_f = 0

    if os.path.exists('need_ftables.txt'):
        os.remove('need_ftables.txt')
    if os.path.exists('run_fit.txt'):
        os.remove('run_fit.txt')


    for index in range(len(data)):

        mass = float(data[index].split(' ')[0])
        incl = float(data[index].split(' ')[1])
        dis = float(data[index].split(' ')[2])
        dirname = 'dir_%5.4f_%5.4f_%5.4f'%(mass,incl,dis)

        if os.path.exists(dirname+'/'+tname):
            if os.path.exists(dirname+'/'+fname):
                continue
            else:
                cts_f += 1
                with open('run_fit.txt','a') as f:
                    f.write('%5.4f %5.4f %5.4f\n'%(mass,incl,dis))
        else:
            cts_t += 1
            with open('need_ftables.txt','a') as f:
                f.write('%5.4f %5.4f %5.4f\n'%(mass,incl,dis))
    return cts_t,cts_f

cts_t,cts_f = runcheck()
print(cts_f,' can run fit process!')
print(cts_t,' need to generate f-table!')