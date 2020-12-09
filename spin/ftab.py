
#!/usr/bin/python

import sys
import numpy as np
import math
import os
import shutil
import argparse 
import time
from datetime import datetime



def get_args():
	dp=[]
	dp.append('python ftab.py [options]')
	dp.append('For example: python ftab.py -r P021400205603_LE_rsp_g0_0-94.fits -b bhspec_spin_0.1.fits -n 0.5 -o ftab.dat -l 1.0 -u 12.0 -f par.txt')
	dp.append('')
	dp.append('version: v.02 (2020.11)')
	dp.append('author: Zhao Xueshan <xszhao@nao.cas.cn>')
	dp.append('')
	dp.append('This script is aiming at: ')
	dp.append('Computing lookup tables for the spectral hardening fator fcol')
	usage=""
	for d in dp: usage += d+'\n'
	parser = argparse.ArgumentParser(usage=usage) 
	parser.add_argument('-r','--response',dest='response',default='',help='specify response file')
	parser.add_argument('-b','--bhspec',dest='bhspec',default='',help='specify bhspec file')
	parser.add_argument('-n','--nh',dest='nh',default='',type=float,help='hydrogen column density in unit of 10^22cm^(-2)')
	parser.add_argument('-o','--outfile',dest='outfile',default='',help='alternative output file')
	parser.add_argument('-l','--low',dest='lower',default='2.0',type=float,help='the lower limit of energy')
	parser.add_argument('-u','--up',dest='upper',default='10.0',type=float,help='the upper limit of energy')
	parser.add_argument('-f','--file',dest='inputfile',default='par.txt',help='input file including three dynamical parameters M, i and D')
	args = parser.parse_args()
	return args



def check_response():
	args = get_args()
	# 1. Check response file
	response = args.response
	if len(response) != 0:
		if not os.path.isfile(response):
			print('Error: response file \'',response,'\' does not exist!')    
			sys.exit()
	else:
		print('Error: please input response file!')
		sys.exit()
	return response

def check_bhspec():
	# 2. Check bhspec file
	args = get_args()
	bhspec = args.bhspec
	if len(bhspec) != 0:
		if not os.path.isfile(bhspec):  
			print('Error: bhspec file \'',bhspec,'\' does not exist!')
			sys.exit()
	else:
		print('Error: please input bhspec file!')
		sys.exit()
	return bhspec

def check_nh():
    # 3. Check nh
	args = get_args()
	nh = args.nh
	if len(str(nh)) == 0:
		print('Error: please input nh!')
		sys.exit()
	return nh
# 4. Check output file
def check_outfile():	
	args = get_args()
	outfile = args.outfile
	if len(outfile) == 0:
		outfile = 'ftab.dat'
		print('Warning: the output file will be named to ftab.dat')
	return outfile
# 5. Check energy band
def check_ub():
	args = get_args()
	ub = args.lower
	return ub
def check_uu():
	args = get_args()
	uu = args.upper
	return uu

def check_inputfile():
	args = get_args()
	# 6. Check input parameters file
	inputfile = args.inputfile
	if not os.path.isfile(inputfile):
		print('Error: input parameters file \'',inputfile,'\' does not exist!')    
		sys.exit()
	return inputfile


def fake(par,a,l):
	with open('%5.4f_%5.4f.xcm'%(a,l),'w') as f:
		
		f.write('model phabs(atable{'+par.bhspec+'})\n')
		f.write('%1.5f  -1\n'%par.nh)
		f.write('%1.5f  -1\n'%par.lgm)
		f.write('%1.5f  -1\n'%l)
		f.write('%1.5f  -1\n'%par.cosi)
		f.write('%1.5f  -1\n'%a)
		f.write('%1.5f  -1\n'%par.norm)
		f.write('fakeit none\n')
		f.write(par.response+'\n'+'\n')
		f.write('y\n')  #Use counting statistics in creating fake data?
		f.write('n\n')  #Input optional fake file prefix
		f.write('%5.4f_%5.4f.fak\n'%(a,l))
		f.write('%f,1\n'%par.exptime)
		f.write('log %5.4f_%5.4f.log\n'%(a,l))
		f.write('show all\n'+'log none\n'+'exit\n')	
	return
	
def get_mpr(a,l):
	with open('%5.4f_%5.4f.log'%(a,l),'r') as f:
		data = f.readlines()
		for i in range(len(data)):
			if 'Model predicted rate:' in data[i]:
				mpr = float(data[i].split()[-1])
	return mpr




def make_ftable(par):
	fd_arr=[]
	dirname='dir_%5.4f_%5.4f_%5.4f'%(par.mass,par.incl,par.dis)
	if os.path.exists(dirname):
		shutil.rmtree(dirname)
	os.mkdir(dirname)
	create_link(root_path,dirname,par.response)
	create_link(root_path,dirname,par.bhspec)
	os.chdir(dirname)
	for i in range(len(par.arr_a)):
		for j in range(len(par.arr_l)):	
			l = par.arr_l[j]
			a = par.arr_a[i]
			print(l,a)
			fake(par,a,l)
			os.system('xspec - %5.4f_%5.4f.xcm'%(a,l))
			par.exptime = 1e6/get_mpr(a,l)
			fake(par,a,l)
			os.system('xspec - %5.4f_%5.4f.xcm'%(a,l))
			args = 'grppha %5.4f_%5.4f.fak '%(a,l)+'%5.4f_%5.4f.g25 chatter=0 comm=\'group min 25 & exit\''%(a,l)
			if os.path.exists('%5.4f_%5.4f.g25'%(a,l)):
				os.remove('%5.4f_%5.4f.g25'%(a,l))
			os.system(args)
			fitscript(l,a,par)
			os.system('xspec - %5.4f_%5.4f_fit.xcm'%(a,l))
			fd = findpars(a,l)
			fd_arr.append(fd)
			os.remove('%5.4f_%5.4f.xcm'%(a,l))
			os.remove('%5.4f_%5.4f_fit.xcm'%(a,l))
			os.remove('%5.4f_%5.4f.log'%(a,l))
			os.remove('%5.4f_%5.4f_fit.log'%(a,l))
			os.remove('%5.4f_%5.4f.g25'%(a,l))
			os.remove('%5.4f_%5.4f.fak'%(a,l))
			with open('tab.txt','a') as f:
				f.write('%1.5f %1.5f %1.5f\n'%(a,l,fd))
	os.remove(par.response)		
	os.remove(par.bhspec)	
	write_ftab(fd_arr,par)
	os.chdir('../')
	return


def fitscript(l,a,par):
	with open('%5.4f_%5.4f_fit.xcm'%(a,l),'w') as f:
		f.write('data %5.4f_%5.4f.g25\n'%(a,l))
		f.write('query yes\n'+'ig bad\n')
		f.write('ig **-%1.5f %1.5f-**\n'%(par.ub,par.uu))
		f.write('mo phabs(kerrbb)\n')
		f.write('%1.5f  -1\n'%par.nh)
		f.write('0 -1\n') #eta
		f.write('%1.5f  -1\n'%a)
		f.write('%1.5f  -1\n'%par.incl)
		f.write('%1.5f  -1\n'%par.mass)
		f.write('\n') #Mdd
		f.write('%1.5f  -1\n'%par.dis)
		f.write('1.7 0.01\n') #fd
		f.write('0 -1\n1 -1\n1 -1\nrenorm\nfit\n')
		f.write('log %5.4f_%5.4f_fit.log\n'%(a,l))
		f.write('sho all\nlog none\nexit\n')
	return
		
def findpars(a,l):
	with open('%5.4f_%5.4f_fit.log'%(a,l),'r') as f:
		data = f.readlines()
		for i in range(len(data)):
			if 'kerrbb     hd' in data[i]:
				fd = float(data[i].split()[5])
	return fd
		
def write_ftab(fd_arr,par):
	with open(par.outfile,'w') as f:
		f.write(' 10 8\n    ')
		for z in range(6):
			f.write('%1.6f   '%par.arr_a[z])
		f.write('\n    ')
		for z in range(2):
			f.write('%1.6f   '%par.arr_a[z+7])
		f.write('\n   ')
		for z in range(6):
			f.write('%1.6f   '%par.arr_l[z])
		f.write('\n   ')
		f.write('%1.6f   '%par.arr_l[7])
		f.write('\n')
		for x in range(11):
			f.write('    %1.6f' %fd_arr[x*7])
			for y in range(6):
				f.write('   %1.6f' %fd_arr[x*7+y+1])
			f.write('\n')
		f.write('    %1.6f' %fd_arr[77])
		for i in range(2):
			f.write('   %1.6f' %fd_arr[i+78]) 
		f.write('\n') 
	return


def create_link(root_path,dirname,linkfile):
	src=root_path+'/'+linkfile
	dst=root_path+'/'+dirname+'/'+linkfile
	os.symlink(src,dst)
	return


def error_analysis(data,par):
	for i in range(len(data)):
		par.mass = float(data[i].split(' ')[0])
		par.incl = float(data[i].split(' ')[1])
		par.dis  = float(data[i].split(' ')[2])
		target=os.path.join('./dir_%5.4f_%5.4f_%5.4f'%(par.mass,par.incl,par.dis),par.outfile)
		print(target)
		if os.path.exists(target):
			continue
		else:
			make_ftable(par)
	return




##########################################
##########################################
##########################################
class par:
	nh = check_nh()
	response = check_response()
	bhspec = check_bhspec()
	outfile = check_outfile()
	mass = 14.8 # black hole mass
	incl = 27.47 # disk inclination
	dis = 1.86 # source distance
	lgm = math.log(mass)/math.log(10) 
	cosi = math.cos(incl*np.pi/180)
	norm = (10/dis)**2 #(10/D)^2
	exptime = 100
	ub = check_ub() #the bottom limit of the energy
	uu = check_uu() #the upper limit of the energy
	if bhspec == 'bhspec_spin_0.01.fits':
		alpha = 0.01
		arr_a = np.arange(-0.9,1.0,0.2)
		arr_l = np.arange(-2,-0.5,0.2)
	if bhspec == 'bhspec_spin_0.1.fits':
		alpha = 0.1
		arr_a = np.arange(-1.,0.9,0.2)
		arr_l = np.arange(-2,-0.5,0.2)


##########################################
##########################################
##########################################

root_path=os.getcwd()
inputfile = check_inputfile()
print('##########################################')
print('##########################################')
print('##########################################')
print('The root path is',root_path)
print('Your response file is',par.response)
print('Your BHSPEC file is',par.bhspec)
print('The lower limit of energy is',par.ub,' keV')
print('The upper limit of energy is',par.uu,' keV')
print('The output file is',par.outfile)
print('logL/L_Edd=',par.arr_l)
print('a*=',par.arr_a)
print('The viscosity parameter alpha=',par.alpha)
print('The hydrogen column density nH=',par.nh,'x 10^22cm^(-2)')
print('The initial exposure time is',par.exptime,'s')
print('The input file named',inputfile,'includes three dynamical parameters: M, i and D')
print('##########################################')
print('##########################################')
print('##########################################')


with open(inputfile,'r')as f:
	data = f.read().splitlines()
error_analysis(data,par)




