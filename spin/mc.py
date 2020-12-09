#!/usr/bin/python

import os,sys,argparse

root_path = os.getcwd()

def create_link(root_path,dirname,linkfile):
	src=root_path+'/'+linkfile
	dst=root_path+'/'+dirname+'/'+linkfile
	os.symlink(src,dst)
	return


def get_args():
	dp=[]
	dp.append('python mc.py [options]')
	dp.append('For example: python mc.py -x xspec_model.xcm -p par.txt -m 5 -i 4 -d 7 -l load.xcm')
	dp.append('')
	dp.append('version: v.02 (2020.11)')
	dp.append('author: Zhao Xueshan <xszhao@nao.cas.cn>')
	dp.append('')
	dp.append('This script is aiming at: ')
	dp.append('Performing MC simulations based on specific model file. ')
	dp.append(' ')
	usage=""
	for d in dp: usage += d+'\n'
	parser = argparse.ArgumentParser(usage=usage) 
	parser.add_argument('-l','--load',dest='load_data',default='',help='load_data_script.xcm')
	parser.add_argument('-x','--model',dest='model',default='',help='your_model.xcm')
	parser.add_argument('-o','--out',dest='output_file_name',default='fit_result.log',help='name of the log file')
	parser.add_argument('-p','--parameter',dest='parameter_sets_file',default='',help='A file that contains \'mass inclination distance\'')
	parser.add_argument('-m','--idxm',dest='index_mass',default='',type=int,help='mass parameter index in your_model.xcm')
	parser.add_argument('-i','--idxi',dest='index_inclination',default='',type=int,help='inclination parameter index in your_model.xcm')
	parser.add_argument('-d','--idxd',dest='index_distance',default='',type=int,help='distance parameter index in your_model.xcm')
	args = parser.parse_args()
	return args


def write_script():
	args = get_args()
	mypspacefile = args.parameter_sets_file
	out = args.output_file_name
	idxm = args.index_mass
	idxi = args.index_inclination
	idxd = args.index_distance
	model = args.model
	load = args.load_data
	with open('fit_script.xcm','w+') as f:
		f.write('@'+load+'\n')

	with open(mypspacefile,'r') as f:
		data = f.read().splitlines()
	for i in range(len(data)):
		mass = float(data[i].split(' ')[0])
		incl = float(data[i].split(' ')[1])
		dis  = float(data[i].split(' ')[2])
		dirname ='dir_%5.4f_%5.4f_%5.4f'%(mass,incl,dis)
		if os.path.exists(dirname+'/'+model):
			os.remove(dirname+'/'+model)
		create_link(root_path,dirname,model)
		with open('fit_script.xcm','a') as f:
			f.write('cd '+dirname+'\n')
			f.write('@'+model+'\n')
			f.write('newp '+str(idxm)+' '+str(mass)+' -1\n')
			f.write('newp '+str(idxi)+' '+str(incl)+' -1\n')
			f.write('newp '+str(idxd)+' '+str(dis)+' -1\n')
			f.write('query yes\n'+'renorm\n'+'fit\n')
			f.write('log '+out+'\n'+'show all\n'+'log none\n'+'cd ../\n')
	with open('fit_script.xcm','a') as f:
		f.write('exit\n')
	os.system('xspec - fit_script.xcm')
	return


write_script()

