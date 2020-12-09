
The following steps is aiming at perform MC simulations.

(1) this package contains following .py files:

    ftab.py
    runcheck.py
    mc.py

    copy these three files to a directory that includes your spectrum, background file and response file. Before running script, prepare load_data_in_xspec.xcm, model_in_xspec.xcm, parameter_set.txt(contains mass, inclination and distance), BHSPEC.fits.

    Note that the format of the parameter_set.txt should be as follows：

    14.8 27.0 1.86
    20.0 29.0 2.2
    .... .... ....


(2) run ftab.py to generate f-tables. A file named ftab.dat will be generated in the newly generated folder dir_*.

    python ftab.py -r rsp.fits -b bhspec_spin_0.1.fits -n 0.7 -o ftab.dat -l 1.0 -u 12.0 -f par.txt

(3) run runcheck.py to check your f-tables. 

    python runcheck.py -p par.txt -f fit_result.log -t ftab.dat

(4) run mc.py to perform MC simulations based on specific model file.

    python mc.py -x xspec_model.xcm -p par.txt -m 5 -i 4 -d 7 -l load.xcm

