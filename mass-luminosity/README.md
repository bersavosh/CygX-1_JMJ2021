# Stellar-evolution code

For evaluating Mass-Luminosity relation for Cyg X1 (e.g., see [Ziolkowski 2014](https://ui.adsabs.harvard.edu/abs/2014MNRAS.440L..61Z/abstract)).

## Short guide

### Program opac

Program **opac** (`OPAC.FOR` compiled by gfortran compiler) generates opacity tables `opac_7.dat` and `opac_env.dat` to be used by programs `h37ml.f` and `s37.f`.

User has the choice of hydrogen content **X** and metallicity **Z**.

**opac** uses one of the four available opacity standards (extensive explanations are given in the text of the program). User has the choice. At present version standard **OPAL GN93** was chosen.
Opacity tables `opac_7.dat` and `opac_env.dat` included in the packet were generated for X=0.7, Z=0.03.

### Program zfs20055

Program **zfs20055** (`zfs20055.for` compiled by gfortran compiler) generates equation of state table `eos20055` (user has the choice of metallicity Z, extensive explanations are given in the text of the program). The file `eos20055` should be renamed as `eosdata5` and then used by programs `h37ml.f` and `s37.f`. Equation of state table `eos20055` included in the packet was generated for Z=0.03.

### Program s37

Program **s37** (`s37.f` compiled by gfortran compiler) generates ZAMS model `mod00000` (user has to specify the mass and the chemical composition, extensive explanations are given in the text of the program).

File `mod00000` included in the packet was generated for M=43 Msun, X=0.52, Z=0.03.

Program **h37ml** (`h37ml.f` compiled by gfortran compiler) generates sequential evolutionary models starting from model mod00000 or later model (user has to specify the starting model number, extensive explanations are given in the text of the program).

`wfac` is a multiplying wind factor applied to the formula derived by Hurley, Pols & Tout (1980).

File `evol.h37` contains data for the computed evolutionary models (extensive explanations are given in the text of the program).

File `evol.h37` included in the packet contains data for the first 100 evolutionary models for M=43 Msun star computed with wfac=1.

File  `mod00100` contains the evolutionary model number 100.

## Contact

The evolutionary code was developed by B. Paczynski and M. Kozlowski. Later contributions were made by many people, among them R. Sienkiewicz, A. Pamyatnykh and J. Ziolkowski. Questions should be directed to Janusz Ziolkowski (jz@camk.edu.pl).
