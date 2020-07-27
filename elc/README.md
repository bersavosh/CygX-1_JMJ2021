# ELC

Light curve synthesis code from [Orosz & Hauschildt 2000](https://ui.adsabs.harvard.edu/abs/2000A%26A...364..265O/abstract). Originally applied to Cyg X-1 in [Orosz et al. 2011](https://ui.adsabs.harvard.edu/abs/2011ApJ...742...84O/abstract).

## Contents

A content list of files needed to run the Differential Evolution Monte Carlo Markov (DE-MCMC) code.

| File              | Description                                                                                               |
|-------------------|-----------------------------------------------------------------------------------------------------------|
|README             |  This file.                                                                                               |
|allRV.off          |  File containing the radial velocity data.                                                                |
|cyg_demcmc.f90     |  The DE-MCMC code (Fortran 90), modified to include a chi^2 contribution for the radius.                  |
|cyg_opt.f90        |  Fortran 90 subroutines needed for the DE-MCMC code.                                                      |
|demcmcELC.inp      |  Configuration file for the DE-MCMC code.                                                                 |
|ELC.atm            |  Specialized table of model atmosphere specific intensities used in the computation of the light curve.   |
|ELC.inp            |  Input file used for the DE-MCMC code.                                                                    |
|ELCposterior.f90   |  Code to generate and plot posterior samples.                                                             |
|ELCcolumns.inp     |  Configuration file for the posterior plotting code.                                                      |
|gridloop.opt       |  Configuration file for the DE-MCMC code.                                                                 |
|lcmods.f90         |  Fortran 90 modules used in the DE-MCMC code.                                                             |
|lcsubs.f90         |  Fortran 90 subroutines needed for the DE-MCMC code.                                                      |
|newU.scale         |  U-band optical light curve.                                                                              |
|newB.scale         |  B-band optical light curve.                                                                              |
|newV.scale         |  V-band optical light curve.                                                                              |
|pgmods.f90         |  Fortran 90 modules used in the posterior plotting code.                                                  |
|pgsubs.f90         |  Fortran 90 subroutines needed for the posterior plotting code.                                           |
|plotsubs.f90       |  Fortran 90 subroutines neededd for the posterior plotting code.                                          |

## Execution

To compile the plotter code use

```shell
gfortran -o ELCposterior ELCposterior.f90
```

To compile the DE-MCMC code use

```shell
gfortran -O3 -fopenmp -o cyg_demcmc cyg_demcmc.f90
```

The `-fopenmp` flag will allow the code to run on multiple cores using Open MP. 
To use multiple cores under Linux, set these environment variables (this
is for the tcsh shell):

```shell
setenv OMP_STACKSIZE 12000000
setenv OMP_NUM_THREADS 20
setenv OMP_SCHEDULE dynamic
limit stacksize unlimited
```

To execute the code, use

```shell
./cyg_demcmc > OUTPUT &
```

There will be various output to the screen, and many output files
will be written.  To plot the current best-fitting model light
curves over the data, use a plotting package such as `xmgrace`:

```shell
xmgrace modelU.mag newU.scale
xmgrace modelB.mag newB.scale
xmgrace modelV.mag newV.scale
xmgrace star1.RV allRV.off
```

The residuals to the fits are written to files:

| File                   | Description    |
|------------------------|----------------|
|ELCresidualsU.fold      |U-band residuals|
|ELCresidualsB.fold      |B-band residuals|
|ELCresidualsV.fold      |V-band residuals|
|ELCresidualsRV1.fold    |RV residuals    |

The file demcmcELC.out has several columns:

1. The generation number
2. The jump fraction
3. The value of the scaling parameter
4. The "temperature" (set to 1 here)
5. (to column 10) Integers generated from the random number generator

After a sufficient burn-in, you can generate a posterior sample.
For example:

```shell
ELCposterior 200 100 100000 50
```

- The first number is the starting generation (200 in this example)
- The second number is the number of generation to skip after the start
(in this example, samples are taken every 100 generations).
- The third number is the ending generation (set to a very large
number here, it won't matter if the code has not yet made it to
that generation)
- The fourth number is the number of bins to use when generating the
histograms.
- To see the posteriors, look at the PostScipt plot named `pgplot.ps`.

There will be "histograms" generated. The suffix of the file gives
the parameter:

|File                | Description                                          |
|--------------------|------------------------------------------------------|
|hist.a1             | semimajor axis of the O-star                         |
|hist.a2             | semimajor axis of the BH                             |
|hist.a2AU           | semimajor axis of orbit in AU                        |
|hist.argper         | argument of periaston                                |
|hist.ecc            | eccentricity                                         |
|hist.fill1          | Roche lobe filling factor of O-star                  |
|hist.gam1           | systemic velocity (km/sec)                           |
|hist.i              | binary inclination in degrees                        |
|hist.L1             | luminosity of O-star in solar luminosities           |
|hist.log_g1         | gravity of O-star (log g)                            |
|hist.lum            | log (base 10) of the luminosity                      |
|hist.M1solar        | O-star mass                                          |
|hist.M2solar        | BH mass                                              |
|hist.omega1         | ratio of rotational to orbital frequency of O-star   |
|hist.out            | ignore                                               |
|hist.primK          | K-velocity of O-star                                 |
|hist.primmass       | O-star mass in solar masses                          |
|hist.pshift         | phase shift parameter                                |
|hist.R1solar        | radius of O-star in solar radii                      |
|hist.Teff1          | effective temperature of O-star in K                 |

The posterior samples are written into files with the prefix
`ELCjunk` and suffix as above.
