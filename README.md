# Cygnus X-1

Emerging view of a 21-solar mass black hole in the Milky Way ([Miller-Jones et al. 2020]()).

This repository contains computational material developed/utilized in Miller-Jones et al. 2020 to estimate the mass of the black hole in the X-ray binary Cygnus X-1.

## Contents

- **astrometry** (Python): This folder contains the VLA source position measuremnets (Table 2 in the paper) and a Jupyter notebook with the details of our astrometric inference.
- **mass-luminosity** (Fortran): This folder contains the code and associated files used for evaluation of mass-luminosity relation for the companion star in Cyg X-1 (see [Ziolkowski 2014](https://ui.adsabs.harvard.edu/abs/2014MNRAS.440L..61Z/abstract)).
- **elc** (Fortran): This folder contains the code and associated files used for light curve synthesis/fitting to estimate binary parameters, particularly the mass of the black hole and the companion star (see [Orosz et al. 2011](https://ui.adsabs.harvard.edu/abs/2011ApJ...742...84O/abstract)).

## Other resources

- Simulations in the paper made use of the [COMPAS](http://github.com/TeamCOMPAS/COMPAS) rapid population synthesis code.
