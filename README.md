# Cygnus X-1: Emerging view of a 21-solar mass black hole in the Milky Way

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3961240.svg)](https://doi.org/10.5281/zenodo.3961240)

This repository contains computational material developed/utilized in [Miller-Jones et al. 2020]() to estimate the mass of the black hole in the X-ray binary Cygnus X-1.

## Contents

- **astrometry** (Python): This folder contains the VLA source position measuremnets (Table 2 in the paper) and a Jupyter notebook with the details of our astrometric inference.
- **mass-luminosity** (Fortran): This folder contains the code and associated files used for evaluation of mass-luminosity relation for the companion star in Cyg X-1 (see [Ziolkowski 2014](https://ui.adsabs.harvard.edu/abs/2014MNRAS.440L..61Z/abstract)).
- **elc** (Fortran): This folder contains the code and associated files used for light curve synthesis/fitting to estimate binary parameters, particularly the mass of the black hole and the companion star (see [Orosz et al. 2011](https://ui.adsabs.harvard.edu/abs/2011ApJ...742...84O/abstract)).

## Other resources

- Simulations in the paper made use of the [COMPAS](http://github.com/TeamCOMPAS/COMPAS) rapid population synthesis code.

## Citation:
If the contents of this repository have been useful for your work, please cite the journal publication ([Miller-Jones et al. 2020]()) along with the zenodo DOI (bibtex format below). Additionally, in case of the stellar evolution or light curve synthesis, please cite the original publications mentioned above.

```bibtex
@dataset{miller_jones_james_2020_3961240,
  author       = {Miller-Jones, James and
                  Bahramian, Arash and
                  Orosz, Jerome and
                  Mandele, Ilya and
                  Gou, Lijun and
                  Maccarone, Thomas and
                  Neijssel, Coenraad and
                  Zhao, Xueshan and
                  Ziolkowski, Janusz and
                  Reid, Mark and
                  Uttley, Phil and
                  Zheng, Xueying and
                  Byun, Do-Young and
                  Dodson, Richard and
                  Grinberg, Victoria and
                  Jung, Taehyun and
                  Kim, Jeong-Sook and
                  Marcote, Benito and
                  Markoff, Sera and
                  Rioja, Maria and
                  Rushton, Anthony and
                  Russell, David and
                  Sivakoff, Gregory and
                  Tetarenko, Alexandra and
                  Tudose, Valeriu and
                  Wilms, Joern},
  title        = {{Emerging view of a 21-solar mass black hole in the 
                   Milky Way}},
  month        = jul,
  year         = 2020,
  publisher    = {Zenodo},
  version      = {v1.0},
  doi          = {10.5281/zenodo.3961240},
  url          = {https://doi.org/10.5281/zenodo.3961240}
}
```
