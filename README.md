# Cygnus X-1 contains a 21-solar mass black hole – implications for massive star winds

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3961240.svg)](https://doi.org/10.5281/zenodo.3961240)

This repository contains computational material developed/utilized in [Miller-Jones et al. 2021](https://doi.org/10.1126/science.abb3363) to estimate the mass of the black hole in the X-ray binary Cygnus X-1.

## Contents

- **astrometry** (Python): This folder contains the VLBA source position measurements (Table 2 in the paper) and a Jupyter notebook with the details of our astrometric inference.
- **elc** (Fortran): This folder contains the code and associated files used for light curve synthesis/fitting to estimate binary parameters, particularly the mass of the black hole and the companion star (see [Orosz et al. 2011](https://ui.adsabs.harvard.edu/abs/2011ApJ...742...84O/abstract)).
- **mass-luminosity** (Fortran): This folder contains the code and associated files used for evaluation of mass-luminosity relation for the companion star in Cyg X-1 (see [Ziolkowski 2014](https://ui.adsabs.harvard.edu/abs/2014MNRAS.440L..61Z/abstract)).
- **spin** (Python; requires XSPEC): This folder contains the code and associated files used for X-ray spectral simulations to estimation the black hole spin (see [Gou et al. 2011](https://ui.adsabs.harvard.edu/abs/2014ApJ...790...29G/abstract)). **An updated calculation based on the new results is now published in [Zhao et al. 2021](https://doi.org/10.3847/1538-4357/abbcd6)**.
- **compass**: This folder contains the input and output files for our simulations of Cyg X-1 using the [COMPAS](http://github.com/TeamCOMPAS/COMPAS) rapid population synthesis code ([Stevenson et al. 2017](https://ui.adsabs.harvard.edu/abs/2017NatCo...814906S/abstract), [Vigna-Gómez et al. 2018](https://ui.adsabs.harvard.edu/abs/2018MNRAS.481.4009V/abstract)). **A detailed description of these simulations for Cyg X-1 based on the new results is now published in [Neijssel et al. 2021](https://doi.org/10.3847/1538-4357/abde4a)**.

## Citation:
If the contents of this repository have been useful for your work, please cite the journal publication ([Miller-Jones et al. 2021]()) along with the dateset's DOI (available in bibtex format below). Additionally, in case of individual packages (ELC, Spin, mass-luminosity, COMPASS), please cite the original publications mentioned above.

```bibtex
@dataset{miller_jones_james_2020_3961240,
  author       = {Miller-Jones, James and
                  Bahramian, Arash and
                  Orosz, Jerome and
                  Mandel, Ilya and
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
  title        = {{Cygnus X-1: Emerging view of a 21-solar mass black 
                   hole in the Milky Way}},
  month        = jul,
  year         = 2020,
  publisher    = {Zenodo},
  version      = {v1.0},
  doi          = {10.5281/zenodo.3961240},
  url          = {https://doi.org/10.5281/zenodo.3961240}
}
```
