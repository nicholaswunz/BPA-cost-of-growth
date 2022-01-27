# Effect of warming and BPA on the energetic cost of growth

This repository contains data needed to reproduce the prediction maps:

**Wu N. C., Rubin A. M., & Seebacher, F.** (2022) Endocrine disruption from plastic pollution and warming interact to increase the energetic cost of growth in a fish. *Proceedings of the Royal Society B*, **289**, 20212077. DOI:
[![DOI](https://zenodo.org/badge/DOI/10.1098/rspb.2021.2077.svg)](http://doi.org/10.1098/rspb.2021.2077)

**When using the data or code from this project, please cite it as:**

Seebacher, Frank; Wu, Nicholas; Alexander, Rubin (2022), Endocrine disruption from plastic pollution and warming interact to increase the energetic cost of growth in a fish, *Dryad*, Dataset. DOI: [![DOI](https://zenodo.org/badge/DOI/10.5061/dryad.v6wwpzgxm.svg)](https://doi.org/10.5061/dryad.v6wwpzgxm)

**Raw data**
- `2050AOGCM.RCP85.Surface.Temperature.Mean.tif` - RCP8.5 sea surface temperature 2040-2050 from Bio-ORACLE v2.1
- `2100AOGCM.RCP85.Surface.Temperature.Mean.tif` - RCP8.5 sea surface temperature 2090-2100 from Bio-ORACLE v2.1
- `PlasticRiverInputs.shp`                       - Shape file of global plastic river input from [Lebreton et al (2015)](https://www.nature.com/articles/ncomms15611)
- `SAUEEZ_July2015.shp`                          - Shape file of Exclusive Economic Zone (EEZ) from [Sea Around Us](https://www.seaaroundus.org/data/#/eez)
- `CatchInd2015_2015.csv`                        - Catch data from [Watson (2015)](https://www.nature.com/articles/sdata201739#Sec20) 

**Analysis**
- Experiment analysis was performed by F.S.

**R Codes**
- `plastic_pollution_map.R` - Data cleaning, analysis and figure production for Fig. 4 and supplementary figures.

**Extra files**
- `rspb20212077_si_001.docx` - Supplementary file includes statistical outcomes and additional figures and descriptions from the main document.

## Abstract
Energetic cost of growth determines how much food-derived energy is needed to produce a given amount of new biomass and thereby influences energy transduction between trophic levels. Growth and development are regulated by hormones and are therefore sensitive to changes in temperature and environmental endocrine disruption. Here we show that the endocrine disruptor bisphenol A (BPA) at an environmentally relevant concentration (10 μg l⁻¹) decreased fish (*Danio rerio*) size at 30 °C water temperature. Under the same conditions, it significantly increased metabolic rates and the energetic cost of growth across development. By contrast, BPA decreased the cost of growth at cooler temperatures (24 °C). BPA-mediated changes in cost of growth were not associated with mitochondrial efficiency (P/O ratios [i.e. adenosine diphosphate (ADP) used/oxygen consumed] and respiratory control ratios) although BPA did increase mitochondrial proton leak. In females, BPA decreased age at maturity at 24 °C but increased it at 30 °C, and it decreased the gonadosomatic index suggesting reduced investment into reproduction. Our data reveal a potentially serious emerging problem: increasing water temperatures resulting from climate warming together with endocrine disruption from plastic pollution can impact animal growth efficiency, and hence the dynamics and resilience of animal populations and the services these provide.

**Keywords:** bisphenol A, climate change, metabolism, mitochondria, zebrafish, trophic levels

## License
This repository is provided by the authors under the [MIT](https://opensource.org/licenses/MIT) License.
