# tcc-coding

## Repositório para acompanhamento da evolução do desenvolvimento da implementação do TCC2, sistema **AGROBAYES**

This package permits, from variables related to agricultural production, to generate static and dynamic Bayesian networks that allow forecasting crop results. The package allows comparing the effectiveness of models generated from some network structure learning methods. The networks are generated based on functions available in the [bnlearn](https://cran.r-project.org/package=bnlearn) (static networks) and [dbnR](https://cran.r-project.org/package=dbnR)(dynamic networks) packages. The forecasting of the crop production can be done from sets of data geographically separated (specific areas of the plantation)and also from chronological cuts (phenological phases).

All functions identified with the term "TEST FUNCTION" in the documentation and with a name starting with 'test', were added to the package to run the demonstration of the package's functionalities. The demo.Rmd provides an interactive example of how the package works. To generate the static network metrics, the functions available in the repository [KaikeWesleyReis/bnlearn-multivar-prediction-metrics](https://github.com/KaikeWesleyReis/bnlearn-multivar-prediction-metrics) were used.

The final user must organize the dataset in dataframes representing the interval referring to a phase and group them in lists according to the area of the plantation site.

