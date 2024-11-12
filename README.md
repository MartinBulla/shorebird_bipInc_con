## Analyses of partner similarity in incubation bouts of biparenal shorebird using data from Bulla *et al.* 2016, Nature
 
### **Overview**

Data, codes and outputs of the analyses in [add preprint](add url)

When using any of the materials, **PLEASE CITE** the [original paper](add url) and this [Supporting information](add url).

**Before** **using** the data and scripts we recommend **read**ing the **[paper](add url)** and the information below

### **Folders and files**

[Data](Data/): xoriginal data files from Bulla *et al.* 2016, Nature (indicated by Bulla_et_al_2016 prefix), used in the current analyses

[R](R/): all r-scripts used in the analyses
- [tools.R](R/tools.R) loads functions and packages used in the other R-scripts (needs to be loaded before running the other scripts)
- [MS_v1.R](R/MS_v1.R) script contains the whole manuscript, including visual outputs along with R-code 
- [_runRmarkdown.R](R/runRmarkdown.R) - calls to run MS_v1 script and generates html document in the Output folder

[Output](Output/): contains outputs of the MS_v1.R, i.e. html filese of all analyses, including plots of model assumptions, and  the main text Figs and Tables for the submission.
