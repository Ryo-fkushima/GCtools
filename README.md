# **GCtools**

This is a set of viable tools for geochemistry.

## Features
This package includes two functions for plotting rare earth element (REE) concentrations normalized by various types of mantle/chondritic values. All you have to do is: 1) prepare a compositional dataset; 2) choose elements for making plots; and 3) choose the appropriate reference dataset (Sun and McDonough, 1989; McDonough and Sun, 1995).

## Requirement
R 4.0.2
## Installation
Please type the following command in your R console.

`remotes::install_github("Ryo-fkushima/GCtools")`
## Usage
`spdg(elist = c("Cs","Rb","Ba","Th","U","Nb","K","La","Ce","Pb","Pr","Sr","P",
"Nd","Zr","Sm","Eu","Ti","Dy","Y","Yb","Lu"), s = example_data_GCtools, 
nml = "CI_MS95", output = FALSE)`

## Author
Ryo Fukushima (Tohoku University, Sendai, Japan)

## References
Sun SS, McDonough WF (1989) Chemical and isotopic systematics of oceanic basalts: implications for mantle composition and processes. Geological Society, London, Special Publications, 42:313-345

McDonough WF, Sun SS (1995) The composition of the Earth. Chemical geology, 120:223-253



