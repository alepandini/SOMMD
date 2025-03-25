# SOMMD

<!-- badges: start -->
  [![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
  [![cran-version](https://www.r-pkg.org/badges/version-last-release/SOMMD?color=blue)](https://cran.r-project.org/web/packages/SOMMD/index.html)
  [![downloads](https://cranlogs.r-pkg.org/badges/grand-total/SOMMD)](https://cranlogs.r-pkg.org/badges/grand-total/SOMMD)

<!-- badges: end -->
 
Self Organising Map for analysis of Molecular Dynamics data

## Features ##

SOMMD provides functions to processes data from Molecular Dynamics simulations using Self Organising Maps. Features include the ability to read different input formats such as pdb, gro, dcd and xtc files. Trajectory can be analysed to identify groups of important frames.

## Installing SOMMD ##

For the majority of users we recommend the use of the last stable release available from CRAN. 
To install from within R issue the command:

```
#!r
install.packages("SOMMD", dependencies=TRUE)
```

## Tutorials ##

The package includes three tutorial notebooks located in the `inst/Rmd` folder, demonstrating different applications of the SOMMD package:  

1. **Clustering of MD trajectories**  
   This tutorial introduces the use of Self-Organizing Maps (SOM) for analyzing the conformational landscape of a protein from molecular dynamics (MD) simulations. It demonstrates how SOMMD can group similar structures, visualize macrostates, and extract representative conformations.  

2. **Analysis of Pathways in Protein Unfolding Simulations**  
   This tutorial applies SOM to Steered MD (SMD) simulations to investigate protein unfolding pathways. It showcases how SOM can classify different unfolding states and track pathways sampled across simulations, enabling a comparative analysis of unfolding mechanisms.  

3. **Transition Network Analysis in Ligand-Protein Metadynamics Simulations**  
   This tutorial focuses on ligand binding dynamics using metadynamics simulations. It explains how to train a SOM on ligand-protein interaction data and build a transition network to analyze binding pathways, providing insights into the complexity of the binding process.

## List of Functions and Their Usage ##   

The table below provides a complete list of functions available in the SOMMD package, categorized by their role in the analysis workflow. The functions are grouped into Data Import and Preprocessing, which are essential for all analyses, and those specific to Conformational State Analysis, Pathway Analysis, and Network Analysis. This structured overview helps users understand how different functions contribute to each stage of the analysis.

#### Data Import
| Function Name  | Description |
|---------------|-------------|
| `read.struct`  | Reads molecular structure files (PDB/GRO) and returns a struct object encapsulating atomic coordinates, box dimensions, and topological information. |
| `read.trj`  | Reads trajectory files (XTC/DCD) and returns a trj object containing the 3D coordinate array, topology, and frame indices. |
| `cat.trj`  | Concatenates multiple trajectory files into a single trj object, ensuring consistency across simulations. |
| `trj2xyz`  | Converts the 3D trajectory array into a 2D XYZ coordinate matrix for further processing or visualization. |
| `print.struct`  | Prints a summary of a struct object, highlighting key features of the imported structure. |
| `print.trj`  | Prints a summary of a trj object, providing basic information about the trajectory data. |

#### Preprocessing
| Function Name  | Description |
|---------------|-------------|
| `fit.trj`  | Aligns a trajectory to a reference structure using the Kabsch algorithm (via bio3d), facilitating structural comparisons. |
| `calc.distances`  | Computes distance matrices from trajectory data, serving as input for SOM training. |
| `native.cont`  | Selects native contact distances from a reference structure or a trajectory frame based on a user-specified distance cutoff. |

#### Analysis of Conformational States
| Function Name  | Description |
|---------------|-------------|
| `cluster.representatives`  | Identifies representative frames for each cluster, providing key conformations that summarize the cluster’s behavior. |
| `silhouette.profile`  | Calculates the silhouette profile for a given number of clusters, helping evaluate the quality of the clustering. |
| `silhouette.score`  | Computes silhouette scores across a range of cluster numbers to assist in determining the optimal cluster partition. |
| `neur.population`  | Computes the per-neuron population (number of frames assigned to each neuron), which can be used to assess cluster significance. |
| `neur.representatives`  | Determines the representative frame for each neuron by selecting the frame closest to the neuron’s codebook vector. |
| `average.neur.property`  | Computes the average value of a given property for each neuron within the SOM, aiding in the characterization of neuron-specific features. Useful to visualize properties. |
| `remap.data`  | Maps new data onto a pre-trained SOM, allowing the extension of the analysis to additional simulations or datasets. |
| `som.add.circles`  | Adds circles to a SOM plot—sized proportionally to a selected property—to enhance the graphical representation of the SOM. |

#### Pathway Analysis
| Function Name  | Description |
|---------------|-------------|
| `trace.path`  | Traces the path of a given trajectory through the SOM, allowing the reconstruction of the sequence of visited neurons. |
| `cluster.pathways`  | Clusters conformational pathways based on time-dependent or independent schemes, facilitating the exploration of dynamic transitions. |

#### Network Analysis
| Function Name  | Description |
|---------------|-------------|
| `comp.trans.mat`  | Computes the transition matrix between SOM neurons, quantifying the probability of transitions between neurons in the simulation. |
| `matrix2graph`  | Converts a transition matrix into an igraph object, enabling network visualization of state transitions between SOM neurons. |
| `map.color`  | Maps a numeric property vector to a color scale based on a provided palette, useful for annotating visual outputs. |

## Citation ##

To cite SOMMD in publications use:

  Motta, S., Callea, L., Bonati, L., Pandini, A. (2022).
  “PathDetect-SOM: A Neural Network Approach for the Identification of
  Pathways in Ligand Binding Simulations.” _Journal of Chemical Theory
  and Computation_, *18*(3), 1957-1968. doi:10.1021/acs.jctc.1c01163
  <https://doi.org/10.1021/acs.jctc.1c01163>.

