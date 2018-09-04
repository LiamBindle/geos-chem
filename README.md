# Probing a CMake-based build system for GEOS-Chem
I setup this repo to explore the idea of using CMake in GEOS-Chem, and gauge the size of such a project.

## Motivation
My motivation for looking into a CMake-based build system for GEOS-Chem comes from some difficulties that I experience when building GCHP on Compute Canada's Graham cluster.
The build system for GEOS-Chem Classic is well tested and thus reliable, but building GCHP is still tricky.
The difficulties that I encountered when building GCHP on the Graham cluster are the following:

1. Verbose build logs (excessively)
    - Difficult to trace build errors
    - Unclear state after failed builds
2. Poor cross-platform support
    - Unable to build on Stetson cluster
    - Some hacking around required to build on Graham
3. Current build system has a complicated flow (routing)
    - Code root &rarr; GeosCore &rarr; All subprojects &rarr; GeosCore

These difficulties are amplified by the fact that compiling GCHP takes a long time&mdash;a single build taking a few hours&mdash;which makes debugging time consuming.
The source of these shortcomings are things that CMake was specifically designed to solve.

## Benefits of CMake
CMake is a mature utility for _generating_ build systems. CMake is 18 years old with regular updates (most recently on Aug 9, 2018), and is used by many scientific projects including HDF5, LAPACK, ROOT, and QGIS. Additional non-scientific software includes zlib, MySQL, MiKTeX, and Netflix.  

1. Well defined scopes for compiler options/flags
    - Bottom up (inherited from dependencies)
    - Top down (project-wide and subproject-wide)
    - Results in a simpler build system (conceptually) which in turn makes debugging easier
2. Designed to be platform and compiler agnostic
3. Powerful tools for finding/using external dependencies
    - E.g. NetCDF include and lib directories are automatically detected
    - ESMF and MAPL could be made external dependencies (currently internal)
        - Removes responsibility for building ESMF/MAPL (get binaries via conda maybe?)
        - Speed up GCHP build
4. Seperate source and build trees
   - Source tree is never modified during a build
   - State of build is obvious after a successful/failed build
    - Aided by clear build logs

Ultimately, I think that CMake would simplify the process of building GCHP. 

## Proposed workflow

![Image of using CMake](https://github.com/LiamBindle/geos-chem/tree/bistro/docs/cmake.png)

## Migration strategy
CMake can be implemented _alongside_ the existing build system, meaning current Make build system can be left untouched until it becomes the recommended approach.

