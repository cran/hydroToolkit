---
title: "Readme"
author: "Ezequiel Toum"
date: "7/5/2020"
output: html_document
---

# hydroToolkit

## Overview
**hydroToolkit** is an object oriented programming tool (S4 type) that provides objects to store 
hydro-meteorological data-sets as if they where meteorological stations; it also has methods to read and 
build the objects, plot (static and interactive graphs) their series and manipulate them. This package was
design to cope with data from Argentina and Chile: Base de Datos Hidrológica Integrada (BDHI - Argentina), 
Instituto Argentino de Nivología, Glaciología y Ciencias Ambientales (IANIGLA - Argentina), Departamento
General de Irrigación (DGI - Argentina) and Centro de Ciencia del Clima y la Resiliencia (CR2 - Chile). 
As it was design under the OOP paradigm, the package functionality could be extended to incorporate new 
data-sets. 

## Installation 
### Dependencies
You can download it from CRAN repository as usual: 
```
install.packages("hydroToolkit")

```