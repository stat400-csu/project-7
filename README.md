# Comparing Greenland, Delta, and Monte Carlo Methods for PAF Confidence Intervals

**Course:** STAT 400  
**Authors:** Kelsey Britton & Theyab Alkhoori  

# What This Project Does

This repository contains the complete analysis for a STAT 400 project comparing three methods for constructing **95% confidence intervals (CIs)** for the **Population-Attributable Fraction (PAF)** based on an article by Lee et al.:

- Delta Method  
- Greenland Method  
- Monte Carlo Simulation Method  

Using simulation, we evaluate how these methods perform with respect to:
- PAF point estimation
- Coverage probability
- Mean confidence interval width
- Frequency of confidence intervals outside the logical bounds \([0,1]\)

## Why This Project Is Useful

The Population-Attributable Fraction is widely used in epidemiology, but many standard confidence-interval methods can produce values below 0 or above 1, which are not interpretable as proportions. This project demonstrates how different CI methods behave under realistic conditions and highlights when simulation-based approaches provide more reliable and interpretable results.

---

## Repository Structure

main:
- Group7STAT400CodeSource.R # ALL simulation and analysis code
- FinalGroup7PaperSTAT400.html # Final paper (rendered output)
- FinalGroup7PaperSTAT400.Rmd # Final paper (source)
- FinalGroup7SlidesSTAT400.html # Final slides (rendered output)
- FinalGroup7SlidesSTAT400.Rmd # Final slides (source)
- project-7.Rproj # RStudio project file
- README.md # Project overview and instructions
- styles.css # Slide styling

---

## Required Packages

- knitr
- ggplot2

---

## Reproducibility

All simulations in Group7STAT400CodeSource.R are reproducible using set.seed(400)

---

## How to Run

1. Knit FinalGroup7PaperSTAT400.Rmd
    - This file references Group7STAT400CodeSource.R for simulation code
    - All simulations will be run
    - Figures will be generated
    - The final paper will be produced
    
2. Knit FinalGroup7SlidesSTAT400.Rmd
    - This file references Group7STAT400CodeSource.R for simulation code
    - This file references styles.css for aesthetics
    - All simulations will be run
    - Figures and plots will be generated
    - The final presentation will be produced
    
---

## Acknowledgments

References are provided in the paper and slides.