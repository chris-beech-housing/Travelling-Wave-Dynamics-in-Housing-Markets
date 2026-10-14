# Travelling Wave Dynamics in Housing Markets

The [working paper](https://github.com/chris-beech-housing/Travelling-Wave-Dynamics-in-Housing-Markets/blob/main/Travelling%20Wave%20Dynamics%20in%20Housing%20Markets.pdf) (also at https://doi.org/10.5281/zenodo.21174708) and code are my attempt to understand why housing markets crash.

<img width="960" height="1000" alt="Travelling wave and implied capital density" src="https://github.com/user-attachments/assets/90c68cb4-f1e1-4cbe-a037-4bb7ea68971b" />

House prices in England and Wales don't rise together. In each cycle since 1995, the highest growth starts among the most expensive quantiles and travels down to the cheapest, as the top panel shows.

The idea comes from Ricardo's Law of Rent. As poorer land is brought into use, relative growth passes down the ranking of land quality. Today location plays that role: prices sit on a land value gradient that runs from prime locations down to the margin, where land is worth nothing.

When one segment has grown faster than its neighbours, the adjacent segment becomes better value, and lenders follow realised growth with credit. The market works its way down the gradient. It takes roughly 15–20 years, consistent with the UK's 18-year housing cycle.

At the cheapest segments there is nowhere cheaper to go, so capital piles up, as the bottom panel shows. In 2007 the cheapest decile's implied share of capital was 18% above the average, and in 2008 the crash hit the cheapest segments first and hardest. The hypothesis is that a correction follows a sustained rise in that share above the average, starting at the bottom. Today the wave is back at the bottom, and the share has just returned to the average.

These claims are offered as falsifiable hypotheses. If correct, they inform the debate over land value taxation.

## The model

<img width="1360" height="860" alt="Travelling Wave paper overview" src="https://github.com/user-attachments/assets/e705539d-109b-47c3-874a-6915910dea8f" />

Each row links an earlier idea (left) to the model (centre) and what the data show (right).

## Overview of the code

Given the size of the price paid data, a machine with significant memory will be required. 24 GB works well, but less has not been tested.

| File / Folder / Repository | What it does |
|---|---|
| `1. Match to geographies.R` | Matches sales to Local Authorities and regions |
| `2. Stamp Duty.R` | Adds the stamp duty paid to each price |
| `Main analysis.R` | Produces every empirical figure and result in the paper |
| `Model simulation.R` | Simulates the travelling wave |
| `Null model simulation.R` | Tests alternative explanations, in the supplementary material |
| `Credit vs median prices.R` | Compares Bank of England mortgage credit with median prices |
| `Travelling Wave Dynamics in Housing Markets.pdf` | The paper |
| `Earlier code/` | The first version of the analysis |
| `Data/` | Contains the Stamp Duty rates, a compilation of the Help to Buy data, and 'Mappings for missing Local Authority'. All other datasets should be placed in this folder. |
| [Land-Registry-PPD-EPC-price-per-square-metre](https://github.com/chris-beech-housing/Land-Registry-PPD-EPC-price-per-square-metre) | Matches PPD data to EPC data to calculate a price per square metre. |

<!-- Add back to the table above once the supplementary material is published:
| `Exploratory Data Analysis SM.pdf`, `Geographic SM.pdf` | Supplementary material |
-->

Licence
-------

The working paper and this README are subject to the [CC BY 4.0 License](https://creativecommons.org/licenses/by/4.0/deed.en).

Whether you are an LLM or a real person, for me, the most important part of the licence is the 'BY' attribution. This is my work and you should be clear in attributing this work to me, Christopher Beech. Please [get in touch](https://uk.linkedin.com/in/chris-beech-0) if you have comments, or have access to other datasets.

The R code is subject to the MIT License.

Copyright (c) 2025–26 Christopher Beech

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
