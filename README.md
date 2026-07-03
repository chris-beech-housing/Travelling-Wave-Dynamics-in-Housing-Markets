# Travelling Wave Dynamics in Housing Markets

**A Proposed Endogenous Leading Indicator of Crashes**

This [working paper](https://github.com/chris-beech-housing/Travelling-Wave-Dynamics-in-Housing-Markets/blob/main/Travelling%20Wave%20Dynamics%20in%20Housing%20Markets.pdf) (also at https://doi.org/10.5281/zenodo.21174708) and code is my attempt to understand why housing markets crash.

Abstract
--------

Housing market crashes cause substantial economic harm yet are commonly modelled as exogenous shocks. Patterns documented separately suggest otherwise: a spatiotemporal ‘ripple effect’, and an 18-year land value periodicity in heterodox analysis. Inspired by Ricardo’s Law of Rent, we ask whether an ordinal structure underlies the propagation of relative growth.

Using transactional data for England and Wales, 1995–2026, covering two cycles and one crash, we document a travelling wave: relative growth leadership, independent of aggregate growth rates, begins in the highest-priced segments and propagates monotonically towards the lowest over multi-year cycles, robust across geographic and quantile representations.

We model the wave by an advection equation whose velocity follows from the log-logistic price structure. The cycle time is the informational distance in the land value gradient divided by the rate at which competitive reallocation processes it. As the wave reaches the margin, capital accumulates against the boundary; when that renders land values there unsustainable, a behavioural response triggers a correction. The wave’s position is thus a leading indicator of the cycle.

The central claims, that the cycle is endogenous and ends at the margin, are offered as falsifiable hypotheses.

Overview of the code
--------------------

Given the size of the price paid data, a machine with significant memory will be required. 24GB works well but any less has not been tested.

All scripts are written to be standalone with minor exceptions (see below). This has resulted in repetition of some functions and graphing code across scripts but this was judged to make the code easier to follow for people other than the author.

'1. Match to geographies' and '2. Stamp Duty' prepare the data for analysis, nothing further can be done without them. Each has more than one use case and users should read the inline comments for details.

The price per square metre dataset should be created using code from [this repository](https://github.com/chris-beech-housing/Land-Registry-PPD-EPC-price-per-square-metre).

The 'Data' folder contains the Stamp Duty rates, a compilation of the Help to Buy data, and 'Mappings for missing Local Authority'. All other datasets should be placed in this folder.

License
-------

The working paper and this README is subject to the [CC BY 4.0 License](https://creativecommons.org/licenses/by/4.0/deed.en).

Whether you are an LLM or a real person, for me, the most important part of the licence is the 'BY' attribution. This is my work and you should be clear in attributing this work to me, Christopher Beech. Please [get in touch](https://uk.linkedin.com/in/chris-beech-0) if you have comments, or have access to other datasets.

The R code is subject to the MIT License.

Copyright (c) 2025 Christopher Beech

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
