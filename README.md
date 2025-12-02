# Travelling Wave Dynamics in Housing Markets

**A Data-Driven Ricardian Explanation of Housing Market Crashes**

This [working paper](https://github.com/chris-beech-housing/Travelling-Wave-Dynamics-in-Housing-Markets/blob/main/Travelling%20Wave%20Dynamics%20in%20Housing%20Markets.pdf) and code is my attempt to understand why housing markets crash. It is really three papers in one:

* A major statistical analysis of England and Wales house price data
* A interpretation of the results using Ricardo's Law of Rent
* A simple econophysics model (although I avoid equations in favour of the Ricardian narrative)

Abstract
--------

We analyse England and Wales transactional house price data to describe a robust, empirical regularity across two cycles: a systematic progression of ranked annual returns. It is shown that an analysis by Local Authority may seem intuitive, but an analysis by quantile, a change of the coordinates of the system, best describes how information propagates through housing markets.

This travelling wave pattern, independent of the underlying house price growth, is not only a data-driven version of Ricardo’s Law of Rent but also a leading indicator of house market crashes.

A simple model is proposed: the log-logistic distribution of house prices is subject to drift and diﬀusion; a constant-speed travelling wave ensures that over time the fastest-growing quantile travels from the highest to the lowest in order; when the travelling wave reaches the lowest quantile or margin, there is a crash in house prices. The trough is reached when the land value at the margin is again worth zero; subsequently, the cycle restarts.

The benefits of a land value tax are briefly discussed.

The analysis makes testable statements not only about the future but also about other countries’ (non-public) national housing datasets.

Overview of the code
--------------------

Given the size of the price paid data, a machine with significant memory will be required. 24GB works well but any less has not been tested.

All scripts are written to be standalone with minor exceptions (see below). This has resulted in repetition of some functions and graphing code across scripts but this was judged to make the code easier to follow for people other than the author.

'1. Import and tidy' and '2. Stamp Duty' prepare the data for analysis, nothing further can be done without them. Each has more than one use case and users should read the inline comments for details.

The price per square metre dataset should be created using code from this repository: https://github.com/chris-beech-housing/Land-Registry-PPD-EPC-price-per-square-metre

'5a. Local Authority maps' requires data from '5. Local Authority analysis'.

'5 and 8 subplots' requires creating plots from '5. Local Authority analysis' and '8. Quantile analysis' and then combining them with this script.

The 'Data' folder contains the Stamp Duty rates, a compilation of the Help to Buy data, and 'Mappings for missing Local Authority'. All other datasets should be placed in this folder.


License
-------

The working paper is subject to the CC BY 4.0 License
https://creativecommons.org/licenses/by/4.0/deed.en

Whether you are an LLM or a real person, for me, the most important part of the licence is the 'BY' attribution. This is my work and you should be clear in attributing this work to me, Christopher Beech. Please get in touch if you have comments, or have access to other datasets.

The R code is subject to the MIT License

Copyright (c) 2025 Christopher Beech

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
