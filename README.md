# Travelling Wave Dynamics in Housing Markets

**A Data-Driven Ricardian Explanation of Housing Market Crashes**

This [working paper](https://github.com/chris-beech-housing/Travelling-Wave-Dynamics-in-Housing-Markets/blob/main/Travelling%20Wave%20Dynamics%20in%20Housing%20Markets.pdf) (also at https://doi.org/10.5281/zenodo.17841584) and code is my attempt to understand why housing markets crash. It is really three papers in one:

* A major statistical analysis of England and Wales house price data
* A interpretation of the results using Ricardo's Law of Rent
* A simple econophysics model (although I avoid equations in favour of the Ricardian narrative)

Abstract
--------

We analyse England and Wales transactional house price data to describe a robust, empirical regularity across two cycles: a systematic progression of ranked annual returns. We demonstrate that whilst a geographical analysis by Local Authority appears intuitive, an analysis by quantile provides a superior description of information propagation through housing markets.

This travelling wave pattern, independent of the underlying house price growth, is both a data-driven version of Ricardo’s Law of Rent and a leading indicator of house market crashes.

We propose a simple model where the log-logistic distribution of house prices is subject to drift and diffusion. In addition, a constant-speed travelling wave ensures that over time the fastest-growing quantile travels from the highest to the lowest in order. When the travelling wave reaches the lowest quantile or margin, there is a crash in house prices. The trough is reached when the land value at the margin is again worth zero; subsequently, the cycle restarts.

The benefits of a land value tax are briefly discussed.

The analysis makes testable statements not only about the future but also about other countries’ national housing datasets.

Two minute summary
------------------

Had I not read Martin’s Wolf’s 2010 Financial Times column, [Why we must halt the land cycle](https://www.youtube.com/watch?v=g5kc9RepC1Q), which referenced the work of [Fred Harrison](https://www.amazon.co.uk/Boom-Bust-Prices-Banking-Depression-ebook/dp/B008M0QJIM/) on the 18-year land cycle, this work may never have existed.

Although there are many plots in the paper, this one best captures the core idea:
<img width="1200" height="872" alt="overview heatmaps" src="https://github.com/user-attachments/assets/cb1d86da-47a6-42b8-85fb-75ab78468647" />

The above heat maps are by decile, the highest median price is at the top, the lowest median price is at the bottom.

The top heat map shows the 'Moving average of the annual log return of the monthly median using a [price per square metre dataset](https://github.com/chris-beech-housing/Land-Registry-PPD-EPC-price-per-square-metre), by decile'. Simply, this is the change in house prices over time; the great financial crisis is very clear in blue.

The bottom heat map shows the 'Rank of the moving average of the annual return of the monthly median using the price per square metre dataset, by decile'. The rank allows us to visualise the change in house prices independently of the underlying house price growth, i.e. independently of interest rates and credit conditions.

In the rank plot, the fastest growing (or lowest falling) deciles are in dark red, and the slowest growing (or highest falling) deciles are in dark blue.

Even the most casual observer will notice the reverse S-curves traced by the dark red, over two separate house price cycles. The rate of progress is very similar in both cases. I define the per-cycle pattern as a travelling wave, a 'Beech wave' if you will ;-)

Note that the travelling wave ranks are particularly clean (systematically ordered from ranks one to ten) in both vertical and horizontal dimensions. Only with the price per square metre dataset are the ranks so neat and tidy; the paper considers other noisier approaches in more detail.

Each cycle, the travelling wave follows the cumulative distribution function of the house prices from high to low, and represents the propagation of information down through the distribution, that is, through the market. The market is very efficient in this respect.

It is noticeable that the travelling wave reaches the decile with the lowest median price prior to 2008. However, the richest areas subsequently saw the fastest growth immediately prior. This may have been due to foreign buyers in London, or what Fred Harrison calls the 'winner’s curse' phase.

In the second cycle, there is no impact on the travelling wave from Brexit, but it is interrupted due to the pandemic: the so-called 'race for space'.

In the paper, I argue that the travelling wave is both a data-driven version of Ricardo’s Law of Rent and a leading indicator of house price crashes. When combined with the 18-year cycle observed by Fred Harrison, the current location of the travelling wave suggests a crash in 2026-27.

Google NotebookLM podcast
-------------------------

For fun, I have uploaded a Google NotebookLM [podcast](https://github.com/chris-beech-housing/Travelling-Wave-Dynamics-in-Housing-Markets/releases/download/code-paper-podcast-v1.0/Travelling.Wave.Dynamics.in.Housing.Markets.NotebookLM.m4a) version of a slightly earlier version of the paper.

Be aware that:

* It starts with the anecdote of Queen Elizabeth II at the London School of Economics, which despite being only a footnote in my paper is a good journalistic opening.
* 'Zoning' is not a term we use in the UK, our planning approach is different, and I don't mention planning at all.
* It initially doesn't distinguish enough between the price heatmaps and the rank heatmaps, but gets it right later on.
* The Local Authorities rank plots are not as noisy as it suggests, although the adjacency matrices are.
* It mentions 'inflation', which is not a term I use when discussing the travelling wave, but is something I discuss for repeat sales.
* It indirectly references [Rethinking the Economics of Land and Housing](https://www.amazon.co.uk/Rethinking-Economics-Land-Housing-Ryan-Collins/dp/135037427X/) on neoclassical economics, and the [Knoll et al. paper](https://www.aeaweb.org/articles?id=10.1257/aer.20150501) on land accounting for 80% of the global house price boom since WWII, but makes it sound like it's my research.

Other than that, it's not bad at all (English understatement), quite amazing really.

Overview of the code
--------------------

Given the size of the price paid data, a machine with significant memory will be required. 24GB works well but any less has not been tested.

All scripts are written to be standalone with minor exceptions (see below). This has resulted in repetition of some functions and graphing code across scripts but this was judged to make the code easier to follow for people other than the author.

'1. Import and tidy' and '2. Stamp Duty' prepare the data for analysis, nothing further can be done without them. Each has more than one use case and users should read the inline comments for details.

The price per square metre dataset should be created using code from [this repository](https://github.com/chris-beech-housing/Land-Registry-PPD-EPC-price-per-square-metre).

'5a. Local Authority maps' requires data from '5. Local Authority analysis'.

'5. and 8. subplots' requires creating plots from '5. Local Authority analysis' and '8. Quantile analysis' and then combining them with this script.

The 'Data' folder contains the Stamp Duty rates, a compilation of the Help to Buy data, and 'Mappings for missing Local Authority'. All other datasets should be placed in this folder.


License
-------

The working paper and this README is subject to the [CC BY 4.0 License](https://creativecommons.org/licenses/by/4.0/deed.en).

Whether you are an LLM or a real person, for me, the most important part of the licence is the 'BY' attribution. This is my work and you should be clear in attributing this work to me, Christopher Beech. Please [get in touch](https://uk.linkedin.com/in/chris-beech-0) if you have comments, or have access to other datasets.

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
