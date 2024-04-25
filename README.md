Welcome to ProteolySee – the human plasma proteolytotype visualizer!
====================================================================

This interactive Shiny App allows you to visualize, investigate, and scan the N-terminome data from human SLE patients ("Proteolytic processing through the human complement system”, Demir et al., 2024) in a convenient and accessible way. The presented data is generated using human EDTA-plasma from a cross-sectional SLE cohort and corresponding CTRL samples from the Aarhus University Hospital (in total n=167) and longitudinal, treatment-resolving SLE lupus nephritis (LN) patients from the University Hospital Hamburg-Eppendorf (n=8). Cleaved protein N-termini were identified by mass spectrometry-based high-throughput protein N-termini purification by the High-efficiency Undecanal-based N Termini EnRichment method ([HUNTER; cf. Weng, Demir et al., MCP 2019: Sensitive Determination of Proteolytic Proteoforms in Limited Microscale Proteome Samples](https://doi.org/10.1074/mcp.TIR119.001560)).

Contact: Arvid Hutzfeldt [✉](mailto:a.hutzfeldt@uke.de "a.hutzfeldt@uke.de") und Fatih Demir [✉](mailto:fatih.demir@biomed.au.dk "fatih.demir@biomed.au.dk")  
Last update: 25.04.2024

### The App is comprised of the following datasets available for filtering/selection in the App:

#### Correlation with clinical parameters

Pearson correlation factors for the exact N-terminus to known & measured clinical parameters for the corresponding patient. Higher Pearson correlation factors indicate a higher degree of correlation.

#### Log2 fold changes

Presents the log2 fold change for the exact N-terminus between SLE/CTRL from the [cross-sectional cohort](https://doi.org/10.3899/jrheum.171033) or the log2 fold change between responders/non-responders from the longitudinal cohort.

#### Proteomic data

Whole-protein abundance data was generated from the cross-sectional SLE & CTRL cohort during the N-termini purification workflow from the same samples and is presented as the log2 fold change between SLE/CTRL.

#### In vitro protease digests

Recombinant key proteases of the human complement system were used for an in-vitro digest of heat-inactivated (56 °C, 30 min.) human EDTA-plasma (n=4, 2x male & 2x female) for 2h at 37 °C. The proteases C1r/C1s represent the classical pathway, MASP-1 the lectin pathway, and MASP-3 the alternative pathway. The presented log2 fold changes represent only significantly cleaved substrates by the proteases and are controlled against catalytically inactive variants for MASP-1 and MASP-3 (log2 fold change active/inactive protease) or against a no protease control for C1r & C1s.

#### Eculizumab treatment cohort

[Eculizumab-treated pediatric STEC-HUS](https://doi.org/10.1056/NEJMoa1208981) patients’ EDTA-plasma at 3 timepoints (D1, D8, D30) was subjected to HUNTER N-termini enrichment. Listed log2 fold changes are quantified in at least 2 patients over the mentioned timepoints.

#### MOFA factors

The cross-sectional SLE cohort N-terminome and proteome data was analyzed with the [MOFA R package](https://doi.org/10.15252/msb.20178124) and divided into 10 factors, where factors 1, 3, 5, 7 and 10 were driven by N-termini.

![mofa_factors](mofa.png)
