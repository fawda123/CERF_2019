# README

Tracking San Francisco Bay water quality using generalized additive models in an R Shiny framework

Marcus Beck, Southern California Coastal Water Research Project, <mailto:marcusb@sccwrp.org>
Ian Wren, San Francisco Baykeeper, <mailto:ianwren@gmail.com>
Rebecca Murphy, University of Maryland Center for Environmental Science, <mailto:rmurphy@chesapeakbay.net >
Perry de Valpine, University of California Berkeley, <mailto:pdevalpine@berkeley.edu>
David Senn, San Francisco Estuary Institute, <mailto:davids@sfei.org>

Accurate and flexible trend assessment methods are valuable tools for describing historical water quality changes with long-term monitoring datasets.  Water quality data have been collected in San Francisco Bay (SFB) for over four decades and long-term trends have shown that eutrophic conditions are uncommon, despite elevated nutrient concentrations.  We explore the historical data available at nine stations in the Central, South, and Lower South Bay portions of SFB using generalized additive models (GAMs) to provide a novel characterization of trends in primary productivity.  GAMs are a generic method for non-linear analysis that can represent trends in a response variable as the sum of smoothed functions for different predictors.  Methods for applying GAMs for trend analysis of long-term water quality datasets have been developed by the Chesapeake Bay Program and were adapted to the time series for SFB.  The application of GAMs captured differences among stations in seasonal and interannual trends in chlorophyll concentrations that varied along the south-north longitudinal axis of SFB.  GAM results also identified complex variations in calculated gross primary productivity, showing an increase in the early period of record followed by a decrease at most sites.  We present the results of the GAMs using an interactive, web-based framework developed with the open-source R Shiny environment.  This framework allowed rapid exploration of different hypotheses and was a valuable communication tool for sharing results with stakeholders.
