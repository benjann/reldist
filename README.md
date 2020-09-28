# reldist
Stata module for relative distribution analysis

`reldist` estimates and analyzes the relative distribution of
outcomes between two groups (two-sample relative distribution) or between two
variables (paired relative distribution). The relative distribution is the
distribution of the relative ranks that the outcomes from one distribution take
on in the other distribution. An example would be the relative positions that
female wages take on in the distribution of male wages. `reldist` can be used
to estimate and plot the relative density function (relative PDF), a histogram
of the relative distribution, or the relative distribution function (relative
CDF). Furthermore, it computes relative polarization indices as well as
descriptive statistics of the relative data, and supports the decomposition of
the relative distribution by adjusting for location, scale, and shape
differences or for differences in covariate distributions. Statistical inference
is implemented in terms of influence functions and supports estimation for
complex samples.

To install `reldist` from the SSC Archive, type

    . ssc install reldist, replace

in Stata. Stata version 12 or newer is required. Furthermore, the `moremata` 
package is required. To install `moremata` from the SSC Archive, type

    . ssc install moremata, replace

---

Installation from GitHub:

    . net install reldist, replace from(https://raw.githubusercontent.com/benjann/reldist/master/)
    . net install moremata, replace from(https://raw.githubusercontent.com/benjann/moremata/master/)

---

Main changes:

    28sep2020 (version 1.2.4)
    - SEs were not correct if balance() was combined with -pooled-; this is fixed
    - etropy balancing crashed if factor variable notation expanded into different
      vectors in the two subsample; this is fixed
    
    27sep2020 (version 1.2.3)
    - now beaking ties in order of base weights if balance() is specified, not the 
      balancing weights
    - now using stable sort order unless -nosort- is specified
    
    26sep2020 (version 1.2.1)
    - balance() reimplemented; balancing weights are no longer assumed fixed when
      computing standard errors
    - option -replace- is now allowed with all subcommands so that balance(, generate())
      can overwrite existing variables
    - suboption -nord- added in bwidth() to omit the RD correction that is applied to
      bandwidth selectors by default
    - reldist pdf used the SJPI bandwidth selector even if a different bandwidth 
      selector was specified in bwidth(method); this if fixed
    - reldist div: compare(balance(,generate())) did not store the variable; this
      is fixed
    
    18sep2020 (version 1.2.0)
    - major update with many changes:
      o analytic standard errors are now computed for all estimates (based on 
        influence functions)
      o svy is supported through option vce(svy ...)
      o new -reldist divergence- command for estimation of divergence measures; 
        -reldist pdf- and -reldist histogram- no longer compute divergence
      o predict after -reldist- now computed influence functions
      o density estimation now based on moremata's new mm_density()
      o balance() option no longer relies on -kmatch-; supported reweighting 
        methods are IPW and entropy balancing
      o default kernel now "gaussian", leading to smoother results for the PDF
      o now using non-adaptive kernel estimation by default
      o -reldist histogram- now implemented in terms of CDF (or PDF, depending on 
        situation)
      o -reldist summarize- no longer calls -tabstat-; list of supported statistics
        changed
      o option pooled now only allowed in syntax 1
      o option cross() discarded
      o aweights no longer allowed; iweights now treated like pweights
      o and various other changes ...

    17jun2020 (version 1.1.8):
    - [y]obael() now prunes labels that are too close together; new suboptions 
      -noprune- and -prune()- affect this behavior
    - -reldist pdf/hist- now have option -cross()-
    - -reldist hist- now also computes divergence measures (based on histogram density)
    - -reldist pdf- now computes divergence measures based on output grid (not 
      the internal approximation grid); divergence measures are now also reported
      if option -exact- is specified
    - -reldist pdf- now also computes the dissimilarity index (total variation 
      distance); e(divergence) renamed to e(entropy)
    - -reldist pdf-: napprox() was set to max(512, n()+1) instead of max(512, n())
      if not specified; this is fixed
    - -reldist mrp- now option -reference-

    12jun2020 (version 1.1.7):
    - [y]olabel() now allows argument #n to generate n labels at evenly spaced 
      (approximately) positions from min to max; [y]olabel without is equivalent
      to [y]olabel(#6); less than n labels may be produced if there is heaping
      in the data
    
    11jun2020 (version 1.1.6):
    - options atx(reference) and atx(comparison) added
    - new option balance(, contrast): compare unbalanced with balanced distribution
    - reldist CDF now has option -alt- to use an alternative estimation method
      based on relative ranks
    - reldist graph has new option -[y]oline()-
    - -reldist olabel- has new option -line()-
    - option -otick- in -reldist olabel- is now called -tick()
    - [y]label() etc. may now repeated
    - [y]otitle() is now also printed if no labels or ticks are requested
    - default for ogrid() has been increased to 401
    - -reldist sum- returned error of -balance()- was specified; this is fixed
    - minor changes to output header
    - option -descending- added
    - adjust(,multiplicative) now returns error if the adjustment factor is 0, 
      negative, or missing
    - interpolation of relative CDF improved for 0<at<1 if an upright segment is
      hit: now using midpoint of upright segment insted of ceiling
    - internal function _rd_quantile() returned error if X only had one row; this is fixed
    - internal function _rd_uniq() returned error if X hat less than two rows; this is fixed
    
    07jun2020 (version 1.1.5):
    - reldist made Stata freeze if the number of evaluation points was too large
      (due size limits of -matrix-); an error message is now displayed if the
      number of evaluation points is too large
    
    05jun2020 (version 1.1.4):
    - made some speed improvements by avoiding repeated storting (data is now sorted
      once when reading the data; subsequent computations then use functions that
      assume sorted data; density estimation, however, still involves repeated
      sorting; this could be further improved).

    05jun2020 (version 1.1.3):
    - new algorithm for computing relative ranks that breaks ties; use option 
      -nobreak- to employ old algorithm
    - option atx() can now be used without arguments, i.e. as -atx-, to use the 
      observed values as evaluation points
    - -reldist cdf- and -reldist pdf- now both have options -discrete- and 
      -categorical-; the two options do the same, but -categorical- requests that
      outcome values are positive integers and labels the coefficients using 
      factor-variable notation; -discrete- does not impose such a restriction and 
      labels the coefficients as x#
    - atx() now only allows positive integers if -categorical- is specified
    - -reldist cdf- now always computes the relavtive CDF based on exact data and
      interpolates between the exact points if necessary
    - -reldist pdf- now allows -n()- or -at()- together with -discrete- or 
      -categorical-; in this case the discrete relative density is computed based
      on exact data and then mapped onto the evaluation grid requested by n() or at()
    - -reldist pdf- with option -discrete- or -categorical- now automatically removes
      outcome-value evaluation points that do not exist in the reference distribution
      (the discrete relative density is infinity for these values, but the values 
      have zero mass on the y-axis, so it appears reasonable to ignore
      them)
    - the option to affect the rendition of the reference line was documented as 
      -refline()- but implemented as -refopts()-; the option is now implemented
      as documented
    - e(bwmethod) is now reset to "oversmoothed" if default bandwidth estimation
      fails
    
    02jun2020 (version 1.1.2):
    - -reldist pdf- and -reldist cdf- now have option -discrete- to treat data as 
      discrete; evaluation is performed at existing outcome values; pdf is displayed
      as a step function
    - -reldist pdf- and -reldist cdf- now have option -atx- without 
      argument to evaluate at existing outcome values
    - new -nomid- option to avoid using midpoints for relative ranks
    - relative CDF is no longer computed as a step function; values are 
      interpolated between jumps; this is consistent with breaking ties
      randomly; option -nomid- has no effect on CDF (i.e. CDF is always computed
      as if nomid has been specified)
    - no longer using midpoints when computing values of at() from atx()
    - now using inverse empirical CDF for computing atx() from at() and for 
      computing e(ogrid) (i.e. no averaging where distribution function is flat)
    - no longer using interpolation when computing the olabel() positions; the
      positions are now consistent with how a CDF is computed
    - no longer using rangen() for creating evaluation grids (precision issue)
    - now only returns e(at) with two rows instead of e(at) and e(atx)
    
    06may2020 (version 1.1.1):
    - option balance() added
    - option pooled added
    - changed approach for olabel()/otick(); reldist now stores quantiles in e(ogrid)
      from where the label positions are computed (instead of computing the positions
      from the original data or e(atx)); option ogrid() can be used to set the
      size of the grid; default is ogrid(201)
    - option graph is no longer allowed with vce(bootstrap/jackknife)
    - reldist failed if weights were specified and the variable containing the
      weights was abbreviated; this is fixed
    - fixed some minor issues with output formatting

    02may2020 (version 1.1.0):
    - reldist released on GitHub and SSC
