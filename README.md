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
differences or for differences in covariate distributions.

To install `reldist` from the SSC Archive, type

    . ssc install reldist, replace

in Stata. Stata version 12 or newer is required. Furthermore, the `moremata`, `kdens`,
and `kmatch` packages are required. To install these packages from the SSC Archive, type

    . ssc install moremata, replace
    . ssc install kdens, replace
    . ssc install kmatch, replace

---

Installation from GitHub:

    . net install reldist, replace from(https://raw.githubusercontent.com/benjann/reldist/master/)

---

Main changes:

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
