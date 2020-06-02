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
