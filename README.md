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
