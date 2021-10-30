{smcl}
{* 30oct2021}{...}
{viewerjumpto "Syntax" "reldist##syntax"}{...}
{viewerjumpto "Description" "reldist##description"}{...}
{viewerjumpto "Options" "reldist##options"}{...}
{viewerjumpto "Examples" "reldist##examples"}{...}
{viewerjumpto "Methods and formulas" "reldist##methods"}{...}
{viewerjumpto "Saved results" "reldist##saved_results"}{...}
{viewerjumpto "References" "reldist##references"}{...}
{hline}
help for {hi:reldist}{...}
{right:{browse "http://github.com/benjann/reldist/"}}
{hline}

{title:Title}

{pstd}{hi:reldist} {hline 2} Relative distribution analysis


{marker syntax}{...}
{title:Syntax}

{pstd}
    Estimation

{pmore}Two-sample relative distribution (syntax 1)

{p 12 21 2}
{cmd:reldist} {it:subcmd}
    {varname}
    {ifin} {weight}{cmd:,}  {cmd:by(}{help varname:{it:groupvar}}{cmd:)}
    [ {help reldist##opts:{it:options}} ]

{pmore}Paired relative distribution (syntax 2)

{p 12 21 2}
{cmd:reldist} {it:subcmd}
    {varname} {help varname:{it:refvar}} {ifin} {weight}
    [{cmd:,} {help reldist##opts:{it:options}} ]

{marker subcmd}{...}
{pmore}
    where {it:subcmd} is

{p2colset 13 25 27 2}{...}
{p2col:{opt pdf}}relative density{p_end}
{p2col:{opt hist:ogram}}relative histogram{p_end}
{p2col:{opt cdf}}relative cumulative distribution{p_end}
{p2col:{opt div:ergence}}divergence measures{p_end}
{p2col:{opt mrp}}median relative polarization{p_end}
{p2col:{opt su:mmarize}}summary statistics of relative ranks{p_end}

{pstd}
    Replay results

{p 8 17 2}
{cmd:reldist} [{cmd:,} {opt nohead:er} {opt notab:le} {help reldist##display_opts:{it:display_options}} ]

{pstd}
    Draw graph after estimation

{p 8 17 2}
{cmd:reldist} {cmdab:gr:aph}
    [{cmd:,} {help reldist##graph_opts:{it:graph_options}} ]

{pstd}
    Obtain influence functions after estimation

{p 8 17 2}
    {cmd:predict} {c -(}{help newvarlist##stub*:{it:stub}}{cmd:*} | {it:{help newvar:newvar1}} {it:{help newvar:newvar2}} {cmd:...}{c )-} {ifin}
        [{cmd:,} {opt sc:ores} {it:{help reldist##density_opts:density_options}} ]


{synoptset 26 tabbed}{...}
{marker opts}{col 5}{help reldist##options:{it:options}}{col 33}Description
{synoptline}
{syntab:{help reldist##mainopts:Main}}
{synopt:{opth by(groupvar)}}binary variable that identifies the groups (syntax 1 only)
    {p_end}
{synopt:{opt swap}}reverse order of groups (syntax 1 only)
    {p_end}
{synopt:{opt pool:ed}}use pooled distribution as reference distribution  (syntax 1 only)
    {p_end}
{synopt:{cmdab:bal:ance(}{help reldist##balance:{it:spec}}{cmd:)}}balance
    covariates using reweighting (syntax 1 only)
    {p_end}
{synopt:{cmdab:adj:ust(}{help reldist##adjust:{it:spec}}{cmd:)}}location and scale adjustment (not allowed for {cmd:mrp})
    {p_end}
{synopt:{help reldist##rankopts:{it:rank_options}}}details about computation of relative ranks
    {p_end}
{synopt:{opt r:eplace}}allow replacing existing variables
    {p_end}

{syntab:{help reldist##pdfopts:Subcommand {bf:pdf}}}
{synopt:{opt n(#)}}number of evaluation points; default is {cmd:n(101)}
    {p_end}
{synopt:{cmd:at(}{help numlist:{it:numlist}}|{it:matname}{cmd:)}}use custom evaluation grid on probability scale
    {p_end}
{synopt:{cmd:atx}[{cmd:(}{help reldist##pdfatx:{it:spec}}{cmd:)}]}evaluate relative density at outcome values
    {p_end}
{synopt:{opt discr:ete}}treat data as discrete
    {p_end}
{synopt:{opt cat:egorical}}treat data as categorical
    {p_end}
{synopt:{cmdab:hist:ogram}[{cmd:(}{it:#}{cmd:)}]}include
    histogram using {it:#} bins; default is {it:#} = 10
    {p_end}
{synopt:{opt alt}}use alternative estimation method for histogram
    {p_end}
{synopt:{help reldist##density_opts:{it:density_options}}}density estimation options
    {p_end}
{synopt:{opt gr:aph}[{cmd:(}{help reldist##graph_opts:{it:graph_options}}{cmd:)}]}display graph
    {p_end}
{synopt:{opt ogrid(#)} | {opt noogrid}}set size of outcome label approximation grid
    {p_end}

{syntab:{help reldist##histopts:Subcommand {bf:histogram}}}
{synopt:{opt n(#)}}number of histogram bins; default is {cmd:n(10)}
    {p_end}
{synopt:{opt alt}}use alternative estimation method
    {p_end}
{synopt:{opt discr:ete}}treat data as discrete
    {p_end}
{synopt:{opt cat:egorical}}treat data as categorical
    {p_end}
{synopt:{opt gr:aph}[{cmd:(}{help reldist##graph_opts:{it:graph_options}}{cmd:)}]}display graph
    {p_end}
{synopt:{opt ogrid(#)} | {opt noogrid}}set size of outcome label approximation grid
    {p_end}

{syntab:{help reldist##cdfopts:Subcommand {bf:cdf}}}
{synopt:{opt n(#)}}number of evaluation points; default is {cmd:n(101)}
    {p_end}
{synopt:{cmd:at(}{help numlist:{it:numlist}}|{it:matname}{cmd:)}}use custom evaluation grid on probability scale
    {p_end}
{synopt:{cmd:atx}[{cmd:(}{help reldist##cdfatx:{it:spec}}{cmd:)}]}evaluate relative CDF at outcome values
    {p_end}
{synopt:{opt alt}}use alternative estimation method
    {p_end}
{synopt:{opt discr:ete}}treat data as discrete
    {p_end}
{synopt:{opt cat:egorical}}treat data as categorical
    {p_end}
{synopt:{opt gr:aph}[{cmd:(}{help reldist##graph_opts:{it:graph_options}}{cmd:)}]}display graph
    {p_end}
{synopt:{opt ogrid(#)} | {opt noogrid}}set size of outcome label approximation grid
    {p_end}

{syntab:{help reldist##divopts:Subcommand {bf:divergence}}}
{synopt:{cmdab:o:ver(}{help varname:{it:overvar}}{cmd:)}}compute results for subpopulations defined by {it:overvar}
    {p_end}
{synopt:{opt en:tropy} or {opt kl}}report Kullback-Leibler divergence (entropy); this is the default
    {p_end}
{synopt:{opt chi:2} or {opt chisq:uared}}report the Chi-squared divergence
    {p_end}
{synopt:{opt tvd} or {opt dis:similarity}}report the dissimilarity index (total variation distance)
    {p_end}
{synopt:{opt all}}report all divergence measures
    {p_end}
{synopt:{opt n(#)}}size of evaluation grid
    {p_end}
{synopt:{opt alt}}use alternative estimation method for histogram
    {p_end}
{synopt:{opt pdf}}use kernel density estimator instead of histogram
    {p_end}
{synopt:{help reldist##density_opts:{it:density_options}}}density estimation options; only
    relevant if {cmd:pdf} is specified
    {p_end}
{synopt:{opt discr:ete}}treat data as discrete
    {p_end}
{synopt:{opt cat:egorical}}treat data as categorical
    {p_end}
{synopt:{opt com:pare}[{cmd:(}{help reldist##divcom:{it:options}}{cmd:)}]}compare divergence measures
    between two models
    {p_end}

{syntab:{help reldist##mrpopts:Subcommand {bf:mrp}}}
{synopt:{cmdab:o:ver(}{help varname:{it:overvar}}{cmd:)}}compute results for subpopulations defined by {it:overvar}
    {p_end}
{synopt:{opt mult:iplicative}}use multiplicative (instead of additive) adjustment
    {p_end}
{synopt:{opt log:arithmic}}use logarithmic (instead of linear) adjustment
    {p_end}
{synopt:{opt sc:ale}[{cmd:(sd)}]}adjust scale between distributions
    {p_end}

{syntab:{help reldist##sumopts:Subcommand {bf:summarize}}}
{synopt:{cmdab:o:ver(}{help varname:{it:overvar}}{cmd:)}}compute results for subpopulations defined by {it:overvar}
    {p_end}
{synopt:{cmdab:s:tatistics(}{help reldist##statistics:{it:statnames}}{cmd:)}}report
    specified statistics
    {p_end}
{synopt:{opth g:enerate(newvar)}}store the relative ranks in {it:newvar}
    {p_end}

{syntab:{help reldist##seopts:SE/CI}}
{synopt:{opt l:evel(#)}}set confidence level; default is {cmd:level(95)}
    {p_end}
{synopt:{cmd:vce(}{help reldist##vce:{it:vcetype}}{cmd:)}}variance estimation method;
    {it:vcetype} may be {cmdab:a:nalytic}, {cmdab:cl:uster} {it:clustvar}, {cmdab:svy}, {cmdab:boot:strap},
    or {cmdab:jack:knife}
    {p_end}
{synopt:{opt nose}}do not compute standard errors
    {p_end}

{syntab:{help reldist##reprtopts:Reporting}}
{synopt:{opt citrans:form}}report transformed confidence intervals
    {p_end}
{synopt:{opt nohead:er}}suppress display of output header
    {p_end}
{synopt:[{cmd:{ul:no}}]{cmdab:tab:le}}suppress/enforce display of coefficients table
    {p_end}
{synopt:{help reldist##display_opts:{it: display_options}}}standard reporting options
    {p_end}
{synoptline}
{pstd}
{cmd:fweight}s, {cmd:pweight}s, and {cmd:iweight}s are allowed; see {help weight}.


{marker density_opts}{col 5}{help reldist##density_options:{it:density_options}}{col 33}Description
{synoptline}
{synopt:{cmdab:bw:idth(}{it:#}|{help reldist##bwidth:{it:method}}{cmd:)}}set bandwidth
    to {it:#}, {it:#} > 0, or choose bandwidth selection method; default is {cmd:bwidth(sjpi)}
    {p_end}
{synopt:{opt bwadj:ust(#)}}rescale bandwidth by {it:#}, {it:#} > 0
    {p_end}
{synopt:{cmdab:bo:undary(}{help reldist##boundary:{it:method}}{cmd:)}}boundary correction method;
    default is {cmd:boundary(renorm)}{p_end}
{synopt:{opt adapt:ive(#)}}number of iterations of the adaptive
    density estimator; default is {cmd:adaptive(0)}
    {p_end}
{synopt:{cmdab:k:ernel(}{help reldist##kernel:{it:kernel}}{cmd:)}}type of kernel function; default is
    {cmd:kernel(gaussian)}
    {p_end}
{synopt:{opt na:pprox(#)}}grid size of approximation estimator; default
    is {cmd:napprox(512)}
    {p_end}
{synopt:{opt exact}}use the exact density estimator
    {p_end}
{synoptline}


{marker graph_opts}{col 5}{help reldist##graph_options:{it:graph_options}}{col 33}Description
{synoptline}
{syntab:Main}
{synopt:{opth ref:line(line_options)}}affect rendition
    of parity line
    {p_end}
{synopt:{opt noref:line}}suppress the parity line
    {p_end}

{syntab:After subcommand {cmd:pdf}}
{synopt:{it:{help cline_options}}}affect rendition of PDF line
  {p_end}
{synopt:{cmdab:hist:opts(}{help reldist##histopts:{it:options}}{cmd:)}}affect
    rendition of histogram bars
    {p_end}
{synopt:{opt nohist:ogram}}omit histogram bars
    {p_end}

{syntab:After subcommand {cmd:histogram}}
{synopt:{it:{help barlook_options}}}affect rendition of histogram bars
    {p_end}

{syntab:After subcommand {cmd:cdf}}
{synopt:{opt noorig:in}}do not add (0,0) coordinate
    {p_end}
{synopt:{it:{help cline_options}}}affect rendition of CDF line
    {p_end}

{syntab:Confidence intervals}
{synopt:{opt l:evel(#)}}set confidence level
    {p_end}
{synopt:{opt citrans:form}}plot transformed confidence intervals
    {p_end}
{synopt:{opt ci(name)}}obtain confidence intervals from {cmd:e(}{it:name}{cmd:)}
    {p_end}
{synopt:{opth ciopt:s(area_options)}}affect rendition of confidence intervals
    {p_end}
{synopt:{opt noci}}omit confidence intervals
    {p_end}

{syntab:Outcome labels}
{synopt:[{cmd:y}]{cmdab:olab:el}[{cmd:(}{help reldist##olabel:{it:spec}}{cmd:)}]}add outcome labels
    on secondary axis
    {p_end}
{synopt:[{cmd:y}]{cmdab:otic:k(}{help reldist##otick:{it:spec}}{cmd:)}}add outcome ticks
    on secondary axis
    {p_end}
{synopt:[{cmd:y}]{cmdab:oli:ne(}{help reldist##oline:{it:spec}}{cmd:)}}add outcome lines
    on secondary axis
    {p_end}
{synopt:[{cmd:y}]{cmdab:oti:tle(}{help title_options:{it:tinfo}}{cmd:)}}title for outcome scale axis
    {p_end}

{syntab:General graph options}
{synopt:{cmd:addplot(}{it:{help addplot_option:plot}}{cmd:)}}add other plots to the generated graph
  {p_end}
{synopt:{it:{help twoway_options}}}any options other than {cmd:by()} documented in help
    {it:{help twoway_options}}
  {p_end}
{synoptline}


{marker description}{...}
{title:Description}

{pstd}
    {cmd:reldist} provides a set of tools for relative distribution analysis. For
    background information see Handcock and Morris (1998, 1999). For methods and
    formulas used by {cmd:reldist} see
    {browse "http://ideas.repec.org/p/bss/wpaper/37.html":Jann (2020)}.

{phang}
    o  Command {cmd:reldist pdf} estimates the density function of the
    relative distribution, possibly including a histogram of the relative density.

{phang}
    o  Command {cmd:reldist histogram} estimates a histogram of the relative density.

{phang}
    o  Command {cmd:reldist cdf} estimates the relative distribution function. This is
    equivalent to a so-called probability-probability plot
     (see {helpb ppplot} from {stata ssc describe ppplot:SSC}).

{phang}
    o  Command {cmd:reldist divergence} estimates the Kullback-Leibler divergence (entropy),
    the Chi-squared divergence, or the dissimilarity index (total variation distance) of the
    relative distribution.

{phang}
    o  Command {cmd:reldist mrp} estimates the
    median relative polarization index (MRP), as well as its decomposition into a
    lower and and upper polarization index (LRP and URP).

{phang}
    o  Command {cmd:reldist summarize} computes summary statistics such as the mean
    or the median of the relative ranks, and can also be used to store the relative
    ranks in a new variable.

{phang}
    o  Command {cmd:reldist graph} plots the results after {cmd:reldist pdf},
    {cmd:reldist histogram}, or {cmd:reldist cdf}.

{phang}
    o  Command {cmd:predict} calculates the influence functions of the estimated
    statistics. Option {cmd:scores} is implied.

{pstd}
    There are two syntaxes:

{phang}
    o  In syntax 1, the distribution of {it:depvar} is
    compared between two groups defined by the {cmd:by()} option
    (two-sample relative distribution).

{phang}
    o  In syntax 2, the distribution of
    {it:depvar} is compared to the distribution of {it:refvar} within the same
    sample (paired relative distribution).

{pstd}
    {cmd:reldist} requires {cmd:moremata} to be installed on the system. See
    {net "describe moremata, from(http://fmwww.bc.edu/repec/bocode/m/)":{bf:ssc describe moremata}}.


{marker options}{...}
{title:Options}

{marker mainopts}{...}
{dlgtab:Main}

{phang}
    {opt by(groupvar)} specifies a binary variable that identifies the two groups
    to be compared. By default, the group with the lower value will be used as
    the reference group. {cmd:by()} is required in syntax 1 and not allowed
    in syntax 2.

{phang}
    {opt swap} reverses the order of the groups identified by {cmd:by()}. {opt swap} is
    only allowed in syntax 1.

{phang}
    {opt pooled} uses the pooled distribution across both groups as reference
    distribution. {opt pooled} is
    only allowed in syntax 1.

{marker balance}{...}
{phang}
    {cmd:balance(}{it:spec}{cmd:)} balances covariate distributions between the
    comparison group and the reference group using reweighting. {opt balance()}
    is only allowed in syntax 1. The syntax of {it:spec} is

            [{it:method}{cmd::}] {varlist} [{cmd:,} {it:options}]

{pmore}
    where {it:method} is either {cmd:ipw} for inverse probability weighting based
    on logistic regression (the default) or {cmd:eb} for entropy balancing (using
    {helpb mf_mm_ebal:mm_ebal()} from {helpb moremata}),
    {it:varlist} specifies the list of covariates to be balanced, and
    {it:options} are as follows:

{phang2}
    {opt ref:erence} reweights the reference group. The
    default is to reweight the comparison group. Option
    {cmd:pooled} is not allowed with {cmd:balance(, reference)}.

{phang2}
    {opt cont:rast} compares the balanced distribution with to the unbalanced
    distribution. Use this option to see how the balancing changes the
    distribution. If {cmd:contrast} is specified together with {cmd:reference},
    the balanced reference distribution will be used as the comparison
    distribution. If {cmd:contrast} is specified without {cmd:reference}, the
    balanced comparison distribution will be used as the reference
    distribution.

{phang2}
    {it:logit_options} are options to be passed through to {helpb logit}. {it:logit_options}
    are only allowed if {it:method} is {cmd:ipw}.

{phang2}
    {opt btol:erance(#)}, {it:#}>=0, specifies the tolerance for the entropy
    balancing algorithm. The default is {cmd:btolerance(1e-5)}. A warning
    message is displayed if a balancing solution is not within the specified
    tolerance. {cmd:btolerance()} is only allowed if {it:method} is {cmd:eb}.

{phang2}
    {opt noi:sily} displays the output of the balancing procedure.

{phang2}
    {opt gen:erate(newvar)} stores the balancing weights in variable
    {it:newvar}. This is useful if you want to check whether covariates have been
    successfully balanced. An example is as follows:

            . {stata sysuse nlsw88, clear}
{p 12 14 2}
            . {stata reldist summarize wage, by(union) balance(collgrad ttl_exp, generate(wbal))}
            {p_end}
{p 12 14 2}
            . {stata tabstat collgrad ttl_exp if wage<., by(union) nototal} (unbalanced)
            {p_end}
{p 12 14 2}
            . {stata tabstat collgrad ttl_exp [aw=wbal], by(union) nototal} (balanced)
            {p_end}

{pmore2}
    The balancing has been quite successful with respect to the means of the
    covariates. Perfect balancing (with respect to the means) can be achieved by entropy balancing:

            . {stata drop wbal}
{p 12 14 2}
            . {stata "reldist summarize wage, by(union) balance(eb:collgrad ttl_exp, generate(wbal))"}
            {p_end}
{p 12 14 2}
            . {stata tabstat collgrad ttl_exp [aw=wbal], by(union) nototal}
            {p_end}

{marker adjust}{...}
{phang}
    {opt adjust(spec)} applies location, scale, and shape adjustments to
    the comparison and reference distributions. {cmd:adjust()} is not allowed
    with {cmd:reldist mrp}. The syntax of {it:spec} is

            {it:adjust} [{cmd:,} {it:options}]

{pmore}
    where {it:adjust} specifies the desired adjustments. {it:adjust} may contain
    any combination of at most two of the following keywords:

            {cmdab:l:ocation}   adjust location
            {cmdab:sc:ale}      adjust scale
            {cmdab:sh:ape}      adjust shape

{pmore}
    By default, the specified adjustments are applied to the comparison
    distribution. However, a colon may be included
    in {it:adjust} to distinguish between distributions: Keywords before the
    colon affect the comparison distribution; keywords after the colon affect
    the reference distribution. For example, type
    {cmd:adjust(location scale)} to adjust the location and scale of the
    comparison distribution. Likewise, you could type {cmd:adjust(:location scale)} to
    adjust the reference distribution. Furthermore,
    {cmd:adjust(location : shape)} would adjust the location of the comparison
    distribution and the shape of the reference distribution. {it:options} are
    as follows:

{phang2}
    {opt mean} uses the mean for the location adjustment. The default is to
    use the median.

{phang2}
    {opt sd} uses the standard deviation for the scale adjustment. The default is
    to use the IQR (interquartile range).

{phang2}
    {opt mult:iplicative} uses a multiplicative adjustment instead of an additive
    adjustment. {it:adjust} may only contain one keyword in this case, either
    {cmd:location} or {cmd:shape}. Error will be returned if the
    location ratio between the comparison distribution and the reference
    distribution is not strictly positive.

{phang2}
    {opt log:arithmic} performs the adjustments on logarithmically transformed
    data. Error will be returned if the data is not strictly positive.

{marker rankopts}{...}
{phang}
    {it:rank_options} specify the details about the computation of relative
    ranks. These options are irrelevant for {cmd:reldist histogram},
    {cmd:reldist cdf}, {cmd:reldist divergence} unless option {cmd:pdf}
    is specified, and {cmd:reldist pdf} if {cmd:discrete} or {cmd:categorical}
    is specified. The options are as follows:

{phang2}
    {opt nobr:eak} changes how the relative ranks are computed in case of ties. By
    default, {cmd:reldist} breaks ties for comparison values that have
    ties in the reference distribution (in ascending order of weights, if
    weights have been specified). This leads to improved results if there is
    heaping in the data. Specify {cmd:nobreak} to omit breaking ties.

{phang2}
    {opt nomid} changes how the relative ranks are computed in case of ties. By
    default, {cmd:reldist} uses midpoints of the steps in the cumulative
    distribution for comparison values that have ties in the
    reference distribution. This ensures that the average
    relative rank is equal to 0.5 if the comparison and reference distributions
    are identical. Specify {cmd:nomid} to assign relative ranks based on full
    steps in the CDF.

{phang2}
    {opt desc:ending} sorts tied observations in descending order of
    weights. The default is to use ascending sort order. Option
    {opt descending} has no effect on results if {cmd:nobreak} is specified or
    if there are no weights.

{phang2}
    {opt nosta:ble} breaks ties randomly (within unique values of weights). The
    default is to break the ties in the sort order of the data (within unique
    values of weights). Option {opt nostable} has no effect on the results
    reported by {cmd:reldist}. It may, however, affect the ranks stored by
    option {cmd:generate()} or the influence functions stored by {cmd:predict}
    (unless option {opt nobreak} is specified).

{phang}
    {opt replace} allows replacing existing variables. This is relevant for
    {cmd:generate()} with {cmd:reldist summarize} and {cmd:generate()}
    in {cmd:balance()}.

{marker pdfopts}{...}
{dlgtab:For subcommand -pdf-}

{phang}
    {opt n(#)} sets the number of evaluation points for which the PDF is to
    be computed. A regular grid of {it:#} evaluation points between 0 and 1 will
    be used. The default is {cmd:n(101)} (unless option {cmd:discrete}
    or {cmd:categorical} is specified, in which case {cmd:n()} has no
    default). Only one of {cmd:n()}, {cmd:at()}, and {cmd:atx()} is allowed.

{phang}
    {cmd:at(}{it:numlist}|{it:matname}{cmd:)} specifies a custom grid of
    evaluation points between 0 and 1, either by providing a
    {help numlist:{it:numlist}} or the name of a matrix containing the values
    (the values will be taken from the first row or the first column of the matrix,
    depending on which is larger). Only
    one of {cmd:n()}, {cmd:at()}, and {cmd:atx()} is allowed.

{marker pdfatx}{...}
{phang}
    {cmd:atx}[{cmd:(}{cmdab:comp:arison}|{cmdab:ref:erence}|{it:numlist}|{it:matname}{cmd:)}],
    specified without argument, causes the relative PDF to be evaluated at each
    distinct outcome value that exists in the data (possibly after applying
    {cmd:adjust()}), instead of using a regular evaluation grid on the
    probability scale. All outcome values across both distributions will be
    considered. To restrict the evaluation points to outcome values from the
    comparison distribution or from the reference distribution, specify
    {cmd:atx(comparison)} or {cmd:atx(reference)}, respectively. Alternatively,
    specify a grid of custom values, either by providing a
    {help numlist:{it:numlist}} or the name of a matrix containing the values
    (the values will be taken from the first row or the first column of the
    matrix, depending on which is larger). Only one of {cmd:n()}, {cmd:at()},
    and {cmd:atx()} is allowed.

{phang}
    {cmd:discrete} causes the data to be treated as discrete. The relative PDF
    will then be evaluated at each level of the data as the ratio of the
    level's frequency between the comparison distribution and the reference
    distribution instead of using kernel density estimation, and the result
    will be displayed as a step function. If option {cmd:n()} or {cmd:at()} is
    specified, the step function will be evaluated at the points of the
    corresponding probability grid instead of returning the relative density
    for each outcome level. Options {cmd:nobreak}, {cmd:nomid}, {cmd:descending}, and
    {help reldist##density_options:{it:density_options}} have no effect if
    {cmd:discrete} is specified. Furthermore, options {cmd:histogram()} and
    {cmd:adjust()} are not allowed.

{phang}
    {cmd:categorical} is like {cmd:discrete}, but additionally requests that
    the data only contains positive integers. Factor-variable notation will be used to
    label the coefficient in the output table.

{phang}
    {cmd:histogram}[{cmd:(}{it:#}{cmd:)}] computes a histogram in
    addition to the PDF, where {it:#} is the number of bins. If {it:#} is omitted,
    10 bins will be used.

{phang}
    {cmd:alt} uses an alternative estimation method for the histogram;
    see {help reldist##histopts:histogram options} below.

{marker density_options}{...}
{phang}
    {it:density_options} set the details of kernel density estimation. The options
    are as follows:

{marker bwidth}{...}
{phang2}
    {cmd:bwidth(}{it:#}|{it:method}[{cmd:,} {cmd:nord}]{cmd:)} determines the bandwidth of the kernel, the
    halfwidth of the estimation window around each evaluation point. Use
    {opt bwidth(#)}, {it:#} > 0, to set the bandwidth to a specific value. Alternatively,
    type {opt bwidth(method)} to choose an automatic bandwidth selection
    method. Choices are {cmdab:s:ilverman} (optimal of Silverman),
    {cmdab:n:ormalscale} (normal scale rule), {cmdab:o:versmoothed}
    (oversmoothed rule), {opt sj:pi} (Sheather-Jones solve-the-equation plug-in),
    {cmdab:d:pi}[{cmd:(}{it:#}{cmd:)}] (Sheather-Jones direct plug-in, where {it:#}
    specifies the number of stages of functional estimation; default is 2), or
    {opt isj} (diffusion estimator bandwidth). The default
    is {cmd:bw(sjpi)}.

{pmore2}
    By default, if estimating the density of the relative data, all bandwidth selectors
    include a correction for relative data based on Cwik and Mielniczuk (1993). Specify
    suboption {cmd:nord} to omit the correction.

{phang2}
    {opt bwadjust(#)} multiplies the bandwidth by
    {it:#}, where {it:#} > 0. Default is {cmd:bwadjust(1)}.

{marker boundary}{...}
{phang2}
    {opt boundary(method)} sets the type of boundary correction method. Choices are
    {opt ren:orm} (renormalization method; the default), {opt refl:ect} (reflection method), or
    {opt lc} (linear combination technique).

{phang2}
    {opt adaptive(#)} specifies the number of iterations used by the adaptive
    kernel density estimator. The default is {cmd:adaptive(0)} (non-adaptive
    density estimator).

{marker kernel}{...}
{phang2}
    {opt kernel(kernel)} specifies the kernel function to be used. {it:kernel} may
    be {opt e:panechnikov} (Epanechnikov kernel function),
    {opt epan2} (alternative Epanechnikov kernel function),
    {opt b:iweight} (biweight kernel function),
    {opt triw:eight} (triweight kernel function),
    {opt c:osine} (cosine trace),
    {opt g:aussian} (Gaussian kernel function),
    {opt p:arzen} (Parzen kernel function),
    {opt r:ectangle} (rectangle kernel function)
    or {opt t:riangle} (triangle kernel function). The default is
    {cmd:kernel(gaussian)}.

{phang2}
    {opt napprox(#)} specifies the grid size used by the binned approximation
    density estimator (and by the data-driven bandwidth selectors). The default
    is {cmd:napprox(512)}.

{phang2}
    {cmd:exact} causes the exact kernel density estimator to be used instead
    of the binned approximation estimator. The exact estimator can be slow in large
    datasets, if the density is to be evaluated at many points.

{phang}
    {opt graph}[{cmd:(}{help reldist##graph_opts:{it:graph_options}}{cmd:)}]
    displays the results in a graph. The coefficients table will be suppressed
    in this case (unless option {cmd:table} is specified). Alternatively, use
    command {cmd:reldist graph} to display the graph after estimation.

{phang}
    {opt ogrid(#)} sets the size of the approximation grid for outcome
    labels. The default is {cmd:ogrid(401)}. The grid is stored in
    {cmd:e(ogrid)} and will be used by graph option
    {helpb reldist##olabel:olabel()} to determine the positions of outcome
    labels. Type {cmd:noogrid} to omit the computation of the grid (no outcome
    labels will then be available for the graph). Option {cmd:ogrid()} is only
    allowed if the relative density is computed with respect an evaluation grid
    on the probability scale. If the relative density is evaluated with respect to
    specific outcome values (e.g. if {cmd:atx()} is specified), the outcome
    labels will be obtained from the information stored in {cmd:e(at)}.

{marker histopts}{...}
{dlgtab:For subcommand -histogram-}

{phang}
    {opt n(#)} specifies the number of histogram bars. The reference distribution
    will be divided into {it:#} bins of equal width. That is, each bin will
    cover 1/{it:#}th of the reference distribution. The default is {cmd:n(10)}.

{phang}
    {cmd:alt} uses an alternative estimation method. The default method obtains
    the relative histogram by computing the empirical CDFs of both distributions at
    all values that exist in the data (across both distributions). The
    alternative method obtains the relative histogram based on the empirical CDF of
    the relative ranks. In both cases, if necessary, linear interpolation will be used
    to map the relative CDF to the evaluation points.

{phang}
    {cmd:discrete} causes the data to be treated as discrete. The relative density
    will then be evaluated at each level of the data as the ratio of the
    level's frequency between the two distributions and the width of bars will be
    proportional to the reference distribution. Option {cmd:alt} has no effect and
    options {cmd:n()} and {cmd:adjust()} are not allowed if {cmd:discrete} is specified.

{phang}
    {cmd:categorical} is like {cmd:discrete}, but additionally requests that
    the data only contains positive integers. Factor-variable notation will be used to
    label the coefficient in the output table.

{phang}
    {opt graph}[{cmd:(}{help reldist##graph_opts:{it:graph_options}}{cmd:)}]
    displays the results in a graph. The coefficients table will be suppressed
    in this case (unless option {cmd:table} is specified). Alternatively, use
    command {cmd:reldist graph} to display the graph after estimation.

{phang}
    {opt ogrid(#)} sets the size of the approximation grid for outcome
    labels. The default is {cmd:ogrid(401)}. The grid is stored in {cmd:e(ogrid)} and
    will be used by graph option {helpb reldist##olabel:olabel()} to determine
    the positions of outcome labels. Type {cmd:noogrid} to omit the computation
    of the grid (no outcome labels will then be available for the graph). {cmd:ogrid()}
    is not allowed together with {cmd:discrete} or {cmd:categorical}.

{marker cdfopts}{...}
{dlgtab:Subcommand -cdf-}

{phang}
    {opt n(#)} sets the number of evaluation points for which the CDF is to
    be computed. A regular grid of {it:#} evaluation points between 0 and 1 will
    be used. The default is {cmd:n(101)} (unless option {cmd:discrete}
    or {cmd:categorical} is specified, in which case {cmd:n()} has no
    default). Only one of {cmd:n()}, {cmd:at()}, and
    {cmd:atx()} is allowed.

{phang}
    {cmd:at(}{it:numlist}|{it:matname}{cmd:)} specifies a custom grid of
    evaluation points between 0 and 1, either by providing a
    {help numlist:{it:numlist}} or the name of a matrix containing the values
    (the values will be taken from the first row or the first column of the matrix,
    depending on which is larger). Only one of {cmd:n()}, {cmd:at()}, and {cmd:atx()}
    is allowed.

{marker cdfatx}{...}
{phang}
    {cmd:atx}[{cmd:(}{cmdab:comp:arison}|{cmdab:ref:erence}|{it:numlist}|{it:matname}{cmd:)}],
    specified without argument, causes the relative CDF to be evaluated at each
    distinct outcome value that exists in the data (possibly after applying
    {cmd:adjust()}), instead of using a regular evaluation grid on the
    probability scale. All outcome values across both distributions will be
    considered. To restrict the evaluation points to outcome values from the
    comparison distribution or from the reference distribution, specify
    {cmd:atx(comparison)} or {cmd:atx(reference)}, respectively. Alternatively,
    specify a grid of custom values, either by providing a
    {help numlist:{it:numlist}} or the name of a matrix containing the values
    (the values will be taken from the first row or the first column of the
    matrix, depending on which is larger). Only one of {cmd:n()}, {cmd:at()},
    and {cmd:atx()} is allowed.

{phang}
    {cmd:alt} uses an alternative estimation method. The default method obtains
    the relative CDF by computing the empirical CDFs of both distributions at
    all values that exist in the data (across both distributions). The
    alternative method obtains the relative CDF based on the empirical CDF of
    the relative ranks. In both cases, if necessary, linear interpolation will be used
    to map the relative CDF to the evaluation points.

{phang}
    {cmd:discrete} causes the data to be treated as discrete. The relative CDF
    will then be evaluated at each observed outcome value instead of using an
    evaluation grid on the probability scale. Option {cmd:discrete} leads to the
    same result as specifying {cmd:atx}. Option {cmd:adjust()} is not allowed
    if {cmd:discrete} is specified.

{phang}
    {cmd:categorical} is like {cmd:discrete}, but additionally requests that
    the data only contains positive integers. Factor-variable notation will be used to
    label the coefficient in the output table.

{phang}
    {opt graph}[{cmd:(}{help reldist##graph_opts:{it:graph_options}}{cmd:)}]
    displays the results in a graph. The coefficients table will be suppressed
    in this case (unless option {cmd:table} is specified). Alternatively, use
    command {cmd:reldist graph} to display the graph after estimation.

{phang}
    {opt ogrid(#)} sets the size of the approximation grid for outcome
    labels. The default is {cmd:ogrid(401)}. The grid is stored in
    {cmd:e(ogrid)} and will be used by graph option
    {helpb reldist##olabel:olabel()} to determine the positions of outcome
    labels. Type {cmd:noogrid} to omit the computation of the grid (no outcome
    labels will then be available for the graph). Option {cmd:ogrid()} is only
    allowed if the relative CDF is computed with respect an evaluation grid
    on the probability scale. If the relative CDF is evaluated with respect to
    specific outcome values (e.g. if {cmd:atx()} is specified), the outcome
    labels will be obtained from the information stored in {cmd:e(at)}.

{marker divopts}{...}
{dlgtab:For subcommand -divergence-}

{phang}
    {cmd:over(}{help varname:{it:overvar}}{cmd:)} computes results for each subpopulation defined
    by the values of {it:overvar}.

{phang}
    {opt entropy} or {opt kl} computes the Kullback-Leibler divergence (entropy) of the
    relative distribution. This is the default.

{phang}
    {opt chi2} or {opt chisquared} computes the Chi-squared divergence of the
    relative distribution.

{phang}
    {opt tvd} or {opt dissimilarity} computes the dissimilarity index (total variation distance) of the
    relative distribution.

{phang}
    {opt all} computes all supported divergence measures. {opt all} is equivalent to
    {cmd:entropy chi2 tvd}.

{phang}
    {opt n(#)} specifies the number of histogram bars or, if option {cmd:pdf} is specified, the
    number of kernel density evaluation points used to estimate the relative distribution. The
    default is {cmd:n(20)} or, if option {cmd:pdf} is specified, {cmd:n(100)}.

{phang}
    {cmd:alt} uses an alternative estimation method for the histogram density;
    see {help reldist##histopts:histogram options} above.

{phang}
    {opt pdf} computes the divergence measures based on a kernel density estimate instead of a
    histogram estimate.

{phang}
    {it:density_options} set the details of the the kernel density estimation. This is
    only relevant if option {cmd:pdf} is specified. See {help reldist##density_options:{it:density_options}}
    above for available options.

{phang}
    {cmd:discrete} causes the data to be treated as discrete. The relative density
    will then be evaluated at each level of the data as the ratio of the
    level's frequency between the two distributions. Option {cmd:alt} has no effect and
    options {cmd:n()}, {cmd:pdf}, and {cmd:adjust()} are not allowed if {cmd:discrete} is specified.

{phang}
    {cmd:categorical} is like {cmd:discrete}, but additionally requests that
    the data only contains positive integers.

{marker divcom}{...}
{phang}
    {cmd:compare}[{cmd:(}{it:options}{cmd:)}] estimates divergence measures for
    two models of the relative distribution, a main model and an alternative model,
    and also reports the difference between the two variants. {it:options} are
    {helpb reldist##balance:balance()} and {helpb reldist##adjust:adjust()} as described
    above. {cmd:balance()} and {cmd:adjust()} specified as main options are applied
    to the main model; {cmd:balance()} and {cmd:adjust()} specified within
    {cmd:compare()} are applied to the alternative model.

{marker mrpopts}{...}
{dlgtab:For subcommand -mrp-}

{phang}
    {cmd:over(}{help varname:{it:overvar}}{cmd:)} computes results for each subpopulation defined
    by the values of {it:overvar}.

{phang}
    {opt multiplicative} applies multiplicative location adjustment. The
    default is to use additive adjustment. Only one of
    {cmd:logarithmic} and {cmd:multiplicative} is allowed.

{phang}
    {opt logarithmic} causes the location (and, optionally, scale)
    adjustment to be performed on the logarithmic scale. Only one of
    {cmd:logarithmic} and {cmd:multiplicative} is allowed.

{phang}
    {opt scale}[{cmd:(sd)}] adjusts the scale of the data before
    computing the polarization indices. If {cmd:scale} is specified without argument,
    the IQR (interquartile range) will be used; that is, the scale of the data will be
    adjusted such that the IQR is the same in both distributions. Specify
    {cmd:scale(sd)} to use the standard deviation instead of the IQR. {cmd:scale()}
    is not allowed if {cmd:multiplicative} is specified.

{marker sumopts}{...}
{dlgtab:For subcommand -summarize-}

{phang}
    {cmd:over(}{help varname:{it:overvar}}{cmd:)} computes results for each subpopulation defined
    by the values of {it:overvar}.

{marker statistics}{...}
{phang}
    {cmd:statistics(}{it:statname} [{it:...}]{cmd:)} specifies a space separated list of
    summary statistics to be reported. The default is {cmd:statistics(mean)}. The
    following summary statistics are supported:

{p2colset 13 25 27 2}{...}
{p2col:{opt m:ean}}mean{p_end}
{p2col:{opt v:ariance}}variance{p_end}
{p2col:{opt sd}}standard deviation{p_end}
{p2col:{opt med:ian}}median; equivalent to {cmd:p50}{p_end}
{p2col:{opt p}{it:#}}{it:#}th percentile, where {it:#} is an integer between 1 and 99{p_end}
{p2col:{opt iqr}}interquartile range ({cmd:p75}-{cmd:p25}){p_end}

{phang}
    {opth generate(newvar)} stores the relative ranks in
    variable {it:newvar}. Depending on {cmd:adjust()}, different observations
    may be filled in.

{marker seopts}{...}
{dlgtab:SE/CI}

{phang}
    {opt level(#)} specifies the confidence level, as a percentage, for
    confidence intervals. The default is {cmd:level(95)} or as set by
    {helpb set level}.

{marker vce}{...}
{phang}
    {opth vce(vcetype)} determines how standard errors and confidence intervals
    are computed. {it:vcetype} may be:

            {cmdab:a:nalytic} [{cmd:,} {help reldist##density_options:{it:density_options}}]
            {cmdab:cl:uster} {it:clustvar} [{cmd:,} {help reldist##density_options:{it:density_options}}]
            {cmdab:svy} [{help svy##svy_vcetype:{it:svy_vcetype}}] [{cmd:,} {help svy##svy_options:{it:svy_options}} {help reldist##density_options:{it:density_options}}]
            {cmdab:boot:strap} [{cmd:,} {help bootstrap:{it:bootstrap_options}}]
            {cmdab:jack:knife} [{cmd:,} {help jackknife:{it:jackknife_options}}]

{pmore}
    The default is {cmd:vce(analytic)}, which computes the standard errors based
    on influence functions. Likewise, {bind:{cmd:vce(cluster} {it:clustvar}{cmd:)}} computes
    influence-function based standard errors allowing for intragroup correlation,
    where {it:clustvar} specifies to which group each observation
    belongs. In both cases, {help reldist##density_options:{it:density_options}} specify
    specify how auxiliary densities are estimated during the computation of the
    influence functions (option {cmd:boundary()} will have no effect; unbounded support
    is assumed for auxiliary densities).

{pmore}
    {cmd:vce(svy)} computes standard errors taking the survey design as set by
    {helpb svyset} into account. The syntax is equivalent to the syntax of the {helpb svy}
    prefix command; that is, {cmd:vce(svy)} is {cmd:reldist}'s way to support
    the {helpb svy} prefix. If {help svy##svy_vcetype:{it:svy_vcetype}} is set to {cmd:linearized}, the
    standard errors are estimated based on influence functions; use
    {help reldist##density_options:{it:density_options}} to specify the details
    of auxiliary density estimation in this case. For
    {help svy##svy_vcetype:{it:svy_vcetype}} other than {cmd:linearized}, {it:density_options}
    are not allowed.

{pmore}
    {cmd:vce(bootstrap)} and {cmd:vce(jackknife)} compute standard errors using
    {helpb bootstrap} or {helpb jackknife}, respectively; see help {it:{help vce_option}}.

{pmore}
    If a replication technique is used for standard error estimation,
    i.e. {cmd:vce(bootstrap)}, {cmd:vce(jackknife)}, {cmd:vce(svy)} with
    {help svy##svy_vcetype:{it:svy_vcetype}} other than {cmd:linearized},
    the bandwidth used by {cmd:reldist pdf} will be held fixed across
    replications (that is, if relevant, the bandwidth will be determined upfront
    and then held constant). If you want to repeat bandwidth
    search in each replication, use {helpb bootstrap}, {helpb jackknife}, or {helpb svy}
    as a prefix command.

{phang}
    {opt nose} prevents {cmd:reldist} from computing standard errors. This saves computer time.

{marker reprtopts}{...}
{dlgtab:Reporting}

{phang}
    {opt citransform} reports transformed confidence intervals
    depending on the type of the reported statistics (log
    transform for PDF and histogram density, logit transform for CDF and
    descriptive statistics, inverse hyperbolic tangent transform for
    polarization indices). {opt citransform} only has an effect in Stata 15 or
    newer.

{phang}
    {opt noheader} suppress the output header.

{phang}
    {cmd:notable} suppresses the output table containing the estimated
    coefficients. {cmd:table} enforces displaying the table.

{marker display_opts}{...}
{phang}
    {it:display_options} are standard reporting options such as {cmd:cformat()} or
    {cmd:coeflegend}; see the Reporting options
    in {helpb estimation options:[R] Estimation options}.


{marker graph_options}{...}
{title:Options for reldist graph}

{dlgtab:Main}

{phang}
    {opt refline(line_options)} specifies options to affect
    the rendition of the parity line. See help {it:{help line_options}}.

{phang}
    {opt norefline} suppresses the parity  line.

{dlgtab:After subcommand -pdf-}

{phang}
    {it:cline_options} affect the rendition of the PDF line. See
    help {it:{help cline_options}}.

{phang}
    {opt histopts(options)} specifies options to affect the
    rendition of the histogram bars (if a histogram was computed) and the
    corresponding confidence spikes. {it:options} are as follows:

{phang2}
    {it:barlook_options} affect the rendition of the histogram bars. See
    help {it:{help barlook_options}}.

{phang2}
    {opt ciopts(rcap_options)} specifies options to affect the
    rendition of the confidence spikes of the histogram bars. See help
    {it:{help rcap_options}}.

{phang2}
    {opt noci} omits the confidence spikes of the histogram bars.

{phang}
    {opt nohistogram} omits the histogram bars.

{dlgtab:After subcommand -histogram-}

{phang}
    {it:barlook_options} affect the rendition of the histogram bars. See
    help {it:{help barlook_options}}.

{dlgtab:After subcommand -cdf-}

{phang}
    {opt noorigin} prevents adding a (0,0) coordinate to the plotted
    line. If the first X-coordinate of the CDF is larger
    than zero and the range of the CDF has not been restricted by {cmd:at()} or
    {cmd:atx()}, {cmd:reldist graph} will automatically add a
    (0,0) coordinate to the plot. Type {opt noorigin}
    to override this behavior.

{phang}
    {it:cline_options} affect the rendition of the CDF line. See
    help {it:{help cline_options}}.

{dlgtab:Confidence intervals}

{phang}
    {opt level(#)} specifies the confidence level, as a percentage, for
    confidence intervals.

{phang}
    {opt citransform} plots transformed confidence intervals
    depending on the type of the reported statistics (log
    transform for PDF and histogram density, logit transform for CDF).

{phang}
    {opt ci(name)} obtains the confidence intervals from
    {cmd:e(}{it:name}{cmd:)} instead of computing them from
    {cmd:e(V)}. {cmd:e(}{it:name}{cmd:)} must contain two rows and
    the same number of columns as {cmd:e(b)}. For example, after
    bootstrap estimation, you could type {cmd:ci(ci_percentile)} to plot
    percentile confidence intervals. {cmd:ci()} and
    {cmd:level()} are not both allowed.

{phang}
    {opt ciopts(options)} specifies options to affect the
    rendition of the confidence intervals. See
    help {it:{help area_options}} or, after, {cmd:reldist histogram}
    help {it:{help rcap_options}}. Use option {cmd:recast()} to change the
    plot type used for confidence intervals. For example, type
    {cmd:ciopts(recast(rline))} to use two lines instead of an area.

{phang}
    {opt noci} omits the confidence intervals.

{dlgtab:Outcome labels}

{marker olabel}{...}
{phang}
    [{cmd:y}]{cmd:olabel}[{cmd:(}{it:spec}{cmd:)}] adds outcome labels on a secondary
    axis. {cmd:olabel()} adds outcome labels for the reference distribution; {cmd:yolabel()}
    adds outcome labels for the comparison distribution (only allowed after
    {cmd:reldist cdf}). The syntax of {it:spec} is

{p 12 17 2}
        [ {cmd:#}{it:#} | {it:{help numlist}} ] [{cmd:,}
        {c -(} {cmd:noprune} | {opt prune(mindist)} {c )-}
        {cmd:at} {opth for:mat(%fmt)} {it:suboptions} ]

{phang2}
    {cmd:#}{it:#} requests that (approximately) {it:#} outcome labels be
    added at (approximately) evenly-spaced positions; the default is
    {cmd:#}6. Alternatively, specify {it:numlist} to generate labels for
    given outcome values.

{phang2}
    {opt prune(mindist)} requests that an outcome label (but not its tick) is to
    be omitted if its distance to the preceding label is less than {it:mindist}
    (an exception are labels that have the same position;
    in such a case the largest label will be printed). The default is
    {cmd:prune(0.1)}; type {cmd:prune(0)} or {cmd:noprune} to print labels at
    all positions. The difference between {cmd:prune(0)} and {cmd:noprune} is
    that {cmd:prune(0)} will only print one label per position whereas
    {cmd:noprune} prints all labels, including labels that have the same
    position.

{phang2}
    {cmd:at} causes {it:numlist} to be interpreted as a list of probabilities for which
    outcome labels are to be determined. Labels obtained this way will not be pruned.

{phang2}
    {opt format(%fmt)} specifies the display format for the outcome labels. Default
    is {cmd:format(%6.0g)}. See help {helpb format} for available formats.

{phang2}
    {it:suboptions} are as described in help {it:{help axis_label_options}}.

{pmore}
    Option [{cmd:y}]{cmd:olabel()} may be repeated. Use suboptions {cmd:add}
    and {cmd:custom} to generate multiple sets of labels
    with different rendering; see {it:{help axis_label_options}}.

{marker otick}{...}
{phang}
    [{cmd:y}]{opt otick(spec)} adds outcome ticks on a secondary axis.
    {cmd:otick()} adds outcome ticks for the reference distribution;
    {cmd:yotick()} adds outcome ticks for the comparison distribution (only
    allowed after {cmd:reldist cdf}). The syntax of {it:spec} is

            {it:{help numlist}} [{cmd:,} {it:suboptions} ]

{pmore}
    where {it:numlist} specifies the outcome values for which ticks be generated
    and {it:suboptions} are as described in help
    {it:{help axis_label_options}}. Option [{cmd:y}]{cmd:otick()} may be
    repeated. Use suboptions {cmd:add} and {cmd:custom} to generate multiple
    sets of ticks with different rendering; see {it:{help axis_label_options}}.

{marker oline}{...}
{phang}
    [{cmd:y}]{opt oline(spec)} draws added lines at the positions of
    the specified outcome values on a secondary axis. {cmd:oline()} adds
    outcome lines for the reference distribution; {cmd:yoline()} adds outcome
    lines for the comparison distribution (only allowed after
    {cmd:reldist cdf}). The syntax of {it:spec} is

            {it:{help numlist}} [{cmd:,} {it:suboptions} ]

{pmore}
    where {it:numlist} specifies the outcome values for which added lines be generated
    and {it:suboptions} are as described in help
    {it:{help added_line_options}}. Option [{cmd:y}]{cmd:oline()} may be repeated
    to draw multiple sets of lines with different rendering.

{phang}
    [{cmd:y}]{opt otitle(tinfo)} provides a title for the outcome scale
    axis; see help {it:{help title_options}}. {cmd:otitle()} is for the reference
    distribution; {cmd:yotitle()} is for the comparison distribution (only
    allowed after {cmd:reldist cdf}).

{pstd}
    Technical note: The positions of the outcome labels, ticks, or lines are
    computed from information stored by {cmd:reldist} in {cmd:e()}, either
    from the quantiles stored in {cmd:e(ogrid)} or from the values stored
    in {cmd:e(at)}, depending on context. There is an undocumented command
    called {cmd:reldist olabel} that can be
    used to compute the positions after the relative distribution has been
    estimated. Use this command, for example, if you want to draw a custom
    graph from the stored results without applying {cmd:reldist graph}. The
    syntax is as follows:

{p 8 17 2}
    {cmd:reldist} {opt olab:el} [ {cmd:#}{it:#} | {it:{help numlist}} ] [{cmd:,}
        {c -(} {cmd:noprune} | {opt prune(mindist)} {c )-} {cmd:at} {opth for:mat(%fmt)}
        {opth tic:k(numlist)} {opth li:ne(numlist)} {opt y} ]

{pstd}
    where {cmd:#}{it:#} or {it:numlist} specifies the (number of) values for
    which labels be generated, {cmd:prune()} determines the
    pruning (see above), {cmd:at} changes the meaning of the main {it:numlist} (see above),
    {cmd:format()} specifies the display format for the labels, {cmd:tick()}
    specifies values for which ticks be generated, {cmd:line()}
    specifies values for which added lines be generated, and {cmd:y} request outcome labels
    for the Y axis of the relative CDF (only allowed after
    {cmd:reldist cdf}). The command returns the following macros in {cmd:r()}:

{p2colset 9 22 22 2}{...}
{p2col:{cmd:r(label)}}label specification for use in an {helpb axis_label_options:xlabel()} option
    {p_end}
{p2col:{cmd:r(label_x)}}expanded and sorted {it:numlist}
    {p_end}
{p2col:{cmd:r(tick)}}tick specification for use in an {helpb axis_label_options:xtick()} option
    {p_end}
{p2col:{cmd:r(tick_x)}}expanded and sorted {it:numlist} from {cmd:tick()}
    {p_end}
{p2col:{cmd:r(line)}}line specification for use in an {helpb added_line_options:xline()} option
    {p_end}
{p2col:{cmd:r(line_x)}}expanded and sorted {it:numlist} from {cmd:line()}
    {p_end}

{dlgtab:General graph options}

{phang}
    {opt addplot(plot)} provides a way to add other plots to the
    generated graph. See help {it:{help addplot_option}}.

{phang}
    {it:twoway_options} are any options other than {cmd:by()} documented in help
    {it:{help twoway_options}}.


{marker examples}{...}
{title:Examples}

{dlgtab:Relative density and histogram}

{pstd}
    Compare the wages of unionized and non-unionized workers:

        . {stata sysuse nlsw88, clear}
        . {stata reldist pdf wage, by(union)}
{p 8 12 2}
        . {stata reldist graph, ciopts(recast(rline) lp(dash) pstyle(p1))}

{pstd}
    To get an idea of the hourly wages that correspond to different positions in
    the reference distribution, we can add outcome labels using the {cmd:olabel}
    option:

{p 8 12 2}
        . {stata reldist graph, olabel otitle(hourly wage)}

{pstd}
    We can also provide a list of custom values for which outcome labels be
    generated:

{p 8 12 2}
        . {stata reldist graph, olabel(1(1)40) otitle(hourly wage)}

{pstd}
    By default, labels that are close together will not be printed. Type
    {cmd:olabel(1(1)40, noprune)} to print all 40 labels. Alternatively use suboption
    {cmd:prune()} to set the minimum distance between labels
    (default is {cmd:prune(0.1)}). Example:

{p 8 12 2}
        . {stata reldist graph, olabel(1(1)40, prune(0.04)) otitle(hourly wage)}

{pstd}
    To include a histogram in addition to the density curve, type:

{p 8 12 2}
        . {stata reldist pdf wage, by(union) histogram graph(ciopts(fc(%50) lc(%0)))}

{pstd}
    Or, to display only the histogram:

        . {stata reldist histogram wage, by(union) graph}

{pstd}
    Since unionized workers have, on average, higher wages than non-unionized workers,
    we might want to adjust the location of their wage distribution to see
    the difference in the distributional shape between the two groups, net of the
    difference in location:

{p 8 12 2}
        . {stata reldist pdf wage, by(union) adjust(location) graph(ciopts(fc(%50) lc(%0)))}

{pstd}
    Interestingly, the distribution among unionized workers appears more polarized, at
    least in the lower part of the distribution. However, this may be a
    misleading result because, by default, {cmd:reldist} performs an
    additive location shift (pushing low-wage earners among the unionized
    to the very bottom of the distribution). Since wages can only be positive,
    it probably makes more sense to use a multiplicative shift (i.e., to rescale the
    wages proportionally):

{p 8 12 2}
        . {stata reldist pdf wage, by(union) adjust(location, multiplicative) graph(ciopts(fc(%50) lc(%0)))}

{pstd}
    We now see that the distribution among the unionized is less polarized than the
    distribution among non-unionized workers, especially in the upper part of the
    distribution.

{dlgtab:Cumulative distribution}

{pstd}
    The cumulative relative distribution can be graphed as follows:

        . {stata sysuse nlsw88, clear}
{p 8 12 2}
        . {stata reldist cdf wage, by(union) nose graph(olab(1(1)40) yolab(1(1)40) aspectratio(1))}

{pstd}
    After applying a multiplicative location shift, the relative distribution
    looks as follows:

{p 8 12 2}
        . {stata reldist cdf wage, by(union) nose adjust(location, multiplicative) graph}

{dlgtab:Median relative polarization}

{pstd}
    An analysis of relative polarization between unionized and non-unionized workers by
    qualification leads to the following results:

        . {stata sysuse nlsw88, clear}
{p 8 12 2}
        . {stata reldist mrp wage, by(union) over(collgrad)}

{pstd}
    It appears that wage polarization is more pronounced among unionized workers than among the
    non-unionized workers in the group of people without college degree. In the group of
    people with college degree, polarization is higher among non-unionized workers. Again, this might
    be an artifact due to the default additive location adjustment imposed by the MRP. A multiplicative
    adjustment (i.e. rescaling wages proportionally) probably makes more sense:

{p 8 12 2}
        . {stata reldist mrp wage, by(union) over(collgrad) multiplicative}

{pstd}
    We now see that polarization is generally larger among the non-unionized workers, but there
    are still some interesting differences. For people without college degree the wage distribution
    is compressed mostly at the top, for people with college degree the
    compression appears more pronounced in the lower part of the distribution.

{dlgtab:Summary statistics}

{pstd}
    To obtain the mean an median rank of unionized workers in the
    wage distribution of non-unionized workers by
    qualification, type:

        . {stata sysuse nlsw88, clear}
{p 8 12 2}
        . {stata reldist summarize wage, by(union) over(collgrad) stat(mean median)}
        {p_end}

{pstd}
    Being unionized seems to pay off more for people without college degree than for
    people with college degree.

{pstd}
    Note that the mean of the relative ranks has an intuitive interpretation: it
    is equal to the probability that a randomly chosen member of the
    comparison group has an outcome value that is at least as large at the
    outcome value of a randomly chosen member of the reference group (this is
    equivalent to the Gastwirth index; see Gastwirth 1975). In the example
    above, among people without college degree, the probability that a randomly
    selected unionized worker earns at least as much as a randomly selected
    non-unionized worker is 64%.

{dlgtab:Paired data relative distribution (syntax 2)}

{pstd}
    {cmd:reldist} can also be used to compare the distributions of two variables within the
    same sample of observations:

        . {stata webuse nlswork, clear}
        . {stata keep idcode year ln_wage}
        . {stata reshape wide ln_wage, i(idcode) j(year)}
        . {stata reldist pdf ln_wage88 ln_wage78, graph}

{pstd}
    It appears that log wages in 1988 have been more
    polarized than in 1978, which might be due to an age effect or a general increase
    in wage inequality. The result is confirmed by the MRP:

        . {stata reldist mrp ln_wage88 ln_wage78}

{dlgtab:Covariate balancing}

{pstd}
    The {helpb reldist##balance:balance()} option can be used to balance covariate
    distributions before computing the relative distribution. Example:

        . {stata sysuse nlsw88, clear}
{p 8 12 2}
        . {stata reldist pdf wage, by(union) balance(grade i.race ttl_exp) notable}
        {p_end}
        . {stata estimates store balanced}
{p 8 12 2}
        . {stata reldist pdf wage if e(sample), by(union) notable}
        {p_end}
        . {stata estimates store raw}
{p 8 12 2}
        . {stata coefplot raw balanced, at(at) noci recast(line) lw(*2) yline(1)}
        {p_end}

{pstd}
    We see that balancing the specified covariances makes the wage distribution between
    unionized and non-unionized workers somewhat more equal, but not very much.

{pstd}
    By default, inverse-probability weighting is used to balance the
    covariates. See the {helpb reldist##balance:balance()}
    option above for alternatives.

{dlgtab:Location and shape decompositions}

{pstd}
    Handcock and Morris (1999) discuss location and shape decompositions defined in
    a way such that for each outcome value the product of the location component
    and the shape component equals the overall relative density. For example,
    let {it:f}({it:y}) be the density
    of {it:y} in the comparison distribution, {it:f0}({it:y}) be the
    density of {it:y} in the reference distribution, {it:f0L}({it:y}) be the
    density of {it:y} in the location-adjusted reference distribution. The relative
    density can then be written as

        {it:f}({it:y})      {it:f0L}({it:y})      {it:f}({it:y})
        -----  =  ------  x  ------
        {it:f0}({it:y})     {it:f0}({it:y})      {it:f0L}({it:y})

{pstd}
    where the first term is the location component and the second term is the
    shape component of the decomposition. The components of such a decomposition
    can be computed by {cmd:reldist} using the {cmd:adjust()} option. It can be
    tricky, however, to figure out how exactly the option has to be specified.
    In the first term of the above decomposition (location component), we have to
    compare the location-adjusted reference distribution with the unadjusted
    reference distribution. Note that the location adjusted reference
    distribution is the same as the shape (and scale) adjusted comparison
    distribution. Hence, the first term can be computed using option
    {cmd:adjust(shape scale)}. For the second term, we compare the (unadjusted)
    comparison distribution with the location-adjusted reference
    distribution. This can be accomplished specifying {cmd:adjust(:location)}
    (note the colon; see the description of the
    {helpb reldist##adjust:adjust()} option above). Example:

        . {stata sysuse nlsw88, clear}
{p 8 12 2}
        . {stata reldist pdf wage, by(union) adjust(shape scale) graph}
        {p_end}
{p 8 12 2}
        . {stata "reldist pdf wage, by(union) adjust(: location) graph"}

{pstd}
    Similarly, to make the adjustments on the logarithmic scale, we could type

{p 8 12 2}
        . {stata reldist pdf wage, by(union) adjust(shape scale, logarithmic) graph}
        {p_end}
{p 8 12 2}
        . {stata "reldist pdf wage, by(union) adjust(: location, logarithmic) graph"}

{pstd}
    In the multiplicative case, no distinction is made between
    scale and shape (i.e. the shape adjustment includes the scale), so that the
    syntax would be:

{p 8 12 2}
        . {stata reldist pdf wage, by(union) adjust(shape, multiplicative) graph}
        {p_end}
{p 8 12 2}
        . {stata "reldist pdf wage, by(union) adjust(: location, multiplicative) graph"}

{pstd}
    In this situation, the multiplicative approach and the logarithmic approach lead to
    equivalent results. The logarithmic approach, however, provides more flexibility because
    it is possible to control the shape, net of scale. This is useful for more complicated
    decompositions.

{pstd}
    Note that the above decomposition might as well have been written as

        {it:f}({it:y})      {it:f}({it:y})      {it:fL}({it:y})
        -----  =  -----  x  -----
        {it:f0}({it:y})     {it:fL}({it:y})     {it:f0}({it:y})

{pstd}
    where {it:fL}({it:y}) is the density of {it:y} in the location-adjusted
    comparison distribution. To obtain this variant of the decomposition specify
    {cmd:adjust(:shape scale)} for the first term (location component)
    and {cmd:adjust(location)} for the second term (shape component).


{marker methods}{...}
{title:Methods and formulas}

{pstd}
    Methods and formulas used by {cmd:reldist} are documented in
    {browse "http://ideas.repec.org/p/bss/wpaper/37.html":Jann (2020)}.


{marker saved_results}{...}
{title:Saved results}

{pstd}
    {cmd:reldist} stores the following results in {cmd:e()}.

{synoptset 20 tabbed}{...}
{p2col 5 20 24 2: Scalars}{p_end}
{synopt:{cmd:e(N)}}number of observations{p_end}
{synopt:{cmd:e(N1)}}number of observations in comparison group (syntax 1 only){p_end}
{synopt:{cmd:e(N0)}}number of observations in reference group (syntax 1 only){p_end}
{synopt:{cmd:e(by1)}}value of comparison group (syntax 1 only){p_end}
{synopt:{cmd:e(by0)}}value of reference group (syntax 1 only){p_end}
{synopt:{cmd:e(N_over)}}number over-groups (if {cmd:over()} has been specified){p_end}
{synopt:{cmd:e(n)}}number of evaluation points ({cmd:pdf}, {cmd:cdf}, and {cmd:divergence} only){p_end}
{synopt:{cmd:e(bwidth)}}bandwidth of kernel density estimator ({cmd:pdf} only){p_end}
{synopt:{cmd:e(bwadjust)}}bandwidth adjustment factor ({cmd:pdf} and {cmd:divergence} only){p_end}
{synopt:{cmd:e(adaptive)}}number of iterations of adaptive estimator ({cmd:pdf} and {cmd:divergence} only){p_end}
{synopt:{cmd:e(napprox)}}size of approximation grid ({cmd:pdf} and {cmd:divergence} only){p_end}
{synopt:{cmd:e(n_hist)}}number of histogram bins ({cmd:pdf}, {cmd:histogram}, and {cmd:divergence} only){p_end}
{synopt:{cmd:e(hwidth)}}width of histogram bins ({cmd:pdf} and {cmd:histogram} only){p_end}

{synoptset 20 tabbed}{...}
{p2col 5 20 24 2: Macros}{p_end}
{synopt:{cmd:e(cmd)}}{cmd:reldist}{p_end}
{synopt:{cmd:e(subcmd)}}{cmd:pdf}, {cmd:histogram}, {cmd:cdf}, {cmd:divergence}, {cmd:mrp}, or {cmd:summarize}{p_end}
{synopt:{cmd:e(cmdline)}}command as typed{p_end}
{synopt:{cmd:e(depvar)}}name of dependent variable{p_end}
{synopt:{cmd:e(by)}}name of {it:groupvar} (syntax 1 only){p_end}
{synopt:{cmd:e(by1lab)}}label of comparison group (syntax 1 only){p_end}
{synopt:{cmd:e(by0lab)}}label of reference group (syntax 1 only){p_end}
{synopt:{cmd:e(swap)}}{cmd:swap} or empty (syntax 1 only){p_end}
{synopt:{cmd:e(pooled)}}{cmd:pooled} or empty{p_end}
{synopt:{cmd:e(refvar)}}name of {it:refvar} (syntax 2 only){p_end}
{synopt:{cmd:e(nobreak)}}{cmd:nobreak} or empty{p_end}
{synopt:{cmd:e(nomid)}}{cmd:nomid} or empty{p_end}
{synopt:{cmd:e(descending)}}{cmd:descending} or empty{p_end}
{synopt:{cmd:e(nostable)}}{cmd:nostable} or empty{p_end}
{synopt:{cmd:e(atopt)}}contents of {cmd:at()} option{p_end}
{synopt:{cmd:e(atx)}}{cmd:atx}, {cmd:comparison}, {cmd:reference} or empty{p_end}
{synopt:{cmd:e(atxopt)}}contents of {cmd:atx()} option{p_end}
{synopt:{cmd:e(discrete)}}{cmd:discrete} or empty{p_end}
{synopt:{cmd:e(categorical)}}{cmd:categorical} or empty{p_end}
{synopt:{cmd:e(alt)}}{cmd:alt} or empty{p_end}
{synopt:{cmd:e(origin)}}{cmd:origin} or empty{p_end}
{synopt:{cmd:e(adjust)}}list of comparison distribution adjustments{p_end}
{synopt:{cmd:e(refadjust)}}list of reference distribution adjustments{p_end}
{synopt:{cmd:e(adjmean)}}{cmd:mean} or empty{p_end}
{synopt:{cmd:e(adjsd)}}{cmd:sd} or empty{p_end}
{synopt:{cmd:e(adjlog)}}{cmd:logarithmic} or empty{p_end}
{synopt:{cmd:e(adjmult)}}{cmd:multiplicative} or empty{p_end}
{synopt:{cmd:e(c_adjust)}}list of alternate comparison distribution adjustments ({cmd:divergence} only){p_end}
{synopt:{cmd:e(c_refadjust)}}list of alternate reference distribution adjustments ({cmd:divergence} only){p_end}
{synopt:{cmd:e(c_adjmean)}}{cmd:mean} or empty ({cmd:divergence} only){p_end}
{synopt:{cmd:e(c_adjsd)}}{cmd:sd} or empty ({cmd:divergence} only){p_end}
{synopt:{cmd:e(c_adjlog)}}{cmd:logarithmic} or empty ({cmd:divergence} only){p_end}
{synopt:{cmd:e(c_adjmult)}}{cmd:multiplicative} or empty ({cmd:divergence} only){p_end}
{synopt:{cmd:e(balance)}}list of balancing variables{p_end}
{synopt:{cmd:e(balmethod)}}balancing method{p_end}
{synopt:{cmd:e(balref)}}{cmd:reference} or empty{p_end}
{synopt:{cmd:e(balcontrast)}}{cmd:contrast} or empty{p_end}
{synopt:{cmd:e(balopts)}}options passed through to balancing procedure{p_end}
{synopt:{cmd:e(compare)}}{cmd:compare} or empty{p_end}
{synopt:{cmd:e(c_balance)}}list of alternate balancing variables ({cmd:divergence} only){p_end}
{synopt:{cmd:e(c_balmethod)}}alternate balancing method ({cmd:divergence} only){p_end}
{synopt:{cmd:e(c_balref)}}{cmd:reference} or empty ({cmd:divergence} only){p_end}
{synopt:{cmd:e(c_balopts)}}alternate options passed through to balancing procedure ({cmd:divergence} only){p_end}
{synopt:{cmd:e(over)}}name of {it:overvar}{p_end}
{synopt:{cmd:e(over_namelist)}}values of over variable{p_end}
{synopt:{cmd:e(over_labels)}}values of over variable{p_end}
{synopt:{cmd:e(pdf)}}{cmd:pdf} or empty ({cmd:divergence} only){p_end}
{synopt:{cmd:e(boundary)}}boundary correction method ({cmd:pdf} and {cmd:divergence} only){p_end}
{synopt:{cmd:e(bwmethod)}}bandwidth selection method ({cmd:pdf} and {cmd:divergence} only){p_end}
{synopt:{cmd:e(kernel)}}kernel function ({cmd:pdf} and {cmd:divergence} only){p_end}
{synopt:{cmd:e(exact)}}{cmd:exact} or empty ({cmd:pdf} and {cmd:divergence} only){p_end}
{synopt:{cmd:e(statistics)}}names of reported statistics ({cmd:summarize} and {cmd:divergence} only){p_end}
{synopt:{cmd:e(generate)}}name of generated variable ({cmd:summarize} only){p_end}
{synopt:{cmd:e(wtype)}}weight type{p_end}
{synopt:{cmd:e(wexp)}}weight expression{p_end}
{synopt:{cmd:e(title)}}title in estimation output{p_end}
{synopt:{cmd:e(properties)}}{cmd:b} or {cmd:b V}{p_end}

{synoptset 20 tabbed}{...}
{p2col 5 20 24 2: Matrices}{p_end}
{synopt:{cmd:e(b)}}estimates{p_end}
{synopt:{cmd:e(V)}}variance-covariance matrix of estimates{p_end}
{synopt:{cmd:e(at)}}evaluation points ({cmd:pdf}, {cmd:histogram}, and {cmd:cdf} only){p_end}
{synopt:{cmd:e(ogrid)}}outcome label approximation grid ({cmd:pdf}, {cmd:histogram}, and {cmd:cdf} only){p_end}
{synopt:{cmd:e(bwidth)}}bandwidths of kernel density estimators ({cmd:divergence} only){p_end}
{synopt:{cmd:e(_N)}}number of obs per over-group (if {cmd:over()} had been specified){p_end}
{synopt:{cmd:e(_N1)}}number of obs per over-group in comparison group (syntax 1 only){p_end}
{synopt:{cmd:e(_N0)}}number of obs per over-group in reference group (syntax 1 only){p_end}

{synoptset 20 tabbed}{...}
{p2col 5 20 24 2: Functions}{p_end}
{synopt:{cmd:e(sample)}}estimation sample{p_end}
{p2colreset}{...}

{pstd}
    If {cmd:vce()} is {cmd:svy}, {cmd:bootstrap}, or {cmd:jackknife}, additional
    results are stored in {cmd:e()}; see {helpb svy}, {helpb bootstrap}, and
    {helpb jackknife}, respectively.


{marker references}{...}
{title:References}

{phang}
    Cwik, J., J. Mielniczuk (1993). Data-dependent bandwidth choice for a grade density
    kernel estimate. Statistics & Probability Letters 16: 397-405.
    {p_end}
{phang}
    Gastwirth, J.L. (1975). Statistical measures of earnings differentials. The 
    American Statistician 29: 32-35.
    {p_end}
{phang}
    Handcock, M.S., M. Morris (1998). Relative Distribution Methods.
    Sociological Methodology 28: 53-97.
    {p_end}
{phang}
    Handcock, M.S., M. Morris (1999). Relative Distribution Methods
    in the Social Sciences. New York: Springer.
    {p_end}
{phang}
    Jann, B. (2020). Relative distribution analysis in Stata.  University of
    Bern Social Sciences Working Papers 37. Available from
    {browse "http://ideas.repec.org/p/bss/wpaper/37.html"}.
    {p_end}


{marker author}{...}
{title:Author}

{pstd}
    Ben Jann, University of Bern, ben.jann@unibe.ch

{pstd}
    Thanks for citing this software as follows:

{pmore}
    Jann, B. (2020). reldist: Stata module for relative distribution analysis. Available from
    {browse "http://ideas.repec.org/c/boc/bocode/s458775.html"}.


{marker also_see}{...}
{title:Also see}

{psee}
    Online: help for {helpb cumul}, {helpb ppplot} (from SSC), {helpb moremata}

