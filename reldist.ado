*! version 1.1.0  02may2020  Ben Jann

local rc 0
capt findfile lmoremata.mlib
if _rc {
    di as error "-moremata- is required; type {stata ssc install moremata}"
    local rc = _rc
}
capt findfile lkdens.mlib
if _rc {
    di as error "-kdens- is required; type {stata ssc install kdens}"
    local rc = _rc
}
if `rc' error 499

program reldist, eclass properties(svyb svyj)
    version 12
    if replay() {
        Replay `0'
        exit
    }
    gettoken subcmd 0 : 0, parse(", ")
    if `"`subcmd'"'==substr("graph",1,max(2,strlen(`"`subcmd'"'))) {
        GRAPH `0'
        exit
    }
    if `"`subcmd'"'==substr("olabel",1,max(4,strlen(`"`subcmd'"'))) {
        OLABEL `0'
        exit
    }
    local version : di "version " string(_caller()) ":"
    Check_vceprefix `subcmd'`0'
    `version' _vce_parserun reldist, noeqlist wtypes(aw pw iw) ///
        bootopts(force reject(e(k_omit))) : `00'
    if "`s(exit)'" != "" {
        ereturn local cmdline `"reldist `subcmd'`0'"'
        exit
    }
    if `"`subcmd'"'=="pdf" {
        PDF `0'
    }
    else if `"`subcmd'"'==substr("histogram",1,max(4,strlen(`"`subcmd'"'))) {
        HIST `0'
    }
    else if `"`subcmd'"'=="cdf" {
        CDF `0'
    }
    else if `"`subcmd'"'=="mrp" {
        MRP `0'
    }
    else if `"`subcmd'"'==substr("summarize",1,max(2,strlen(`"`subcmd'"'))) {
        SUM `0'
    }
    else {
        di as err `"invalid subcommand: `subcmd'"'
        exit 198
    }
    eret local cmdline `"reldist `subcmd'`0'"'
    Replay, `diopts'
    if `"`e(generate)'"'!="" {
        di as txt "(relative ranks stored in variable {bf:`e(generate)'})"
    }
end

program Check_vceprefix
    _parse comma lhs 0 : 0
    syntax [, vce(str) Generate(passthru) atx(passthru) NOSE ///
        BWidth(str) BWADJust(passthru) * ]
    local options `generate' `atx' `nose' `options'
    if `"`vce'"'!="" {
        Parse_vceprefix `vce'
        if "`vcecmd'"!="" {
            if "`generate'"!="" {
                di as err "{bf:generate()} not allowed with {bf:vce(`vcecmd')}"
                exit 198
            }
            if `"`atx'"'!="" {
                di as err "{bf:atx()} not allowed with {bf:vce(`vcecmd')}"
                exit 198
            }
            if "`nose'"=="" {
                local options nose `options'
            }
            Obtain_bwidth `lhs', _vcevars(`vcevars') ///
                bwidth(`bwidth') `bwadjust' `options'
        }
        local options vce(`vce') `options'
    }
    if `"`bwidth'"'!="" {
        local bwidth bwidth(`bwidth')
    }
    c_local 00 `lhs', `bwidth' `bwadjust' `options'
end

program Parse_vceprefix
    _parse comma vcecmd 0 : 0
    if `"`vcecmd'"'== substr("bootstrap",1,max(4,strlen(`"`vcecmd'"'))) {
        c_local vcecmd bootstrap
    }
    else if `"`vcecmd'"'== substr("jackknife",1,max(4,strlen(`"`vcecmd'"'))) {
        c_local vcecmd jackknife
    }
    else exit
    syntax [, STRata(varlist) CLuster(varlist) group(varname) JACKknifeopts(str) * ]
    Parse_vceopt_jack, `jackknifeopts'  // returns vcevars
    c_local vcevars `vcevars' `strata' `cluster' `group'
end

program Parse_vceopt_jack
    syntax [, CLuster(varlist) * ]
    c_local vcevars `cluster'
end

program Obtain_bwidth   // returns bwidth, bwadjust
    gettoken subcmd 0 : 0, parse(", ")
    if `"`subcmd'"'!="pdf" exit
    syntax [anything] [if] [in] [fw iw aw pw] [, _vcevars(str) bwidth(str) * ]
    capt confirm number `bwidth'
    if _rc==0 exit
    di as txt "(running {bf:reldist} to obtain bandwith)"
    marksample touse
    markout `touse' `_vcevars', strok
    qui PDF `anything' if `touse' [`weight'`exp'], `options'
    c_local bwidth = e(bwidth)
    c_local bwadjust
end

program Replay
    if `"`e(cmd)'"'!="reldist" {
        di as err "last reldist results not found"
        exit 301
    }
    local subcmd `"`e(subcmd)'"'
    if !inlist(`"`subcmd'"', "pdf", "histogram", "cdf", "mrp", "summarize") {
        di as err "last reldist results not found"
        exit 301
    }
    syntax [, Level(passthru) noHeader NOTABle TABle GRaph GRaph2(str asis) * ]
    if `"`graph2'"'!="" local graph graph
    if `"`level'"'=="" {
        if `"`e(level)'"'!="" {
            local level level(`e(level)')
        }
    }
    local options `level' `options'
    if "`header'"=="" {
        if `"`e(by)'"'!="" {
            local line1 `"`e(by)' = "'
            local line2 `"`line1'"'
            if `"`e(by1lab)'"'!="" local line1 `"`line1'{res:`e(by1lab)'}"'
            else                   local line1 `"`line1'{res:`e(by1)'}"'
            if `"`e(by0lab)'"'!="" local line2 `"`line2'{res:`e(by0lab)'}"'
            else                   local line2 `"`line2'{res:`e(by0)'}"'
        }
        else {
            local line1 `"{res:`e(depvar)'}"'
            local line2 `"{res:`e(refvar)'}"'
        }
        local line1 = abbrev(`"`line1'"', 31)
        local line2 = abbrev(`"`line2'"', 31)
        local line1 `" F comparison: `line1'"'
        local line2 `" F0 reference: `line2'"'
        if `"`e(adjust)'`e(refadjust)'"'!="" {
            if `"`e(adjust)'"'!=""    local line3 `"{res:`e(adjust)'}"'
            else                      local line3 `"(none)"'
            if `"`e(refadjust)'"'!="" local line4 `"{res:`e(refadjust)'}"'
            else                      local line4 `"(none)"'
            local line3 `" Adjustment F: `line3'"'
            local line4 `"           F0: `line4'"'
            local adjopts `"`e(adjmean)' `e(adjsd)' `e(adjmult)' `e(adjlog)'"'
            local adjopts: list retok adjopts
            if `"`adjopts'"'!=""  local line5 `"         type: {res:`adjopts'}"'
        }
        _coef_table_header
        local i 0
        if `"`e(by)'"'!="" {
            di as txt `"`line`++i''"' /*
                */ as txt _col(49) "Comparison obs" _col(67) "= " as res %10.0gc e(N1)
            di as txt `"`line`++i''"' /*
                */ as txt _col(49) "Reference obs"  _col(67) "= " as res %10.0gc e(N0)
        }
        if "`subcmd'"=="pdf" {
            di as txt `"`line`++i''"' /*
                */ as txt _col(49) "Bandwidth" _col(67) "= " as res %10.0g e(bwidth)
            if `"`e(divergence)'"'!="" {
                di as txt `"`line`++i''"' /*
                    */ as txt _col(49) "Divergence" _col(67) "= " as res %10.0g e(divergence)
            }
            if `"`e(chi2)'"'!="" {
                di as txt `"`line`++i''"' /*
                    */ as txt _col(49) "Chi-squared" _col(67) "= " as res %10.0g e(chi2)
            }
        }
        while (`i'<5) { // flush remaining lines
            if `"`line`++i''"'=="" continue, break
             di as txt `"`line`i''"'
        }
        if `"`e(over)'"'!="" {
            _svy_summarize_legend
        }
        else di ""
    }
    if ("`table'"!="" | "`graph'"=="") & "`notable'"=="" {
        if `"`subcmd'"'=="pdf" {
            capt confirm matrix e(V)
            if _rc {
                capt confirm matrix e(se)
                if _rc==0 {
                    tempname V
                    mat `V' = e(se)
                    mata: st_replacematrix("`V'", st_matrix("`V'"):^2)
                    mat `V' = diag(`V')
                    local vmatrix vmatrix(`V')
                    if c(stata_version)<12 {
                        tempname b
                        mat `b' = e(b)
                        local vmatrix bmatrix(`b') `vmatrix'
                    }
                }
            }
        }
        _coef_table, `vmatrix' `options'
    }
    else if "`notable'"=="" {
        di as txt "(coefficients table suppressed)"
    }
    if "`graph'"!="" {
        GRAPH, `graph2'
    }
end

program GRAPH
    if `"`e(cmd)'"'!="reldist" {
        di as err "last {bf:reldist} results not found"
        exit 301
    }
    local subcmd `"`e(subcmd)'"'
    if !inlist(`"`subcmd'"', "pdf", "histogram", "cdf") {
        di as err `"{bf:reldist graph} not supported after {bf:reldist `subcmd'}"'
        exit 499
    }
    GRAPH_`subcmd' `0'
end

program _GRAPH_get_gropts // separate general twoway options from other options
                          // returns twopts, options
    _get_gropts, graphopts(`0') gettwoway
    local twopts `s(twowayopts)'
    local 0 `", `s(graphopts)'"'
    // parse some additional options that do not seem to be covered by
    // _get_gropts, gettwoway; this possibly has to be updated for
    // future Stata versions
    syntax [, ///
        LEGend(passthru)     ///
        play(passthru)       ///
        PCYCle(passthru)     ///
        YVARLabel(passthru)  ///
        XVARLabel(passthru)  ///
        YVARFormat(passthru) ///
        XVARFormat(passthru) ///
        YOVERHANGs           ///
        XOVERHANGs           ///
        /// recast(passthru)     ///
        fxsize(passthru)     ///
        fysize(passthru) * ]
    c_local twopts `twopts' `legend' `play' `pcycle' `yvarlabel' `xvarlabel'/*
        */ `yvarformat' `xvarformat' `yoverhangs' `fxsize' `fysize'
    c_local options `options'
end

program _GRAPH_get_CI // obtain CI, if available
                      // returns hasci, hashci
    args ll ul n level noci ci nhist nohci
    if "`nhist'"=="" local nohci nohci
    if "`noci'"!="" & "`nohci'"!="" {
        c_local hasci
        c_local hashci
        exit
    }
    if "`ci'"!="" {
        if `"`level'"'!="" {
            di as err "{bf:ci()} and {bf:level()} not both allowed"
            exit 198
        }
        capt confirm matrix e(`ci')
        if _rc {
            di as error "matrix e(`ci') not found"
            exit 111
        }
        if rowsof(e(`ci'))!=2 | colsof(e(`ci'))!=`n' {
            di as error "matrix e(`ci') not conformable"
            exit 499
        }
        mata: rd_svmat("e(`ci')", ("`ll'", "`ul'"), 1)
        local hasci hasci
    }
    else {
        if `"`level'"'=="" {
            if `"`e(level)'"'!="" {
                local level level(`e(level)')
            }
        }
        capt confirm matrix e(V)
        if _rc {
            capt confirm matrix e(se)
            if _rc==0 {
                tempname V
                mat `V' = e(se)
                mata: st_replacematrix("`V'", st_matrix("`V'"):^2)
                mat `V' = diag(`V')
                if c(stata_version)<12 {
                    tempname b
                    mat `b' = e(b)
                    qui _coef_table, bmatrix(`b')  vmatrix(`V') `level'
                }
                else {
                    qui _coef_table, vmatrix(`V') `level'
                }
                local hasci hasci
            }
        }
        else {
            qui _coef_table, `level'
            local hasci hasci
        }
        if "`hasci'"!="" {
            tempname CI
            mat `CI' = r(table)
            mat `CI' = `CI'[5..6, 1...]
            mata: rd_svmat("`CI'", ("`ll'", "`ul'"), 1)
        }
    }
    if "`hasci'"=="" {
        c_local hasci
        c_local hashci
        exit
    }
    if "`noci'"=="" c_local hasci hasci
    else            c_local hasci
    if "`nohci'"=="" {
        local a = `n' - `nhist' + 1
        su `ll' in `a'/`n', meanonly // check whether CI exists
        if r(N) c_local hashci hashci
        else    c_local hashci
    }
    else c_local hashci
end

program _GRAPH_olab // returns oaxis, twopts
    syntax [, olabel(str asis) otick(str asis) otitle(str asis) olabquick * ]
    local twopts `options'
    // otitle
    _parse comma otitle 0 : otitle
    syntax [, * ]
    if `"`otitle'"'=="" local otitle `""""'
    local otitle  `"`otitle'"'
    local otiopts `"`options'"'
    // olabel and otick
    if `"`olabel'"'!="" {
        local 0 `"`olabel'"'
        syntax [anything] [, FORmat(passthru) * ]
        local olabel `"`anything'"'
        local olabopts `"`options'"'
    }
    if `"`otick'"'!="" {
        local 0 `"`otick'"'
        syntax [anything] [, * ]
        local otick `"`anything'"'
        local otickopts `"`options'"'
    }
    if `"`olabel'`otick'"'=="" exit             // nothing to do
    if "`olabquick'"!="" local olabquick quick
    OLABEL `"`olabel'"', otick(`"`otick'"') `format' `olabquick'
    local olabel `"`r(label)'"'
    local otick  `"`r(tick)'"'
    // returns
    if `"`olabel'"'!="" {
        local twopts xlabel(`olabel', axis(2) `olabopts') `twopts'
    }
    else {
        local twopts xlabel(none, axis(2)) `twopts'
    }
    if `"`otick'"'!="" {
        local twopts xtick(`otick', axis(2) `otickopts') `twopts'
    }
    c_local oaxis xaxis(1 2) // add to each plot so that axes do not move
    c_local twopts xtitle(`otitle', axis(2) `otiopts') `twopts'
end

program _GRAPH_xyti // return xti, yti
    if `"`e(by)'"'=="" {
        c_local xti `"`e(refvar)'"'
        c_local yti `"`e(depvar)'"'
    }
    else {
        if `"`e(by0lab)'"'!="" {
            c_local xti `"`e(by0lab)'"'
        }
        else {
            c_local xti `"`e(by)' = `e(by0)'"'
        }
        if `"`e(by1lab)'"'!="" {
            c_local yti `"`e(by1lab)'"'
        }
        else {
            c_local yti `"`e(by)' = `e(by1)'"'
        }
    }
end

program GRAPH_pdf
    // syntax
    syntax [, Level(passthru) NOCI ci(name) CIOPTs(str) ///
        noREFline REFOPTs(str) NOHISTogram HISTopts(str) ///
        OLABel(passthru) OTICk(passthru) OTItle(passthru) OLABQuick ///
        addplot(str asis) * ]
    _GRAPH_pdf_histopts, `histopts'
    _GRAPH_get_gropts `options'
     
    // obtain original scale labels
    _GRAPH_olab, `olabel' `otick' `otitle' `olabquick' `twopts'
    
    // check number of obs and expand data if necessary
    local n     = e(n)
    local npdf  = e(n)
    local nhist = e(n_hist)
    if `nhist'>=. {
        local nhist
        local nohistogram nohistogram
    }
    else {
        local n = `n' + `nhist'
    }
    if `n'>_N {
        preserve
        qui set obs `n'
    }
    
    // store results to data
    tempname pdf at
    mata: rd_svmat("e(b)", "`pdf'", 1)
    mata: rd_svmat("e(at)", "`at'", 1)
    
    // obtain CI if available
    tempname ll ul
    _GRAPH_get_CI `ll' `ul' `n' `"`level'"' "`noci'" "`ci'" ///
        "`nhist'" "`nohci'"
    
    // compile graph
    local plots
    // - refline
    if "`refline'"=="" {
        local yline yline(1, `refopts')
    }
    // - histogram bars
    if "`nohistogram'"=="" {
        local hwidth = e(hwidth)
        local a = `npdf' + 1
        local plots `plots' /*
            */ (bar `pdf' `at' in `a'/`n', `oaxis'/*
            */ barwidth(`hwidth') pstyle(histogram) `histopts')
        if "`hashci'"!="" {
            local plots `plots' /*
                */ (rcap `ll' `ul' `at' in `a'/`n', `oaxis'/*
                */ pstyle(histogram) `hciopts')
        }
    }
    // - CI
    if "`hasci'"!="" {
        local plots `plots' /*
            */ (rarea `ll' `ul' `at' in 1/`npdf', `oaxis'/*
            */ pstyle(ci) `ciopts')
    }
    // - pdf
    local plots `plots' /*
        */ (line `pdf' `at' in 1/`npdf', `oaxis'/*
        */ pstyle(p1) `options')
    // - addplot
    if `"`addplot'"'!="" {
        local plots `plots' || `addplot' ||
    }
    // - axis titles
    _GRAPH_xyti
    // - draw
    twoway `plots', legend(off) `yline' xtitle(`"`xti'"') ytitle(`"`yti'"') ///
        ylabel(0, add) `twopts'
end

program _GRAPH_pdf_histopts
    syntax [, NOCI CIOPTs(str) * ]
    c_local nohci `noci'
    c_local hciopts `ciopts'
    c_local histopts `options'
end

program GRAPH_histogram
    // syntax
    syntax [, Level(passthru) NOCI ci(name) CIOPTs(str) ///
        noREFline REFOPTs(str) ///
        OLABel(passthru) OTICk(passthru) OTItle(passthru) OLABQuick ///
        addplot(str asis) * ]
    _GRAPH_get_gropts `options'
     
    // obtain original scale labels
    _GRAPH_olab, `olabel' `otick' `otitle' `olabquick' `twopts'
    
    // check number of obs and expand data if necessary
    local n = e(n_hist)
    if `n'>_N {
        preserve
        qui set obs `n'
    }
    
    // store results to data
    tempname pdf at
    mata: rd_svmat("e(b)", "`pdf'", 1)
    mata: rd_svmat("e(at)", "`at'", 1)
    
    // obtain CI if available
    tempname ll ul
    _GRAPH_get_CI `ll' `ul' `n' `"`level'"' "`noci'" "`ci'" 
    
    // compile graph
    local plots
    // - refline
    if "`refline'"=="" {
        local yline yline(1, `refopts')
    }
    // - histogram bars
    local hwidth = e(hwidth) 
    local plots `plots' /*
        */ (bar `pdf' `at' in 1/`n', `oaxis'/*
        */ barwidth(`hwidth') pstyle(histogram) `options')
    // - CI
    if "`hasci'"!="" {
        local plots `plots' /*
            */ (rcap `ll' `ul' `at' in 1/`n', `oaxis'/*
            */ pstyle(histogram) `ciopts')
    }
    // - addplot
    if `"`addplot'"'!="" {
        local plots `plots' || `addplot' ||
    }
    // - axis titles
    _GRAPH_xyti
    // - draw
    twoway `plots', legend(off) `yline' xtitle(`"`xti'"') ytitle(`"`yti'"') ///
        ylabel(0, add) `twopts'
end

program GRAPH_cdf
    // syntax
    syntax [, Level(passthru) NOCI ci(name) CIOPTs(str) ///
        noREFline REFOPTs(str) ///
        OLABel(passthru) OTICk(passthru) OTItle(passthru) OLABQuick ///
        addplot(str asis) * ]
    _GRAPH_get_gropts `options'
    
    // obtain original scale labels
    _GRAPH_olab, `olabel' `otick' `otitle' `olabquick' `twopts'
    
    // check number of obs and expand data if necessary
    local n = e(n)
    if `n'>_N {
        preserve
        qui set obs `n'
    }
    
    // store results to data
    tempname cdf at
    mata: rd_svmat("e(b)", "`cdf'", 1)
    mata: rd_svmat("e(at)", "`at'", 1)
    
    // obtain CI if available
    tempname ll ul
    _GRAPH_get_CI `ll' `ul' `n' `"`level'"' "`noci'" "`ci'" 
    
    // compile graph
    local plots
    // - refline
    if "`refline'"=="" {
        local plots `plots' /*
            */ (scatteri 0 0 1 1, `oaxis'/*
            */ connect(l) ms(i) lstyle(xyline) `refopts')
    }
    // - CI
    if "`hasci'"!="" {
        local plots `plots' /*
            */ (rarea `ll' `ul' `at' in 1/`n', `oaxis'/*
            */ connect(J) pstyle(ci) `ciopts')
    }
    // - cdf
    local plots `plots' /*
        */ (line `cdf' `at' in 1/`n', `oaxis'/*
        */ pstyle(p1) connect(J) `options')
    // - addplot
    if `"`addplot'"'!="" {
        local plots `plots' || `addplot' ||
    }
    // - axis titles
    _GRAPH_xyti
    // - draw
    twoway `plots', legend(off) xtitle(`"`xti'"') ytitle(`"`yti'"') ///
        `twopts'
end

program OLABEL, rclass
    if `"`e(cmd)'"'!="reldist" {
        di as err "last {bf:reldist} results not found"
        exit 301
    }
    _parse comma lhs 0 : 0
    syntax [, OTICk(numlist sort) FORmat(str) quick ]
    return local tick_x `"`otick'"'
    if `"`format'"'=="" local format "%6.0g"
    confirm format `format'
    local 0 `", olabel(`lhs')"'
    syntax [, OLABel(numlist sort) ]
    return local label_x `"`olabel'"'
    if `"`olabel'`otick'"'=="" exit // nothing to do
    if "`quick'"=="" {
        capt _OLABEL `"`olabel'"' "`format'" `"`otick'"'
        if _rc==1 exit _rc // break
        else if _rc {
            di as txt "(computing olabels from data failed; " /*
                */ "using approximate method based on values stored in "/*
                */ "e(at) and e(atx))"
            local quick quick
        }
    }
    if "`quick'"!="" {
        confirm matrix e(at)
        confirm matrix e(atx)
        mata: rd_olab_ipolate("olabel", "`format'")
        mata: rd_olab_ipolate("otick", "")
    }
    return local label `"`olabel'"'
    return local tick  `"`otick'"'
    return local quick  "`quick'"
end

program _OLABEL // returns olabel, otick
    args olabel format otick
    
    // get settings
    local N         = e(N)
    local weight    `"`e(wtype)'"'
    local exp       `"`e(wexp)'"'
    local depvar    `"`e(depvar)'"'
    local refvar    `"`e(refvar)'"'
    local by        `"`e(by)'"'
    local by1       = e(by1)
    local by0       = e(by0)
    local adj1      `"`e(adjust)'"'
    local adj0      `"`e(refadjust)'"'
    local adjmean   `"`e(adjmean)'"'
    local adjsd     `"`e(adjsd)'"'
    local adjlog    `"`e(adjlog)'"'
    local adjmult   `"`e(adjmult)'"'
    
    // mark sample
    confirm variable `depvar' `refvar' `by'
    tempvar touse touse1 touse0 wvar
    qui gen byte `touse' = e(sample)
    assert (`touse'<.)
    if `"`weight'"'!="" {
        gettoken eq exp : exp, parse("=") // strip "="
        assert (`"`eq'"' == "=")
        capt confirm variable `exp'
        if _rc {
            qui gen double `wvar' = `exp' if `touse'
        }
        else local wvar `exp'
        local wgt "[`weight'=`wvar']"
    }
    _nobs `touse' `wgt', min(0)
    assert (`N' == r(N))
    if `"`by'"'!="" {
        qui gen byte `touse1' = (`touse' & `by'==`by1')
        qui gen byte `touse0' = (`touse' & `by'==`by0')
        local refvar `depvar'
    }
    else {
        local touse1 `touse'
        local touse0 `touse'
    }

    // compute postions
    mata: rd_OLABEL()
    c_local olabel `"`olabel'"'
    c_local otick  `"`otick'"'
end

program Parse_syntax    // preprocess syntax: two-sample vs. paired
                        // returns under syntax 1: 0, depvar, by, swap
                        // returns under syntax 2: 0, depvar, refvar
    syntax varlist(min=1 max=2 numeric) [if] [in] [fw iw aw pw] ///
        [, by(varname numeric) swap * ]
    // Syntax 1: two-sample
    if `"`by'"'!="" {
        if `:list sizeof varlist'>1 {
            di as err "{it:refvar} and {bf:by()} not both allowed"
            exit 198
        }
        c_local 0 `if' `in' [`weight'`exp'], `options'
        c_local depvar `varlist'
        c_local by     `by'
        c_local swap   `swap'
        exit
    }
    // Syntax 2: paired
    if `:list sizeof varlist'==1 {
        di as err "{bf:by()} or {it:refvar} required"
        exit 198
    }
    if "`swap'"!="" {
        di as err "{bf:swap} not allowed in syntax 2"
        exit 198
    }
    c_local 0 `if' `in' [`weight'`exp'], `options'
    c_local depvar: word 1 of `varlist'
    c_local refvar: word 2 of `varlist'
end

program Parse_adjust // parse the adjust() option
                     // returns adj1, ads0, adjmean, adjsd, adjlog, adjmult
    syntax [anything] [, mean sd LOGarithmic MULTiplicative ]
    if "`logarithmic'"!="" & "`multiplicative'"!="" {
        di as err "{bf:adjust()}: only one of {bf:logarithmic} and {bf:multiplicative} allowed"
        exit 198
    }
    local k 1
    while (`"`anything'"'!="") {
        gettoken tok anything: anything, parse(": ")
        if `"`tok'"'==":" {    // start of reference adjustment
            local k 0
            continue
        }
        if `"`tok'"'==substr("location",1,max(1,strlen(`"`tok'"'))) {
            local tok location
        }
        else if `"`tok'"'==substr("scale",1,max(2,strlen(`"`tok'"'))) {
            local tok scale
            if "`multiplicative'"!="" {
                di as err "{bf:adjust()}: 'scale' not with option {bf:multiplicative}"
                exit 198
            }
        }
        else if `"`tok'"'==substr("shape",1,max(2,strlen(`"`tok'"'))) {
            local tok shape
        }
        else if `"`tok'"'!="" {
            di as err "{bf:adjust()}: '" `"`tok'"' "' not allowed"
            exit 198
        }
        local adj`k' `adj`k'' `tok'
    }
    local adj1: list uniq adj1
    local adj0: list uniq adj0
    local adj1: list sort adj1
    local adj0: list sort adj0
    if "`adj0'`adj1'"=="" {
        local mean
        local logarithmic
        local multiplicative
    }
    c_local adj1: list sort adj1
    c_local adj0: list sort adj0
    c_local adjmean `mean'
    c_local adjsd   `sd'
    c_local adjlog  `logarithmic'
    c_local adjmult `multiplicative'
end

program Parse_at   // parse n(), at(), atx() in reldist pdf and reldist cdf
                   // returns n, ATX0, atx, AT0, at
    args n at atx
    if `"`atx'"'!="" {
        if "`n'"!="" {
            di as err "{bf:n()} and {bf:atx()} not both allowed"
            exit 198
        }
        if `"`at'"'!="" {
            di as err "{bf:at()} and {bf:atx()} not both allowed"
            exit 198
        }
        if `: list sizeof atx'==1 {
            capt confirm matrix `atx'
            if _rc==0 {
                c_local ATX0 `atx'
                c_local n = max(rowsof(`atx'), colsof(`atx'))
                c_local atx
                exit
            }
        }
        local 0 `", atx(`atx')"'
        syntax [, atx(numlist ascending) ]
        c_local atx "`atx'"
        c_local n: list sizeof atx
        exit
    }
    if `"`at'"'!="" {
        if "`n'"!="" {
            di as err "{bf:n()} and {bf:at()} not both allowed"
            exit 198
        }
        if `: list sizeof at'==1 {
            capt confirm matrix `at'
            if _rc==0 {
                c_local AT0 `at'
                c_local n = max(rowsof(`at'), colsof(`at'))
                c_local at
                exit
            }
        }
        local 0 `", at(`at')"'
        syntax [, at(numlist ascending >=0 <=1) ]
        c_local at "`at'"
        c_local n: list sizeof at
        exit
    }
    if "`n'"=="" c_local n 101
end

program Samplesetup // common function to prepare estimation sample
            // general returns: wvar, exp, wgt, swgt, N
            // returns if syntax 1: by1, by0, by1lab, by0lab, refvar, N1, N0
            // returns if syntax 2: touse1, touse0 
            // returns if over: N_over, overlevels, over_labels
    args touse touse1 touse0 wvar depvar by swap refvar weight exp over
    if "`weight'"!="" {
        capt confirm variable `exp'
        if _rc {
            qui gen double `wvar' = `exp' if `touse'
        }
        else local wvar `exp'
        local wgt "[`weight'=`wvar']"
        if inlist("`weight'", "iweight", "pweight") {
            c_local swgt "[aweight=`wvar']"
        }
        else c_local swgt "`wgt'"
        c_local wvar `wvar'
        c_local exp `"= `exp'"'
        c_local wgt "`wgt'"
    }
    if "`by'"!="" {     // syntax 1
        markout `touse' `by'
        qui levelsof `by' if `touse', local(bylevels)
        if `:list sizeof bylevels'!=2 {
            di as err "{it:groupvar} must define two groups"
            exit 498
        }
        if "`swap'"=="" {
            local by0: word 1 of `bylevels'
            local by1: word 2 of `bylevels'
        }
        else {
            local by0: word 2 of `bylevels'
            local by1: word 1 of `bylevels'
        }
        qui gen byte `touse1' = (`touse' & `by'==`by1')
        _nobs `touse1' `wgt'
        c_local N1 = r(N)
        qui gen byte `touse0' = (`touse' & `by'==`by0')
        _nobs `touse0' `wgt'
        c_local N0 = r(N)
        c_local by1lab: label (`by') `by1', strict
        c_local by0lab: label (`by') `by0', strict
        c_local by1 `by1'
        c_local by0 `by0'
        c_local refvar `depvar'
    }
    else {              // syntax 2
        markout `touse' `refvar'
        c_local touse1 `touse'
        c_local touse0 `touse'
    }
    _nobs `touse' `wgt', min(1)
    c_local N = r(N)
    if "`over'"!="" {
        capt assert ((`over'==floor(`over')) & (`over'>=0)) if `touse'
        if _rc {
            di as err "variable in over() must be integer and nonnegative"
            exit 452
        }
        qui levelsof `over' if `touse', local(overlevels)
        c_local N_over: list sizeof overlevels
        c_local overlevels "`overlevels'"
        local over_labels
        foreach o of local overlevels {
            local olab: label (`over') `o'
            local over_labels `"`over_labels' `"`olab'"'"'
        }
        c_local over_labels: list clean over_labels
    }
end

program Check_adjlog // logarithmic adjustment: assert that y > 0
    args touse depvar refvar by adjlog
    if "`adjlog'"=="" exit
    if "`by'"!="" {
        capt assert (`depvar'>0) if `touse'
    }
    else {
        capt assert (`depvar'>0) & (`refvar'>0) if `touse'
    }
    if _rc==1 exit _rc // break
    else if _rc {
        di as err "{bf:logarithmic} only allowed if outcomes are strictly positive"
        exit 499
    }
end

program PrepareOver // common function to prepare cycling across over groups
    args N_over overlevels by touse1 touse0 _N _N1 _N0
    mat `_N' = J(`N_over',1,.)
    mat rown `_N' = `overlevels'
    if "`by'"!="" {
        mat `_N1' = `_N'
        mat `_N0' = `_N'
        qui gen byte `touse1' = .
        qui gen byte `touse0' = .
    }
    else {
        qui gen byte `touse1' = .
        c_local touse0 `touse1'     // !!!
    }
end

program PrepareOverlevel // common function to handle specific over level
    args i o by touse touse1 touse0 TOUSE1 TOUSE0 _N _N1 _N0 wgt
    _nobs `touse' `wgt' if `o'
    mat `_N'[`i',1] = r(N)
    if "`by'"!="" {
        qui replace `touse1' = `TOUSE1' & `o'
        qui replace `touse0' = `TOUSE0' & `o'
        _nobs `touse1' `wgt', min(0)
        mat `_N1'[`i',1] = r(N)
        _nobs `touse0' `wgt', min(0)
        mat `_N0'[`i',1] = r(N)
    }
    else {
        qui replace `touse1' = `TOUSE1' & `o'
    }
end

program PDF, eclass
    // syntax
    Parse_syntax `0'
    syntax [if] [in] [fw iw aw pw/], [ ADJust(str) ///
        n(numlist int >0 max=1) ///
        at(str) atx(str) ///
        BWidth(str) BWADJust(numlist >0 max=1) ///
        BOundary(str) Kernel(string) ///
        ADAPTive(numlist int >=0 max=1) ///
        altlbwf /// (undocumented)
        exact NApprox(numlist int >1 max=1) ///
        HISTogram HISTogram2(numlist int >0 max=1) ///
        /*vce(str)*/ NOSE Level(cilevel) noHeader NOTABle TABle ///
        GRaph GRaph2(passthru) * ]
    //if `"`vce'"'!="" {
    //    if "`nose'"!="" {
    //        di as err "{bf:vce()} and {bf:nose} not both allowed"
    //        exit 198
    //    }
    //    if `"`vce'"'!=substr("analytic",1,max(1,strlen(`"`vce'"'))) {
    //        di as err "{bf:vce()}: '" `"`vce'"' "' not allowed"
    //        exit 198
    //    }
    //    local vce analytic
    //}
    _get_diopts diopts, `options'
    c_local diopts `diopts' `header' `notable' `table' `graph' `graph2'
    Parse_adjust `adjust'
    Parse_at "`n'" `"`at'"' `"`atx'"'
    if "`histogram2'"!=""     local nhist `histogram2'
    else if "`histogram'"!="" local nhist 10
    if "`napprox'"==""        local napprox = max(512, `n'+1)
    capt confirm number `bwidth'
    if _rc==0 {
        if `bwidth'<=0 {
            di as error "{bf:bwidth()} must be strictly positive"
            exit 198
        }
    }
    else PDF_parse_bwmethod, `bwidth'
    if "`bwadjust'"=="" local bwadjust 1
    if `"`kernel'"'=="" local kernel "epan2"
    if "`adaptive'"=="" local adaptive 1
    PDF_parse_boundary, `boundary'
    
    // mark sample
    marksample touse
    markout `touse' `depvar' `over'
    tempvar touse1 touse0 wvar
    Samplesetup `touse' `touse1' `touse0' `wvar' `depvar' ///
        "`by'" "`swap'" "`refvar'" "`weight'" `"`exp'"' ""
    Check_adjlog `touse' `depvar' `refvar' "`by'" "`adjlog'"
    
    // compute relative PDF
    tempname b se AT ATX BW DIV CHI2 k_omit
    scalar `k_omit' = 0
    mata: rd_PDF(`n')
    
    // returns
    eret post `b' [`weight'`exp'], obs(`N') esample(`touse')
    mata: rd_Post_common_e()
    eret local  subcmd   "pdf"
    eret local  title    "Relative density function"
    eret matrix at       = `AT'
    eret matrix atx      = `ATX'
    if "`nose'"=="" {
        eret matrix se = `se'
    }
    eret scalar n        = `n'
    eret scalar napprox  = `napprox'
    eret scalar bwidth   = `BW'
    if "`bwmethod'"=="dpi" {
        local bwmethod `bwmethod'(`bwdpi')
    }
    eret local  bwmethod "`bwmethod'"
    eret scalar bwadjust = `bwadjust'
    eret local  kernel   "`kernel'"
    eret scalar adaptive = `adaptive'
    eret local  exact    "`exact'"
    eret local  boundary "`boundary'"
    eret local  altlbwf  "`altlbwf'"
    if "`exact'"=="" {
        eret scalar divergence = `DIV'
        eret scalar chi2 = `CHI2'
    }
    if "`nhist'"!="" {
        eret scalar n_hist = `nhist'
        eret scalar hwidth = 1/`nhist'
    }
end

program PDF_parse_bwmethod  // returns: bwmethod, bwdpi
    syntax [, Silverman Normalscale Oversmoothed SJpi Dpi Dpi2(numlist int >=0 max=1) ]
    if "`dpi2'"!="" local dpi dpi
    local bwmethod `silverman' `normalscale' `oversmoothed' `sjpi' `dpi'
    if "`bwmethod'"=="" local bwmethod "sjpi"
    if `: list sizeof bwmethod'>1 {
        di as err "{bf:bwidth()}: may not specify multiple methods"
        exit 198
    }
    if "`dpi2'"=="" local dpi2 2
    c_local bwmethod `bwmethod'
    c_local bwdpi `dpi2'
end

program PDF_parse_boundary // returns: boundary
    syntax [, RENorm REFlect lc ]
    local boundary `renorm' `reflect' `lc'
    if "`boundary'"=="" local boundary "renorm"
    if `: list sizeof boundary'>1 {
        di as err "{bf:boundary()}: may not specify multiple methods"
        exit 198
    }
    c_local boundary `boundary'
end

program HIST, eclass
    // syntax
    Parse_syntax `0'
    syntax [if] [in] [fw iw aw pw/], [ ADJust(str) ///
        n(numlist int >0 max=1) ///
        NOSE Level(cilevel) noHeader NOTABle TABle ///
        GRaph GRaph2(passthru) * ]
    _get_diopts diopts, `options'
    c_local diopts `diopts' `header' `notable' `table' `graph' `graph2'
    Parse_adjust `adjust'
    if "`n'"=="" local n 10
    
    // mark sample
    marksample touse
    markout `touse' `depvar' `over'
    tempvar touse1 touse0 wvar
    Samplesetup `touse' `touse1' `touse0' `wvar' `depvar' ///
        "`by'" "`swap'" "`refvar'" "`weight'" `"`exp'"' ""
    Check_adjlog `touse' `depvar' `refvar' "`by'" "`adjlog'"
    
    // compute relative PDF
    tempname b AT ATX k_omit
    scalar `k_omit' = 0
    mata: rd_HIST(`n')
    
    // returns
    eret post `b' [`weight'`exp'], obs(`N') esample(`touse')
    mata: rd_Post_common_e()
    eret local  subcmd   "histogram"
    eret local  title    "Relative histogram"
    eret matrix at       = `AT'
    eret matrix atx      = `ATX'
    eret scalar n_hist   = `n'
    eret scalar hwidth   = 1/`n'
end

program CDF, eclass
    // syntax
    Parse_syntax `0'
    syntax [if] [in] [fw iw aw pw/], [ ADJust(str) ///
        n(numlist int >0 max=1) ///
        at(str) atx(str) ///
        NOSE Level(cilevel) noHeader NOTABle TABle ///
        GRaph GRaph2(passthru) * ]
    _get_diopts diopts, `options'
    c_local diopts `diopts' `header' `notable' `table' `graph' `graph2'
    Parse_adjust `adjust'
    Parse_at "`n'" `"`at'"' `"`atx'"'
    
    // mark sample
    marksample touse
    markout `touse' `depvar' `over'
    tempvar touse1 touse0 wvar
    Samplesetup `touse' `touse1' `touse0' `wvar' `depvar' ///
        "`by'" "`swap'" "`refvar'" "`weight'" `"`exp'"' ""
    Check_adjlog `touse' `depvar' `refvar' "`by'" "`adjlog'"
    
    // compute relative CDF
    tempname b AT ATX k_omit
    scalar `k_omit' = 0
    mata: rd_CDF(`n')
    
    // returns
    eret post `b' [`weight'`exp'], obs(`N') esample(`touse')
    mata: rd_Post_common_e()
    eret local  subcmd   "cdf"
    eret local  title    "Cumulative relative distribution"
    eret matrix at       = `AT'
    eret matrix atx      = `ATX'
    eret scalar n        = `n'
end

program MRP, eclass
    // syntax
    Parse_syntax `0'
    syntax [if] [in] [fw iw aw pw/], [ Over(varname numeric) ///
        SCale SCale2(str) MULTiplicative LOGarithmic ///
        NOSE Level(cilevel) noHeader NOTABle TABle * ]
    _get_diopts diopts, `options'
    c_local diopts `diopts' `header' `notable' `table'
    if `"`scale2'"'!="" local scale scale
    local adj1 location
    if "`scale'"!="" {
        if "`multiplicative'"!="" {
            di as err "{bf:scale} and {bf:multiplicative} not both allowed"
            exit 198
        }
        local adj1 `adj1' scale
        if `"`scale2'"'=="sd" {
            local adjsd sd
        }
        else if `"`scale2'"'!="" {
            if `"`scale2'"'!=substr("iqrange",1,max(3,strlen(`"`scale2'"'))) {
                di as err "'" `"`scale2'"' "' not allowed in {bf:scale()}"
                exit 198
            }
        }
    }
    else if "`multiplicative'"!="" {
        local adjmult multiplicative
    }
    if "`logarithmic'"!="" {
        if "`multiplicative'"!="" {
            di as err "{bf:logarithmic} and {bf:multiplicative} not both allowed"
            exit 198
        }
        local adjlog logarithmic
    }
    
    // mark sample
    marksample touse
    markout `touse' `depvar' `over'
    tempvar touse1 touse0 wvar
    Samplesetup `touse' `touse1' `touse0' `wvar' `depvar' ///
        "`by'" "`swap'" "`refvar'" "`weight'" `"`exp'"' "`over'"
    Check_adjlog `touse' `depvar' `refvar' "`by'" "`adjlog'"
    
    // compute polarization statistics
    tempname b btmp k_omit
    scalar `k_omit' = 0
    if "`over'"=="" {
        mata: rd_MRP("`b'")
    }
    else {
        local TOUSE1 `touse1'
        local TOUSE0 `touse0'
        tempname touse1 touse0 _N _N1 _N0 
        PrepareOver `N_over' "`overlevels'" "`by'" ///
            `touse1' `touse0' `_N' `_N1' `_N0'
        local i 0
        foreach o of local overlevels {
            local ++i
            PrepareOverlevel `i' "`over'==`o'" "`by'" `touse' `touse1' ///
                `touse0' `TOUSE1' `TOUSE0' `_N' `_N1' `_N0' "`wgt'"
            mata: rd_MRP("`btmp'")
            mata: rd_FlagOmitted("`btmp'") // stats can be missing if too few obs
            mat coleq `btmp' = "`o'"
            mat `b' = nullmat(`b'), `btmp'
        }
    }
    
    // returns
    eret post `b' [`weight'`exp'], obs(`N') esample(`touse')
    mata: rd_Post_common_e()
    eret local  subcmd   "mrp"
    eret local  title    "Median relative polarization"
end

program SUM, eclass
    // syntax
    Parse_syntax `0'
    syntax [if] [in] [fw iw aw pw/], [ ADJust(str) Over(varname numeric) ///
        Statistics(passthru) Generate(name) Replace ///
        NOSE Level(cilevel) noHeader NOTABle TABle * ]
    _get_diopts diopts, `options'
    c_local diopts `diopts' `header' `notable' `table'
    Parse_adjust `adjust'
    if "`generate'"!="" & "`replace'"=="" {
        confirm new variable `generate'
    }
    
    // mark sample
    marksample touse
    markout `touse' `depvar' `over'
    tempvar touse1 touse0 wvar
    Samplesetup `touse' `touse1' `touse0' `wvar' `depvar' ///
        "`by'" "`swap'" "`refvar'" "`weight'" `"`exp'"' "`over'"
    Check_adjlog `touse' `depvar' `refvar' "`by'" "`adjlog'"
    
    // compute relative ranks and statistics
    tempvar ranks
    qui gen double `ranks' = .
    tempname b btmp k_omit
    scalar `k_omit' = 0
    if "`over'"=="" {
        mata: rd_SUM()
        quietly tabstat `ranks' if `touse' `swgt', save `statistics'
        mat `b' = r(StatTotal)'
        local statnames: coln `b'
        mata: rd_FlagOmitted("`b'") // stats can be missing if too few obs
    }
    else {
        local TOUSE1 `touse1'
        local TOUSE0 `touse0'
        tempname touse1 touse0 _N _N1 _N0 
        PrepareOver `N_over' "`overlevels'" "`by'" ///
            `touse1' `touse0' `_N' `_N1' `_N0'
        local i 0
        foreach o of local overlevels {
            local ++i
            PrepareOverlevel `i' "`over'==`o'" "`by'" `touse' `touse1' ///
                `touse0' `TOUSE1' `TOUSE0' `_N' `_N1' `_N0' "`wgt'"
            mata: rd_SUM()
            quietly tabstat `ranks' if `touse' & `over'==`o' `swgt', save `statistics'
            mat `btmp' = r(StatTotal)'
            if `i'==1 local statnames: coln `btmp'
            mata: rd_FlagOmitted("`btmp'") // stats can be missing if too few obs
            mat coleq `btmp' = "`o'"
            mat `b' = nullmat(`b'), `btmp'
        }
    }
    
    // returns
    eret post `b' [`weight'`exp'], obs(`N') esample(`touse')
    mata: rd_Post_common_e()
    eret local subcmd     "summarize"
    eret local title      "Relative ranks"
    eret local statistics "`statnames'"
    
    // store in variable
    if "`generate'"!="" {
        eret local generate `generate'
        capt confirm new variable `generate'
        if _rc==1 exit _rc
        if _rc drop `generate'
        rename `ranks' `generate'
        lab var `generate' "Relative ranks"
    }
end

version 12
// struct
local DATA   rd_data
local Data   struct `DATA' scalar
local ADJ    rd_adjset
local Adj    struct `ADJ' scalar
local MRP    rd_mrp
local Mrp    struct `Mrp' scalar
// string
local SS     string scalar
local SR     string rowvector
local SM     string matrix
// real
local RS     real scalar
local RC     real colvector
local RR     real rowvector
local RM     real matrix
// counters
local Int    real scalar
// boolean
local Bool   real scalar
local TRUE   1
local FALSE  0
// pointer
local PSRC   pointer(real colvector) scalar
local PSRCF  pointer(real colvector function) scalar
mata:
mata set matastrict on

/* Helper functions directly called by ado ----------------------------------*/

void rd_Post_common_e()
{
    st_global("e(cmd)", "reldist")
    st_global("e(depvar)", st_local("depvar"))
    if (st_local("by")!="") {
        st_global("e(by)", st_local("by"))
        st_global("e(by1lab)", st_local("by1lab"))
        st_global("e(by0lab)", st_local("by0lab"))
        stata("ereturn scalar by1 = " + st_local("by1"))
        stata("ereturn scalar by0 = " + st_local("by0"))
        stata("ereturn scalar N1 = " + st_local("N1"))
        stata("ereturn scalar N0 = " + st_local("N0"))
    }
    else {
        st_global("e(refvar)", st_local("refvar"))
    }
    st_global("e(adjust)",    st_local("adj1"))
    st_global("e(refadjust)", st_local("adj0"))
    st_global("e(adjmean)",   st_local("adjmean"))
    st_global("e(adjsd)",     st_local("adjsd"))
    st_global("e(adjlog)",    st_local("adjlog"))
    st_global("e(adjmult)",   st_local("adjmult"))
    if (st_local("over")!="") {
        st_global("e(over)", st_local("over"))
        st_global("e(over_namelist)", st_local("overlevels"))
        st_global("e(over_labels)", st_local("over_labels"))
        stata("ereturn scalar N_over = " + st_local("N_over"))
        stata("ereturn matrix _N = " + st_local("_N"))
        if (st_local("by")!="") {
            stata("ereturn matrix _N1 = " + st_local("_N1"))
            stata("ereturn matrix _N0 = " + st_local("_N0"))
        }
    }
    stata("ereturn scalar level = " + st_local("level"))
    stata("ereturn scalar k_omit = " + st_local("k_omit"))
}

void rd_FlagOmitted(`SS' bnm)
{
    `Int' i, k_omit
    `RR'  b
    `SM'  cstripe
    
    b = st_matrix(bnm)
    if (hasmissing(b)==0) return
    cstripe = st_matrixcolstripe(bnm)
    k_omit = st_numscalar(st_local("k_omit"))
    i = cols(b)
    for (;i;i--) {
        if (b[i]>=.) {
            b[i] = 0
            cstripe[i,2] = "o." + cstripe[i,2]
            k_omit++
        }
    }
    st_matrix(bnm, b)
    st_matrixcolstripe(bnm, cstripe)
    st_numscalar(st_local("k_omit"), k_omit)
}

void rd_svmat(`SS' nm, `SR' vnms, `Bool' transpose)
{
    `RM' M

    if (transpose) M = st_matrix(nm)'
    else           M = st_matrix(nm)
    st_store((1,rows(M)), st_addvar("double",vnms), M)
}

void rd_olab_ipolate(`SS' nm, `SS' fmt)
{
    `RC' x, x0, y0
    
    x  = strtoreal(tokens(st_local(nm)))'
    if (length(x)==0) {
        st_local(nm,"")
        return
    }
    x0 = st_matrix("e(atx)")'
    y0 = st_matrix("e(at)")'
    if (st_global("e(subcmd)")=="pdf") {
        // (there might be a second equation containing the histogram)
        x0 = x0[|1\st_numscalar("e(n)")|]
        y0 = y0[|1\st_numscalar("e(n)")|]
    }
    st_local(nm, _rd_OLABEL(mm_ipolate(x0, y0, x, 1), x, fmt))
}

/* Setup for estimation -----------------------------------------------------*/

struct `ADJ' {
    `Bool'  adj       // has adjustment
    `Bool'  location  // adjust location
    `Bool'  scale     // adjust scale
    `Bool'  shape     // adjust shape
}

struct `DATA' {
    `RC'    y1, w1    // data and weights comparing group
    `RS'    N1        // N of comparing group
    `RC'    y0, w0    // data and weights reference group
    `RS'    N0        // N of referenc group
    `Int'   wtype     // weights: 0 none, 1 fw, 2 pw, 3 aw, 4 iw
    `Adj'   adj1      // comparison group adjustment
    `Adj'   adj0      // reference group adjustment
    `Bool'  adjmean   // 0 use median, 1 use mean
    `Bool'  adjsd     // 0 use IQR, 1 use sd
    `Int'   adjlink   // 0 linear/additive, 1 logarithmic, 2 multiplicative
    `PSRC'  Y1, W1    // (adjusted) comparison data
    `PSRC'  Y0, W0    // (adjusted) reference data
    `RC'    ranks     // relative ranks
}

void rd_getadj(`Adj' adj, `SS' lnm)
{
    `SR'    ADJ
    
    ADJ = tokens(st_local(lnm))
    adj.adj      = (length(ADJ)!=0)
    adj.location = anyof(ADJ, "location")
    adj.scale    = anyof(ADJ, "scale")
    adj.shape    = anyof(ADJ, "shape")
}

void rd_get_at(`RC' at, `RC' atx, `Int' n)
{
    if (st_local("atx")!="")       atx = strtoreal(tokens(st_local("atx")))'
    else if (st_local("ATX0")!="") atx = _rd_get_at_mat("ATX0")
    else if (st_local("at")!="")   at = strtoreal(tokens(st_local("at")))'
    else if (st_local("AT0")!="") {
        at = _rd_get_at_mat("AT0")
        if (any(at:<0) | any(at:>1)) {
            display("{err}values provided in {bf:at()} must be in [0,1]")
            exit(error(125))
        }
    }
    else at = rangen(0, 1, n)
}

`RC' _rd_get_at_mat(`SS' nm)
{
    `RM' at
    
    at = st_matrix(st_local(nm))'
    if (cols(at)>rows(at)) at = at'
    if (cols(at)>1) at = at[,1]
    return(sort(at,1))
}

void rd_getdata(`Data' data)
{
    `SS'   weight
    `Int'  touse1, touse0
    
    // setup
    weight      = st_local("weight")
    data.wtype  = (weight=="fweight" ? 1 :
                  (weight=="pweight" ? 2 :
                  (weight=="aweight" ? 3 :
                  (weight=="iweight" ? 4 : 0))))
    touse1      = _st_varindex(st_local("touse1"))
    touse0      = _st_varindex(st_local("touse0"))
    
    // comparison group data
    data.y1 = st_data(., st_local("depvar"), touse1)
    if (data.wtype) data.w1 = st_data(., st_local("wvar"), touse1)
    else            data.w1 = 1
    if (data.wtype>1) { // pw, aw, iw
        data.N1 = rows(data.y1)
        data.w1 = data.w1 * data.N1 / quadsum(data.w1) // normalize weights
    }
    else if (data.wtype) data.N1 = sum(data.w1) // fw
    else                 data.N1 =rows(data.y1) // no weights

    // reference group data
    data.y0 = st_data(., st_local("refvar"), touse0)
    if (data.wtype) data.w0 = st_data(., st_local("wvar"), touse0)
    else            data.w0 = 1
    if (data.wtype>1) { // pw, aw, iw
        data.N0 = rows(data.y0)
        data.w0 = data.w0 * data.N0 / quadsum(data.w0) // normalize weights
    }
    else if (data.wtype) data.N0 = sum(data.w0) // fw
    else                 data.N0 =rows(data.y0) // no weights
    
    // adjustment settings
    rd_getadj(data.adj1, "adj1")
    rd_getadj(data.adj0, "adj0")
    data.adjmean = (st_local("adjmean")!="")
    data.adjsd   = (st_local("adjsd")!="")
    data.adjlink = 0 + (st_local("adjlog")!="") + 2*(st_local("adjmult")!="")
}

void rd_adjust(`Data' data)
{
    _rd_adjust(data.Y1, data.W1, data.y1, data.w1, data.y0, data.w0, data.adj1,
        data.adjmean, data.adjsd, data.adjlink)
    _rd_adjust(data.Y0, data.W0, data.y0, data.w0, data.y1, data.w1, data.adj0,
        data.adjmean, data.adjsd, data.adjlink)
}

void _rd_adjust(`PSRC' Y, `PSRC' W, `RC' y, `RC' w, `RC' y0, `RC' w0, `Adj' adj,
    `Bool' mean, `Bool' sd, `Int' link)
{
    if (adj.adj==0) {
        Y = &y
        W = &w
        return
    }
    if (adj.shape) {
        if (adj.location & adj.scale) Y = &y0
        else if (adj.location) Y = _rd_adjust_s(y0, w0, y, w, mean, sd, link)
        else if (adj.scale) Y = _rd_adjust_l(y0, w0, y, w, mean, link)
        else Y = _rd_adjust_ls(y0, w0, y, w, mean, sd, link)
        W = &w0
        return
    }
    if (adj.location & adj.scale) Y = _rd_adjust_ls(y, w, y0, w0, mean, sd, link)
    else if (adj.location) Y = _rd_adjust_l(y, w, y0, w0, mean, link)
    else if (adj.scale) Y = _rd_adjust_s(y, w, y0, w0, mean, sd, link)
    W = &w
}

`PSRC' _rd_adjust_ls(`RC' y, `RC' w, `RC' y0, `RC' w0, `Bool' mean, `Bool' sd,
    `Int' link)
{   // scale and location adjustment
    `RS' l, l0, s, s0
    `RC' lny
    
    if (link==1) {                       // logarithmic
        lny = ln(y)
        l  = (mean ? mean(lny, w)     : mm_median(lny, w))
        l0 = (mean ? mean(ln(y0), w0) : mm_median(ln(y0), w0))
        s  = (sd ? sqrt(variance(lny, w))     : mm_iqrange(lny, w))
        s0 = (sd ? sqrt(variance(ln(y0), w0)) : mm_iqrange(ln(y0), w0))
        return(&(exp((lny :- l) * (s0 / s) :+ l0)))
    }
    l  = (mean ? mean(y, w)   : mm_median(y, w))
    l0 = (mean ? mean(y0, w0) : mm_median(y0, w0))
    if (link==2) return(&(y * (l0 / l))) // multiplicative (scale not relevant)
    s  = (sd ? sqrt(variance(y, w))   : mm_iqrange(y, w))
    s0 = (sd ? sqrt(variance(y0, w0)) : mm_iqrange(y0, w0))
    return(&((y :- l) * (s0 / s) :+ l0)) // additive
}

`PSRC' _rd_adjust_l(`RC' y, `RC' w, `RC' y0, `RC' w0, `Bool' mean, `Int' link)
{   // location adjustment
    `RS' l, l0
    `RC' lny
    
    if (link==1) {                       // logarithmic
        lny = ln(y)
        l  = (mean ? mean(lny, w)     : mm_median(lny, w))
        l0 = (mean ? mean(ln(y0), w0) : mm_median(ln(y0), w0))
        return(&(exp(lny :+ (l0 - l))))
    }
    l  = (mean ? mean(y, w)   : mm_median(y, w))
    l0 = (mean ? mean(y0, w0) : mm_median(y0, w0))
    if (link==2) return(&(y * (l0 / l))) // multiplicative
    return(&(y :+ (l0 - l)))             // additive
}

`PSRC' _rd_adjust_s(`RC' y, `RC' w, `RC' y0, `RC' w0, `Bool' mean, `Bool' sd,
    `Int' link)
{   // scale adjustment
    `RS' l, s, s0
    `RC' lny
    
    if (link==1) {                       // logarithmic
        lny = ln(y)
        l  = (mean ? mean(lny, w) : mm_median(lny, w))
        s  = (sd ? sqrt(variance(lny, w))     : mm_iqrange(lny, w))
        s0 = (sd ? sqrt(variance(ln(y0), w0)) : mm_iqrange(ln(y0), w0))
        return(&(exp((lny :- l) * (s0 / s) :+ l)))
    }
    if (link==2) return(&y)              // multiplicative (scale not relevant)
    l  = (mean ? mean(y, w) : mm_median(y, w))
    s  = (sd ? sqrt(variance(y, w))   : mm_iqrange(y, w))
    s0 = (sd ? sqrt(variance(y0, w0)) : mm_iqrange(y0, w0))
    return(&((y :- l) * (s0 / s) :+ l)) // additive
}

void rd_relrank(`Data' data)
{
    data.ranks = mm_relrank(*data.Y0, *data.W0, *data.Y1, 1)
}

/* PDF estimation -----------------------------------------------------------*/

void rd_PDF(`Int' n)
{
    `Bool'  exact, altlbwf, pw, nose
    `Int'   adaptive, boundary, n0, nhist
    `RS'    bw, bwdpi, bwadj
    `SS'    bwmethod, kernel
    `RC'    b, v, at, atx, at0, lbwf, gc
    `SM'    cstripe
    `Data'  data
    `PSRCF' lbwffun
    pragma unset at
    pragma unset atx
    pragma unset lbwf
    pragma unset gc
    
    // settings
    bw       = strtoreal(st_local("bwidth"))
    bwmethod = st_local("bwmethod")
    bwdpi    = strtoreal(st_local("bwdpi"))
    bwadj    = strtoreal(st_local("bwadjust"))
    kernel   = _mm_unabkern(st_local("kernel"))
    adaptive = strtoreal(st_local("adaptive"))
    exact    = (st_local("exact")!="")
    n0       = strtoreal(st_local("napprox"))
    altlbwf  = (st_local("altlbwf")!="")
    boundary = 0 + (st_local("boundary")=="reflect") + 2*(st_local("boundary")=="lc")
    nose     = (st_local("nose")!="")
    nhist    = strtoreal(st_local("nhist"))
    
    // evaluation grid: step 1
    rd_get_at(at, atx, n)
    
    // prepare data
    rd_getdata(data)
    rd_adjust(data)
    rd_relrank(data)
    pw = (data.wtype==2)
    
    // evaluation grid: step 2
    if (length(at)) atx = mm_quantile(*data.Y0, *data.W0, at)
    else            at  = mm_relrank(*data.Y0, *data.W0, atx, 1)

    // bandwidth
    if (bw>=.) {
        bw = rd_PDF_bw(data.ranks, *data.W1, bwmethod, bwdpi, kernel, n0)
        if (bw>=.) {
            display("{txt}(bandwidth estimation failed; using oversmoothed bandwidth)")
            bw = rd_PDF_bw(data.ranks, *data.W1, "oversmoothed", bwdpi, kernel, n0)
        }
        if (data.wtype==2) {    // adjustment in case of pweights
            bw = bw * (colsum(*data.W1:^2)/rows(*data.W1))^.2
        }
    }
    bw = bw * bwadj
    
    // estimation
    lbwffun = (altlbwf ? &rd_PDF_lbwf() : &kdens_lbwf())
    if (exact) {    // exact estimator: can directly use evaluation grid
        b = _kdens(data.ranks, *data.W1, at, bw, kernel, adaptive, 0, 1, 
            boundary, lbwf, lbwffun)
    }
    else {          // binned approximation estimator: use grid of size n0
        at0 = rangen(0, 1, n0)
        b = kdens(data.ranks, *data.W1, at0, bw, kernel, adaptive, 1, 1, 
            boundary, lbwf, gc, 1, lbwffun)
    }
    
    // standard errors
    if (nose==0) {
        if (exact) {
            v = _kdens_var(b, data.ranks, *data.W1, at, bw, kernel, pw, 0, 1,
                boundary, lbwf)
            if (rows(lbwf)>1) lbwf = mm_ipolate(data.ranks, lbwf, at, 1)
        }
        else {
            v = kdens_var(b, data.ranks, *data.W1, at0, bw, kernel, pw, 1, 1,
                boundary, lbwf, gc)
        }
        v = v + rd_PDF_varincr(data.N0, data.w0, b, bw*lbwf, kernel, pw)
    }
    
    // return results
    if (nhist<.) cstripe = J(n, 1, "pdf")
    else         cstripe = J(n, 1, "")
    cstripe = cstripe, "p" :+ strofreal(1::n)
    if (exact) st_matrix(st_local("b"), b')
    else       st_matrix(st_local("b"), mm_ipolate(at0, b, at)')
    st_matrixcolstripe(st_local("b"), cstripe)
    st_matrix(st_local("AT"), at')
    st_matrixcolstripe(st_local("AT"), cstripe)
    st_matrix(st_local("ATX"), atx')
    st_matrixcolstripe(st_local("ATX"), cstripe)
    st_numscalar(st_local("BW"), bw)
    st_local("kernel", kernel)
    if (nose==0) {
        if (exact) st_matrix(st_local("se"), sqrt(v)')
        else       st_matrix(st_local("se"), mm_ipolate(at0, sqrt(v), at)')
        st_matrixcolstripe(st_local("se"), cstripe)
    }
    
    // divergence
    if (exact==0) {
        gc = J(rows(b),1,1); gc[1] = .5; gc[rows(gc)] = .5 // 1/2 weight at boundary
        st_numscalar(st_local("DIV"), mean(b :* ln(b), gc))
        st_numscalar(st_local("CHI2"), mean((b :-1):^2, gc))
    }
    
    // append histogram
    if (nhist<.) {
        b = _rd_HIST(nhist, data, at, atx)
        cstripe = cstripe \ (J(nhist,1,"histogram"), "h":+strofreal(1::nhist))
        st_matrix(st_local("b"), (st_matrix(st_local("b"))' \ b)')
        st_matrixcolstripe(st_local("b"), cstripe)
        st_matrix(st_local("AT"), (st_matrix(st_local("AT"))' \ at)')
        st_matrixcolstripe(st_local("AT"), cstripe)
        st_matrix(st_local("ATX"), (st_matrix(st_local("ATX"))' \ atx)')
        st_matrixcolstripe(st_local("ATX"), cstripe)
        if (nose==0) {
            st_matrix(st_local("se"), (st_matrix(st_local("se")), J(1,nhist,0)))
            st_matrixcolstripe(st_local("se"), cstripe)
        }
    }
}

// bandwidth estimation
`RS' rd_PDF_bw(`RC' x, `RC' w, `SS' method, `Int' dpi, `SS' kernel, `Int' m) 
{
    `RS' bw

    if (method=="sjpi")     bw = rd_PDF_bw_sjpi(x, w, m, "minim")
    else if (method=="dpi") bw = rd_PDF_bw_dpi(x, w, m, "minim", dpi)
    else                    bw = rd_PDF_bw_simple(x, w, method)
    return( (*_mm_findkdel0(kernel))() * bw )
}

// alternative local bandwidth factor function
`RC' rd_PDF_lbwf(`RC' x, `RC' w, `RC' g, `RC' d)
{
    `RC' l

    if (x==g) l = d
    else      l = mm_ipolate(g, d, x)
    //l = l:*(l:>=1) + (2:-l):*(l:<1)    // 1 + |deviation from 1|
    l = l:*(l:>=1) + (1:/l):*(l:<1)      // symmetric
    l = sqrt( exp(mean(log(l), w)) :/ l)
    _editmissing(l, 1)
    return(l)
}

// bandwidth estimation: modified rule-of-thumb methods
`RS' rd_PDF_bw_simple(`RC' x, `RC' w, `SS' method)
{
    `SS' scale

    scale = (method=="oversmoothed" ? "stddev" : "minim")
    return(kdens_bw_simple(x, w, method, scale) *
        (1 + 1/(2*sqrt(pi())*_kdens_bw_scale(x, w, scale)))^.2)
}

// bandwidth estimation: modified DPI
`RS' rd_PDF_bw_dpi(`RC' x, `RC' w, `Int' m, `SS' scale, `Int' level)
{
    `RS' n, s, psi, alpha, i, psi0, alpha0
    `RC' g, gc

    // grid
    g  = mm_makegrid(x, m, 0, 0, 1)
    gc = mm_fastlinbin(x, w, g)
    n  = colsum(gc)
    s  = _kdens_bw_scale(x, w, scale)

    // plug-in steps
    if (level==0) {
        psi = 3/(8*sqrt(pi())*s^5)
        psi0 = 1/(2*sqrt(pi())*s)
    }
    else {
        alpha = (2*(sqrt(2)*s)^(3+2*(level+1)) /
            ((1+2*(level+1))*n))^(1/(3+2*(level+1)))
        alpha0 = (2*(sqrt(2)*s)^(3+2*(level-1)) /
            ((1+2*(level-1))*n))^(1/(3+2*(level-1)))
        for (i=level; i>=1; i--) {
            psi = kdens_df(g, gc, alpha, i+1, 1, 1)
            psi0 = kdens_df(g, gc, alpha0, i-1, 1, 1)
            if (i>1) {
                alpha = ( factorial(i*2)/(2^i*factorial(i)) *
                    sqrt(2/pi())/(psi*n) )^(1/(3+2*(i)))
                alpha0 = ( factorial((i-2)*2)/(2^(i-2)*factorial(i-2)) *
                    sqrt(2/pi())/(psi0*n) )^(1/(3+2*(i-2)))
            }
        }
    }
    return( ((1+psi0)/(psi*n))^(1/5) )
}

// bandwidth estimation: modified SJPI
`RS' rd_PDF_bw_sjpi(`RC' x, `RC' w, `Int' m, `SS' scale)
{
    `RS' n, s, lambda, hmin, ax, bx, rc
    `RC' g, gc
    `RR' h

    // grid
    g       = mm_makegrid(x, m, 0, 0, 1)
    gc      = mm_fastlinbin(x, w, g)
    n       = colsum(gc)
    s       = sqrt(variance(x, w))
    if      (scale=="minim")  lambda = min((s, mm_iqrange(x, w) / 1.349))
    else if (scale=="stddev") lambda = s
    else if (scale=="iqr")    lambda = mm_iqrange(x, w) / 1.349
    else    _error(3498, `"""' + scale + `"" invalid"')
    if      (lambda<=0)       lambda = s

    // root finding
    hmin = (g[rows(g)]-g[1])/(rows(g)-1) / 2 *
        mm_kdel0_gaussian() / mm_kdel0_rectangle()
    bx = s * (243/(35*n))^.2 * mm_kdel0_gaussian() *
        (1 + 1/(2*sqrt(pi())*s))^.2     // h_oversmoothed
    while (1) {
        if (hmin>=bx) return(.)
        ax = max((hmin, bx*0.1))
        rc = mm_root(h=., &_rd_PDF_bw_sjpi(), ax, bx, ax*0.1, 100,
            g, gc, lambda)
        if ( rc==2 ) bx = ax            // continue if solution < ax
        else return(h / mm_kdel0_gaussian())
    }
}

`RS' _rd_PDF_bw_sjpi(`RS' h, `RC' g, `RC' gc, `RS' lambda)
{
    `RS' n, a, b, tdb, sda, alpha2, sdalpha2, c, tdc, beta, sdbeta

    n           = colsum(gc)
    a           = 1.241 * lambda * n^(-1/7)
    b           = 1.230 * lambda * n^(-1/9)
    tdb         = kdens_df(g, gc, b, 3, 1, 1)
    sda         = kdens_df(g, gc, a, 2, 1, 1)
    alpha2      = 1.357 * (sda/tdb)^(1/7) * h^(5/7)
    sdalpha2    = kdens_df(g, gc, alpha2, 2, 1, 1)
    c           = 1.304 * lambda * n^(-1/5)
    tdc         = kdens_df(g, gc, c, 1, 1, 1)
    beta        = 1.414 * (sda/tdc)^(1/3) * h^(5/3)
    sdbeta      = kdens_df(g, gc, beta, 0, 1, 1)
    //siple alternative (Cwik and Mileniczuk 1993):
    //c           = 1.781 * lambda * n^(-1/3)
    //sdbeta      = kdens_df(g, gc, c, 0, 1, 1)
    return(((mm_kint_gaussian(2) * (1 + sdbeta))/(n * sdalpha2))^0.2 - h)
}

// variance correction
`RC' rd_PDF_varincr(`RS' N0, `RC' w0, `RC' g, `RC' h, `SS' kernel, `Bool' pw)
{
    `RC' c

    c = (g:^2 * (*_mm_findkint(kernel))(2)) :/ h
    if (pw) c = c * (colsum(w0:^2) / rows(w0)^2)
    else    c = c / N0
    return(c)
}

/* histogram estimation -----------------------------------------------------*/

void rd_HIST(`Int' n)
{
    `RC'    b, at, atx
    `SM'    cstripe
    `Data'  data
    pragma unset at
    pragma unset atx
    
    // prepare data
    rd_getdata(data)
    rd_adjust(data)
    rd_relrank(data)
    
    // estimation
    b = _rd_HIST(n, data, at, atx)
    
    // return results
    cstripe = J(n,1,""), "h":+strofreal(1::n)
    st_matrix(st_local("b"), b')
    st_matrixcolstripe(st_local("b"), cstripe)
    st_matrix(st_local("AT"), at')
    st_matrixcolstripe(st_local("AT"), cstripe)
    st_matrix(st_local("ATX"), atx')
    st_matrixcolstripe(st_local("ATX"), cstripe)
}

`RC' _rd_HIST(`Int' n, `Data' data, `RC' at, `RC' atx)
{
    `RC' b
    
    at = rangen(0, 1, n+1)
    b = mm_exactbin(data.ranks, *data.W1, at, 1)
    b = b * (n / mm_nobs(data.ranks, *data.W1))
    at = at[|1 \ n |] :+ .5/n
    atx = mm_quantile(*data.Y0, *data.W0, at)
    return(b)
}

/* CDF estimation -----------------------------------------------------------*/

void rd_CDF(`Int' n)
{
    `RC'   b, at, atx
    `SM'   cstripe
    `Data' data
    pragma unset at
    pragma unset atx
    
    // evaluation grid: step 1
    rd_get_at(at, atx, n)
    
    // prepare data
    rd_getdata(data)
    rd_adjust(data)
    rd_relrank(data)
    
    // evaluation grid: step 2
    if (length(at)) atx = mm_quantile(*data.Y0, *data.W0, at)
    else            at  = mm_relrank(*data.Y0, *data.W0, atx, 1)
    
    // estimation
    b  = mm_relrank(data.ranks, *data.W1, at)
    
    // return results
    cstripe = (J(n,1,""), "p":+strofreal(1::n))
    st_matrix(st_local("b"), b')
    st_matrixcolstripe(st_local("b"), cstripe)
    st_matrix(st_local("AT"), at')
    st_matrixcolstripe(st_local("AT"), cstripe)
    st_matrix(st_local("ATX"), atx')
    st_matrixcolstripe(st_local("ATX"), cstripe)
}

/* MRP estimation -----------------------------------------------------------*/

void rd_MRP(`SS' bnm)
{
    `RR'   b
    `RC'   d
    `Data' data
    
    // prepare data
    rd_getdata(data)
    rd_adjust(data)
    rd_relrank(data)
    
    // estimation
    d = data.ranks :- 0.5
    b = J(1,3,.)
    b[2] = 8 * mean( -d :* (d:<0), *data.W1) - 1 // LRP
    b[3] = 8 * mean(  d :* (d:>0), *data.W1) - 1 // URP
    b[1] = (b[2] + b[3]) / 2                     // MRP
    //b[1] = 4 * mean(abs(d), *data.W1) - 1
    
    // return results
    st_matrix(bnm, b)
    st_matrixcolstripe(bnm, (J(3,1,""), tokens("MRP LRP URP")'))
}

/* Generate relative ranks for summarize ------------------------------------*/

void rd_SUM()
{
    `Data' data
    
    // prepare data
    rd_getdata(data)
    rd_adjust(data)
    rd_relrank(data)
    
    // return results
    if (data.adj1.shape) 
         st_store(., st_local("ranks"), st_local("touse0"), data.ranks)
    else st_store(., st_local("ranks"), st_local("touse1"), data.ranks)
    // // check adjustments
    // data.adjmean, data.adjlink
    // if (data.adjmean==0) {
    //      if (data.adjlink==1) { // log
    //         "location"
    //         mm_median(log(data.y1),data.w1), data.N1, mm_median(log(*data.Y1),*data.W1), rows(*data.Y1)
    //         mm_median(log(data.y0),data.w0), data.N0, mm_median(log(*data.Y0),*data.W0), rows(*data.Y0)
    //         "scale"
    //         mm_iqrange(log(data.y1),data.w1), data.N1, mm_iqrange(log(*data.Y1),*data.W1), rows(*data.Y1)
    //         mm_iqrange(log(data.y0),data.w0), data.N0, mm_iqrange(log(*data.Y0),*data.W0), rows(*data.Y0)
    //     }
    //     else {
    //        "location"
    //        mm_median(data.y1,data.w1), data.N1, mm_median(*data.Y1,*data.W1), rows(*data.Y1)
    //        mm_median(data.y0,data.w0), data.N0, mm_median(*data.Y0,*data.W0), rows(*data.Y0)
    //        "scale"
    //        mm_iqrange(data.y1,data.w1), data.N1, mm_iqrange(*data.Y1,*data.W1), rows(*data.Y1)
    //        mm_iqrange(data.y0,data.w0), data.N0, mm_iqrange(*data.Y0,*data.W0), rows(*data.Y0)
    //    }
    // }
    // else {
    //     if (data.adjlink==1) { // log
    //         "location"
    //         mean(log(data.y1),data.w1), data.N1, mean(log(*data.Y1),*data.W1), rows(*data.Y1)
    //         mean(log(data.y0),data.w0), data.N0, mean(log(*data.Y0),*data.W0), rows(*data.Y0)
    //         "scale"
    //         sqrt(variance(log(data.y1),data.w1)), data.N1, sqrt(variance(log(*data.Y1),*data.W1)), rows(*data.Y1)
    //         sqrt(variance(log(data.y0),data.w0)), data.N0, sqrt(variance(log(*data.Y0),*data.W0)), rows(*data.Y0)
    //    }
    //    else {
    //         "location"
    //         mean(data.y1,data.w1), data.N1, mean(*data.Y1,*data.W1), rows(*data.Y1)
    //         mean(data.y0,data.w0), data.N0, mean(*data.Y0,*data.W0), rows(*data.Y0)
    //         "scale"
    //         sqrt(variance(data.y1,data.w1)), data.N1, sqrt(variance(*data.Y1,*data.W1)), rows(*data.Y1)
    //         sqrt(variance(data.y0,data.w0)), data.N0, sqrt(variance(*data.Y0,*data.W0)), rows(*data.Y0)
    //     }
    // }
}

/* Obtain ranks of olabel values in reference distribution ------------------*/

void rd_OLABEL()
{
    `Data' data
    `RC' xlab, xtic
    `SS' fmt
    
    // olabel values
    xlab = strtoreal(tokens(st_local("olabel")))'
    xtic = strtoreal(tokens(st_local("otick")))'
    fmt = st_local("format")
    if (length(xlab)==0 & length(xtic)==0) return // nothing to do
    
    // prepare data
    rd_getdata(data)
    rd_adjust(data)
    
    // obtain positions
    if (length(xlab)!=0) st_local("olabel", 
        _rd_OLABEL(mm_relrank(*data.Y0, *data.W0, xlab, 1), xlab, fmt))
    if (length(xtic)!=0) st_local("otick", 
        _rd_OLABEL(mm_relrank(*data.Y0, *data.W0, xtic, 1)))
}

`SS' _rd_OLABEL(`RC' y, | `RC' x, `SS' fmt)
{
    if (fmt=="") return(invtokens(strofreal(y)'))
    return(invtokens((strofreal(y) :+ `" ""' :+ strofreal(x, fmt)  :+ `"""')'))
}

end
exit

