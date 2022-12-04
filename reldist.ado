*! version 1.3.1  04dec2022  Ben Jann

capt findfile lmoremata.mlib
if _rc {
    di as error "-moremata- is required; type {stata ssc install moremata}"
    error 499
}

program reldist, eclass properties(svyb svyj)
    version 12
    if replay() {
        Replay `0'
        exit
    }
    gettoken subcmd 00 : 0, parse(", ")
    if `"`subcmd'"'==substr("graph",1,max(2,strlen(`"`subcmd'"'))) {
        GRAPH `00'
        exit
    }
    if `"`subcmd'"'==substr("olabel",1,max(4,strlen(`"`subcmd'"'))) {
        OLABEL `00'
        exit
    }
    if `"`subcmd'"'=="predict" {
        PREDICT `00'
        exit
    }
    local version : di "version " string(_caller()) ":"
    Parse_subcmd `subcmd'
    Get_diopts `SUBCMD' `00' // returns 00, diopts, dioptshaslevel
    tempname BW
    Check_vce `BW' `SUBCMD' `00'
    if "`vcetype'"=="svyr" {
        if `"`svylevel'"'!="" {
            if `dioptshaslevel'==0 {
                local diopts `svylevel' `diopts'
            }
        }
        `version' reldist_SVYR `"`svyopts'"' `subcmd' `00'
    }
    else if "`vcetype'"=="svy" {
        `version' svy `svytype', noheader notable `svyopts': reldist `subcmd' `00'
    }
    else if "`vcetype'"!="" { // bootstrap/jackknife
        `version' _vce_parserun reldist, noeqlist wtypes(pw iw) ///
            bootopts(noheader notable force) ///
            jkopts(noheader notable force) : `subcmd' `00'
    }
    else {
        `SUBCMD' `00' 
    }
    eret local cmdline `"reldist `0'"'
    Replay, `diopts'
    if `"`e(generate)'"'!="" {
        di as txt `"(relative ranks stored in variable {bf:`e(generate)'})"'
    }
end

program Parse_subcmd
    if `"`0'"'=="pdf" {
        c_local SUBCMD PDF
        exit
    }
    if `"`0'"'==substr("histogram",1,max(4,strlen(`"`0'"'))) {
        c_local SUBCMD HIST
        exit
    }
    if `"`0'"'=="cdf" {
        c_local SUBCMD CDF
        exit
    }
    if `"`0'"'==substr("divergence",1,max(3,strlen(`"`0'"'))) {
        c_local SUBCMD DIV
        exit
    }
    if `"`0'"'=="mrp" {
        c_local SUBCMD MRP
        exit
    }
    if `"`0'"'==substr("summarize",1,max(2,strlen(`"`0'"'))) {
        c_local SUBCMD SUM
        exit
    }
    di as err `"invalid subcommand: `0'"'
    exit 198
end

program Get_diopts
    gettoken SUBCMD 0 : 0
    _parse comma lhs 0 : 0
    syntax [, Level(passthru) CITRANSform noHEADer NOTABle TABle ///
        COMpare /// hide from _get_diopts
        GRaph GRaph2(passthru) * ]
    if inlist("`SUBCMD'","MRP","SUM","DIV") {
        if `"`graph'`graph2'"'!="" {
            di as err "option {bf:graph()} not allowed"
            exit 198
        }
    }
    _get_diopts diopts options, `options'
    local options `level' `compare' `options'
    if `"`options'"'!="" local lhs `lhs', `options'
    c_local diopts `diopts' `level' `citransform' `header' `notable' `table' `graph' `graph2'
    c_local dioptshaslevel = `"`level'"'!=""
    c_local 00 `lhs'
end

program Check_vce
    gettoken BW  0 : 0
    gettoken SUBCMD 0 : 0
    _parse comma lhs 0 : 0
    syntax [, vce(str) NOSE * ]
    if `"`vce'"'=="" exit
    Parse_vceopt `vce' // returns vcetype, vcevars, svytype, svyopts, svydopts, level
    if "`vcetype'"=="" exit  // no prefix command
    // svy linearized
    if "`vcetype'"=="svyr" {
        c_local 00 `lhs', nose `svydopts' `options'
        c_local svyopts `svyopts'
        c_local svylevel `svylevel'
        c_local vcetype svyr
        exit
    }
    // check for options that are not allowed with replication techniques
    local 0 `", `options'"'
    syntax [, Generate(passthru) BWidth(passthru) BWADJust(passthru) ///
        n(passthru) at(passthru) atx ATX2(passthru) DISCRete CATegorical ///
        BALance(passthru) COMpare COMpare2(passthru) * ]
    if "`compare'"!="" {
        // so that _vce_parserun will not absorb the option
        if `"`compare2'"'=="" local compare2 compare(adjust())
        local compare
    }
    local options `n' `at' `atx' `atx2' `discrete' `categorical'/*
        */ `balance' `compare2' `options'
    if `"`generate'"'!="" {
        local vcetype `vcetype' `svytype'
        di as err `"option {bf:generate()} not allowed with {bf:vce(`vcetype')}"'
        exit 198
    }
    if `"`balance'"'!="" {
        capt Check_vce_balance_gen, `balance'
        if _rc==1 exit _rc
        if _rc {
            di as err `"option {bf:generate()} not allowed in {bf:balance()} with {bf:vce(`vcetype')}"'
            exit 498
        }
    }
    if `"`compare2'"'!="" {
        capt Check_vce_compare_gen, `compare2'
        if _rc==1 exit _rc
        if _rc {
            di as err `"option {bf:generate()} not allowed in {bf:balance()} with {bf:vce(`vcetype')}"'
            exit 498
        }
    }
    if "`discrete'"!="" {
        if "`categorical'`n'`at'"=="" {
            local vcetype `vcetype' `svytype'
            di as err "option {bf:discrete} only allowed with "/*
                */`"{bf:vce(`vcecmd')} if {bf:n()}, {bf:at()}, or "'/*
                */"{bf:categorical} is specified"
            exit 198
        }
    }
    else if `"`atx'`atx2'"'!="" {
        if "`categorical'"=="" {
            local vcetype `vcetype' `svytype'
            di as err `"option {bf:atx()} not allowed with {bf:vce(`vcecmd')}"'
            exit 198
        }
    }
    // obtain bandwidth
    if "`discrete'`categorical'"=="" {
        Obtain_bwidth `BW' `SUBCMD' `lhs', `bwidth' `bwadjust' nose `options' ///
            _vcevars(`vcevars') _vcetype(`vcetype') _svysubpop(`svysubpop')
    }
    // svy
    if "`vcetype'"=="svy" {
        c_local 00 `lhs', `bwidth' `bwadjust' nose `options'
        c_local svyopts `"`svyopts'"'
        c_local svytype `"`svytype'"'
        c_local vcetype `vcetype'
        exit
    }
    // bootstrap and jackknife
    c_local 00 `lhs', vce(`vce') `bwidth' `bwadjust' nose `options'
    c_local vcetype `vcetype'
end

program Check_vce_balance_gen
    syntax [, BALance(str) ]
    _parse comma lhs 0 : balance
    syntax [, GENerate(str) * ]
    if `"`generate'"'!="" exit 198
end

program Check_vce_compare_gen
    syntax [, COMpare2(str) ]
    local 0 `", `compare2'"'
    syntax [, BALance(passthru) * ]
    if `"`balance'"'=="" exit
    Check_vce_balance_gen, `balance'
end

program Parse_vceopt
    _parse comma vcetype 0 : 0
    gettoken vcetype vcearg : vcetype
    mata: st_local("vcearg", strtrim(st_local("vcearg")))
    if `"`vcetype'"'=="svy" {
        qui svyset
        if `"`r(settings)'"'==", clear" {
             di as err "data not set up for svy, use {helpb svyset}"
             exit 119
        }
        if `"`vcearg'"'=="" local vcearg `"`r(vce)'"'
        if `"`vcearg'"'== substr("linearized",1,max(3,strlen(`"`vcearg'"'))) {
            Get_densityopts dopts options `0'
            Check_densityopts, `dopts'
            if `"`dopts'"'!="" {
                c_local svydopts svydensityopts(`dopts')
            }
            c_local svyopts `options'
            c_local vcetype svyr
            syntax [, Level(passthru) * ] // must pass level through to diopts
            c_local svylevel `level'
            exit
        }
        syntax [, SUBpop(passthru) * ]
        c_local svyopts `subpop' `options'
        c_local svysubpop `subpop'
        c_local svytype `"`vcearg'"'
        c_local vcetype svy
        exit
    }
    if `"`vcetype'"'== substr("bootstrap",1,max(4,strlen(`"`vcetype'"'))) {
        if `"`vcearg'"'!="" {
            di as err `"'`vcearg'' not allowed"'
            di as err "error in option {bf:vce()}"
            exit 198
        }
        syntax [, STRata(varlist) CLuster(varlist) group(varname) JACKknifeopts(str) * ]
        Parse_vceopt_jack, `jackknifeopts'  // returns vcevars
        c_local vcevars `vcevars' `strata' `cluster' `group'
        c_local vcetype bootstrap
        exit
    }
    if `"`vcetype'"'== substr("jackknife",1,max(4,strlen(`"`vcetype'"'))) {
        if `"`vcearg'"'!="" {
            di as err `"'`vcearg'' not allowed"'
            di as err "error in option {bf:vce()}"
            exit 198
        }
        Parse_vceopt_jack `0'  // returns vcevars
        c_local vcevars `vcevars'
        c_local vcetype jackknife
        exit
    }
end

program Parse_vceopt_jack
    syntax [, CLuster(varlist) * ]
    c_local vcevars `cluster'
end

program Obtain_bwidth   // returns bwidth, bwadjust
    gettoken BW     0 : 0
    gettoken SUBCMD 0 : 0
    if !inlist("`SUBCMD'","PDF","DIV") exit
    syntax [anything] [if] [in] [fw iw pw] [, ///
        _vcevars(str) _vcetype(str) _svysubpop(str) ///
        bwidth(str) * ]
    if "`SUBCMD'"=="DIV" {
        Obtain_bwidth_DIV_pdf, `options' // returns pdf
        if "`pdf'"=="" exit
    }
    capt confirm number `bwidth'
    if _rc==0 exit
    capt confirm scalar `bwidth'
    if _rc==0 exit
    capt confirm matrix `bwidth'
    if _rc==0 exit
    local options bwidth(`bwidth') `options'
    di as txt "(running {bf:reldist} to obtain bandwidth)"
    marksample touse
    if `"`_vcetype'"'=="svy" {
        if `"`weight'"'!="" {
            di as err "weights not allowed with {bf:vce(svy)}"
            exit 198
        }
        tempvar svysub wvar
        _svy_setup `touse' `svysub' `wvar', svy `_svysubpop'
        qui replace `touse' = 0 if `svysub'==0
        local wgt [pw = `wvar']
    }
    else {
        loca wgt [`weight'`exp']
        markout `touse' `_vcevars', strok
    }
    qui `SUBCMD' `anything' if `touse' `wgt', `options'
    if "`SUBCMD'"=="DIV" {
        matrix `BW' = e(bwidth)
        c_local bwidth bwidth(`BW')
    }
    else {
        scalar `BW' = e(bwidth)
        c_local bwidth bwidth(`BW')
    }
    c_local bwadjust
end

program Obtain_bwidth_DIV_pdf
    syntax [, pdf * ]
    c_local pdf `pdf'
end

program reldist_SVYR, eclass
    local version : di "version " string(_caller()) ":"
    gettoken svyopts cmdline : 0
    `version' svy linearized, noheader notable `svyopts': reldist_svyr `cmdline'
    tempname b
    mat `b' = e(b)
    mata: rd_svylbl_b_undo() // returns k_eq
    ereturn repost b=`b', rename
    eret scalar k_eq = `k_eq'
    eret local cmd "reldist"
    eret local cmdname ""
    eret local command
    eret local svydensityopts ""
end

program PREDICT 
    // ignores types; always stores variables as double
    // by default, IFs will be set only within e(sample); when -if/in- is 
    // specified, IFs will be set within the scope of -if/in- (i.e. IFs will be
    // set to zero for obs outside e(sample) but within -if/in-)
    tempname b
    if `"`e(cmd)'"'=="reldist_svyr" {
        mat `b' = e(b)
        local svydensityopts `"`e(svydensityopts)'"'
    }
    else if `"`e(cmd)'"'=="reldist" {
        mat `b' = e(b)
        mata: rd_svylbl_b()
        local bmat b(`b')
    }
    else {
        di as err "last reldist results not found"
        exit 301
    }
    local n = colsof(`b')
    local k = `n'
    syntax [anything] [if] [in], [ SCores * ]
    local options `svydensityopts' `options'
    Check_densityopts, `options'
    _score_spec `anything', scores `bmat'
    local tlist `s(typlist)'
    local vlist `s(varlist)'
    local over `"`e(over)'"'
    if `"`over'"'!="" {
        local levels `"`e(over_namelist)'"'
        local nover: list sizeof levels
        local k = `k'/`nover'
    }
    forv i=1/`k' {
        tempvar IF
        local IFs `IFs' `IF'
    }
    PREDICT_compute_IFs "`IFs'" `"`options'"'
    local vlist1 `vlist'
    if `"`over'"'!="" {
        foreach l of local levels {
            foreach IF of local IFs {
                gettoken v vlist1 : vlist1
                if "`v'"=="" continue
                qui gen double `v' = cond(`over'==`l', `IF', 0) if `IF'<.
            }
        }
    }
    else {
        foreach IF of local IFs {
            gettoken v vlist1 : vlist1
            if "`v'"=="" continue
            rename `IF' `v'
        }
    }
    if `"`if'`in'"'=="" exit
    tempvar tmp
    forv i = 1/`n' {
        gettoken v vlist : vlist
        if "`v'"=="" continue
        qui gen double `tmp' = cond(`v'<., `v', 0) `if' `in'
        drop `v'
        rename `tmp' `v'
    }
end

program PREDICT_compute_IFs
    args IFs opts
    
    // check subcommand
    local subcmd `e(subcmd)'
    Parse_subcmd `subcmd'
    
    // determine estimation sample
    tempvar touse
    qui gen byte `touse' = e(sample)==1
    if `"`e(subpop)'"'!="" {
        local 0 `"`e(subpop)'"'
        syntax [varname(default=none)] [if]
        if `"`varlist'"'!="" {
            qui replace `touse' = `touse' & `varlist'!=0 & `varlist'<.
        }
        if `"`if'"'!="" {
            di "`touse'"
            rename `touse' `tmp'
            qui gen byte `touse' = 0
            qui replace `touse' = `tmp' `if'
            drop `tmp'
        }
    }
    qui count if `touse'
    if r(N)==0 {
        di as err "could not identify estimation sample; computation of influence functions failed"
        exit 498
    }
    // compile command line
    local cmdline `e(depvar)' `e(refvar)' [`e(wtype)'`e(wexp)'] /*  what about complex weights in svy?
         */ if `touse', _ifgenerate(`IFs') vce(analytic, `opts')
    // - by()
    if `"`e(by)'"'!="" { // syntax 1
        local cmdline `cmdline' by(`e(by)') `e(swap)' `e(pooled)'
    }
    // - balance()
    if `"`e(balance)'"'!="" {
        local cmdline `cmdline' balance(`e(balmethod)':`e(balance)', /*
            */ `e(balref)' `e(balcontrast)' `e(balopts)')
    }
    // - adjust()
    if `"`e(adjust)'`e(refadjust)'"'!="" & "`SUBCMD'"!="MRP" {
        local cmdline `cmdline' adjust(`e(adjust)':`e(refadjust)',/*
            */ `e(adjmean)' `e(adjsd)' `e(adjlog)' `e(adjmult)')
    }
    // - nobreak, nomid, descending, nostable
    local cmdline `cmdline' `e(nobreak)' `e(nomid)' `e(descending)' `e(nostable)'
    // - PDF or CDF
    if "`SUBCMD'"=="PDF" | "`SUBCMD'"=="CDF" {
        local cmdline `cmdline' `e(categorical)' `e(discrete)'
        if `"`e(atxopt)'"'!="" {        // atx()
            local cmdline `cmdline' atx(`e(atxopt)')
        }
        else if `"`e(atx)'"'!="" {      // atx
            local cmdline `cmdline' atx
        }
        else if `"`e(atopt)'"'!="" {    // at()
            local cmdline `cmdline' at(`e(atopt)')
        }
        else if `"`e(discrete)'"'!="" { // n() with discrete/cat
            if `"`e(atx)'`e(atopt)'"'=="" { 
                local cmdline `cmdline' n(`e(n)')
            }
        }
        else {                          // n() (or none) without discrete/cat
            local cmdline `cmdline' n(`e(n)')
        }
        // - CDF
        if "`SUBCMD'"=="CDF" {
            local cmdline `cmdline' `e(alt)'
        }
        // - PDF (without discrete/cat)
        else if  `"`e(discrete)'"'=="" {
            tempname BWIDTH
            scalar `BWIDTH' = e(bwidth)
            local cmdline `cmdline' bwidth(`BWIDTH') boundary(`e(boundary)')/*
                */ adaptive(`e(adaptive)') kernel(`e(kernel)')/*
                */ napprox(`e(napprox)') `e(exact)'
            if e(n_hist)<. {
                local cmdline `cmdline' histogram(`e(n_hist)') `e(alt)'
            }
        }
    }
    // - HIST
    else if "`SUBCMD'"=="HIST" {
        if `"`e(categorical)'`e(discrete)'"'!="" {
            local cmdline `cmdline' `e(categorical)' `e(discrete)'
        }
        else {
            local cmdline `cmdline' n(`e(n_hist)') `e(alt)'
        }
    }
    // - DIV
    else if "`SUBCMD'"=="DIV" {
        if `"`e(over)'"'!="" {
            local cmdline `cmdline' over(`e(over)')
        }
        if `"`e(categorical)'`e(discrete)'"'!="" {
            local cmdline `cmdline' `e(categorical)' `e(discrete)'
        }
        local cmdline `cmdline' `e(statistics)'
        else if `"`e(pdf)'"'!="" {
            tempname BWIDTH
            matrix `BWIDTH' = e(bwidth)
            local cmdline `cmdline' pdf(bwidth(`BWIDTH') boundary(`e(boundary)')/*
                */ adaptive(`e(adaptive)') kernel(`e(kernel)')/*
                */ napprox(`e(napprox)') `e(exact)')
        }
        else {
            local cmdline `cmdline' n(`e(n_hist)') `e(alt)'
        }
        if `"`e(compare)'"'!="" {
            local compare
            // - balance()
            if `"`e(c_balance)'"'!="" {
                local compare `compare' balance(`e(c_balmethod)':`e(c_balance)',/*
                     */ `e(c_balref)' `e(c_balcontrast)' `e(c_balopts)')
            }
            // - adjust()
            if `"`e(c_adjust)'`e(c_refadjust)'"'!="" {
                local compare `compare' adjust(`e(c_adjust)':`e(c_refadjust)',/*
                    */ `e(c_adjmean)' `e(c_adjsd)' `e(c_adjlog)' `e(c_adjmult)')
            }
            if `"`compare'"'=="" local compare compare
            else {
                local compare compare(`compare')
            }
            local cmdline `cmdline' `compare'
        }
    }
    // - MRP
    else if "`SUBCMD'"=="MRP" {
        if `"`e(over)'"'!="" {
            local cmdline `cmdline' over(`e(over)')
        }
        local mrpadj `e(adjust)' `e(refadjust)'
        if `:list sizeof mrpadj'>1 {
            if `"`e(adjsd)'"'!="" local mrpadj scale(sd)
            else                  local mrpadj scale
        }
        else local mrpadj ""
        if `"`e(refadjust)'"'!="" local mrpadj `mrpadj' reference
        local mrpadj `mrpadj' `e(adjlog)' `e(adjmult)'
        local cmdline `cmdline' `mrpadj'
    }
    // - SUM
    else if "`SUBCMD'"=="SUM" {
        if `"`e(over)'"'!="" {
            local cmdline `cmdline' over(`e(over)')
        }
        local cmdline `cmdline' statistics(`e(statistics)')
    }
    // compute IFs
    tempname ecurrent b
    mat `b' = e(b)
    _estimates hold `ecurrent', restore
    qui `SUBCMD' `cmdline'
    mat `b' = mreldif(`b',e(b)) // returns missing if non-conformable
    capt assert (`b'[1,1]<1e-15)
    if _rc==1 exit _rc
    if _rc {
        di as err "inconsistent re-estimation results; computation of influence functions failed"
        exit 498
    }
end

program Replay
    if `"`e(cmd)'"'!="reldist" {
        di as err "last reldist results not found"
        exit 301
    }
    syntax [, GRaph GRaph2(str asis) * ]
    if `"`graph2'"'!="" local graph graph
    _Replay, `graph' `options'
    if "`graph'"!="" {
        tempname rcurrent
        _return hold `rcurrent'
        GRAPH, `graph2'
        _return restore `rcurrent'
    }
end

program _Replay, rclass
    local subcmd `"`e(subcmd)'"'
    if !inlist(`"`subcmd'"', "pdf", "histogram", "cdf", "divergence", "mrp", "summarize") {
        di as err "last reldist results not found"
        exit 301
    }
    syntax [, Level(passthru) CITRANSform noHeader NOTABle TABle GRaph * ]
    if `"`level'"'=="" {
        if `"`e(level)'"'!="" {
            local level level(`e(level)')
        }
    }
    local options `level' `options'
    if "`header'"=="" {
        if `"`e(by)'"'!="" {
            local by `"`e(by)'"'
            if `"`e(by1lab)'"'!="" local by1 `"`e(by1lab)'"'
            else                   local by1 `"`e(by1)'"'
            if `"`e(by0lab)'"'!="" local by0 `"`e(by0lab)'"'
            else                   local by0 `"`e(by0)'"'
            mata: Strlen("lby", st_local("by"))
            mata: Strlen("lbyl", st_local("by1"), st_local("by0"))
            local ll   = `lby' + `lbyl' + 3
            if `ll'>40 {
                if `lby'<=18 {
                    mata: Abbrev("by1", st_local("by1"), 37 - `lby')
                    mata: Abbrev("by0", st_local("by0"), 37 - `lby')
                }
                else if `lbyl'<=19 {
                    mata: Abbrev("by", st_local("by"), 37 - `lbyl')
                    mata: Strlen("lby", st_local("by"))
                }
                else {
                    local by   = abbrev(`"`by'"', 18)
                    local lby  = 18
                    local by1  = abbrev(`"`by1'"', 19)
                    local by0  = abbrev(`"`by0'"', 19)
                }
            }
            mata: Strlen("ll", st_local("by0"))
            local ll = `lby' + `ll' + 3
            local line1 `"`by' = {res:`by1'}"'
            local line2 `"`by' = {res:`by0'}"'
            if `"`e(balcontrast)'"'!="" {
                if `"`e(balref)'"'!="" local line1 "(balanced F0)"
                else                   local line2 "(balanced F1)"
            }
        }
        else {
            local line1 = abbrev(`"`e(depvar)'"', 32)
            local line1 `"{res:`line1'}"'
            local line2 = abbrev(`"`e(refvar)'"', 32)
            mata: Strlen("ll", st_local("line2"))
            local line2 `"{res:`line2'}"'
        }
        local line1 `"  F1: `line1'"'
        local line2 `"  F0: `line2'"'
        local j 2
        if `"`e(pooled)'"'!="" {
            if `ll'<=31 {
                local line2 `"`line2' (pooled)"'
            }
            else {
                local ++j
                local line`j' "      (pooled)"
            }
        }
        if `"`e(adjust)'`e(refadjust)'"'!="" {
            local ++j
            local line`j' `"  Adjustment"'
            if `"`e(compare)'"'!="" local line`j' `"`line`j'' (main model)"'
            if `"`subcmd'"'=="mrp" {
                local line`j' `"`line`j'': {res:`e(adjust)'`e(refadjust)'}"'
            }
            local adjopts `"`e(adjmean)' `e(adjsd)' `e(adjmult)' `e(adjlog)'"'
            local adjopts: list retok adjopts
            if `"`adjopts'"'!="" {
                local adjopts: subinstr local adjopts "logarithmic" "log"
                local adjopts: subinstr local adjopts "multiplicative" "mult"
                local line`j' `"`line`j'' ({res:`adjopts'})"'
            }
            
            if `"`subcmd'"'!="mrp" {
                local ++j
                if `"`e(adjust)'"'!=""    local line`j' `"{res:`e(adjust)'}"'
                else                      local line`j' `"(none)"'
                local line`j' `"      F1: `line`j''"'
                local ++j
                if `"`e(refadjust)'"'!="" local line`j' `"{res:`e(refadjust)'}"'
                else                      local line`j' `"(none)"'
                local line`j' `"      F0: `line`j''"'
            }
        }
        if `"`e(balance)'"'!="" {
            local ++j
            local line`j' "  Balancing of"
            if `"`e(balref)'"'!="" local line`j' `"`line`j'' F0"'
            else                   local line`j' `"`line`j'' F1"'
            if `"`e(compare)'"'!="" local line`j' `"`line`j'' (main model)"'
            local ++j
            local line`j' `"      method = {res:`e(balmethod)'}"'
            local ++j
            local i 0
            if `"`e(by)'"'!="" local i = `i' + 2
            if "`subcmd'"=="pdf" {
                if `"`e(discrete)'"'==""   local i = `i' + 1
            }
            if `j'>`i' {
                mata: Abbrev("xvars", st_global("e(balance)"), 72, 1)
            }
            else {
                mata: Abbrev("xvars", st_global("e(balance)"), 40, 1)
            }
            local line`j' "      `xvars'"
        }
        if `"`e(compare)'"'!="" {
            if `"`e(c_adjust)'`e(c_refadjust)'"'!="" {
                local ++j
                local line`j' `"  Adjustment (alternate model)"'
                local adjopts `"`e(c_adjmean)' `e(c_adjsd)' `e(c_adjmult)' `e(c_adjlog)'"'
                local adjopts: list retok adjopts
                if `"`adjopts'"'!="" {
                    local adjopts: subinstr local adjopts "logarithmic" "log"
                    local adjopts: subinstr local adjopts "multiplicative" "mult"
                    local line`j' `"`line`j'' ({res:`adjopts'})"'
                }
                local ++j
                if `"`e(c_adjust)'"'!=""    local line`j' `"{res:`e(c_adjust)'}"'
                else                        local line`j' `"(none)"'
                local line`j' `"      F1: `line`j''"'
                local ++j
                if `"`e(c_refadjust)'"'!="" local line`j' `"{res:`e(c_refadjust)'}"'
                else                        local line`j' `"(none)"'
                local line`j' `"      F0: `line`j''"'
            }
            if `"`e(c_balance)'"'!="" {
                local ++j
                local line`j' "  Balancing of"
                if `"`e(c_balref)'"'!="" local line`j' `"`line`j'' F0"'
                else                     local line`j' `"`line`j'' F1"'
                local line`j' `"`line`j'' (alternate model)"'
                local ++j
                local line`j' `"      method = {res:`e(c_balmethod)'}"'
                local ++j
                mata: Abbrev("xvars", st_global("e(c_balance)"), 72, 1)
                local line`j' "      `xvars'"
            }
        }
        local headopt17 head2left(17) head2right(10)
        if      c(stata_version)<17            local headopt17
        else if d(`c(born_date)')<d(13jul2021) local headopt17
        _coef_table_header, nomodeltest `headopt17'
        local i 0
        if `"`e(by)'"'!="" {
            di as txt `"`line`++i''"' /*
                */ as txt _col(49) "Comparison obs" _col(67) "= " as res %10.0gc e(N1)
            di as txt `"`line`++i''"' /*
                */ as txt _col(49) "Reference obs"  _col(67) "= " as res %10.0gc e(N0)
        }
        if "`subcmd'"=="pdf" {
            if `"`e(discrete)'"'=="" {
                di as txt `"`line`++i''"' /*
                    */ as txt _col(49) "Bandwidth" _col(67) "= " as res %10.0g e(bwidth)
            }
        }
        if "`subcmd'"=="divergence" {
            if `"`e(discrete)'"'=="" {
                if `"`e(pdf)'"'!="" {
                    di as txt `"`line`++i''"' /*
                        */ as txt _col(49) "Grid size" _col(67) "= " as res %10.0g e(n)
                    mata: st_local("bwidth", ///
                        mm_isconstant(st_matrix("e(bwidth)")) ? ///
                        "%10.0g el(e(bwidth), 1, 1)" : ///
                        `""{stata matrix list e(bwidth):{bf:{ralign 10:e(bwidth)}}}""')
                    di as txt `"`line`++i''"' /*
                        */ as txt _col(49) "Bandwidth" _col(67) "= " /*
                        */ as res `bwidth'
                }
                else {
                    di as txt `"`line`++i''"' /*
                        */ as txt _col(49) "Histogram bins" _col(67) "= " as res %10.0g e(n_hist)
                }
            }
            local stats `"`e(statistics)'"'
            if `:list sizeof stats'==1 & !(`"`e(compare)'"'=="" & `"`e(over)'"'=="") {
                di as txt `"`line`++i''"' /*
                    */ as txt _col(49) "Statsistic" _col(67) "= " as res %10s "`stats'"
            }
        }
        while (`i'<`j') { // flush remaining lines
            if `"`line`++i''"'=="" continue, break
             di as txt `"`line`i''"'
        }
        if `"`e(over)'"'!="" {
            if "`subcmd'"=="divergence" {
                if `:list sizeof stats'!=1 | `"`e(compare)'"'!="" {
                    _svy_summarize_legend
                }
                else di ""
            }
            else {
                _svy_summarize_legend
            }
        }
        else di ""
    }
    if ("`table'"!="" | "`graph'"=="") & "`notable'"=="" {
        local citype 0
        if c(stata_version)>=15 & "`citransform'"!="" {
            if      "`subcmd'"=="pdf"       local citype 4 // log
            else if "`subcmd'"=="histogram" local citype 4 // log
            else if "`subcmd'"=="cdf"       local citype 1 // logit
            else if "`subcmd'"=="summarize" local citype 1 // logit
            else if "`subcmd'"=="mrp"       local citype 3 // atanh
            if (`citype') {
                capt confirm matrix e(V)
                if _rc==0 {
                    tempname cimat
                    GetCI, cimat(`cimat') citype(`citype') `level' 
                    local options cimat(`cimat') `options'
                }
                else local citype 0
            }
        }
        if "`subcmd'"!="mrp" {
            if c(stata_version)>=15 {
                _coef_table, nopvalue `options'
            }
            else if c(stata_version)>=14 {
                eret di, nopvalue `options'
            }
            else if c(stata_version)>=13 {
                quietly update
                if r(inst_ado)>=d(26jun2014) {
                    eret di, nopvalue `options'
                }
                else {
                    _coef_table, cionly `options'
                }
            }
            else {
                _coef_table, cionly `options'
            }
        }
        else {
            if c(stata_version)>=15 {
                _coef_table, `options'
            }
            else {
                eret di, `options'
            }
        }
        if `citype' {
            local citype: word `citype' of logit probit atanh log
            di as txt "(`citype' transformed confidence intervals)"
        }
        capt confirm matrix e(at)
        if _rc==0 {
            di as txt "(evaluation grid stored in {stata matrix list e(at):{bf:e(at)}})"
        }
        return add
    }
    else if "`notable'"=="" {
        di as txt "({stata reldist:coefficients table} suppressed)"
    }
end

program GetCI
    syntax, cimat(str) [ citype(int 0) Level(cilevel) ]
    mata: rd_GetCI(`level', `citype')
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
    args b ll ul n offset level citrans noci ci nhist nohci
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
        if rowsof(e(`ci'))!=2 | colsof(e(`ci'))!=(`n'-("`offset'"!="")) {
            di as error "matrix e(`ci') not conformable"
            exit 499
        }
        tempname CI
        mat `CI' = e(ci)'
        local hasci hasci
    }
    else {
        if `"`level'"'=="" {
            if `"`e(level)'"'!="" {
                local level level(`e(level)')
            }
        }
        capt confirm matrix e(V)
        if _rc==0 {
            if "`citrans'"==""             local citype 0 // normal
            else if `"`e(subcmd)'"'=="cdf" local citype 1 // logit
            else                           local citype 4 // log
            tempname CI
            GetCI, cimat(`CI') citype(`citype') `level'
            mat `CI' = `CI''
            local hasci hasci
        }
    }
    if "`hasci'"!="" {
        if ("`offset'"!="") {
            if `"`e(subcmd)'"'=="cdf" {
                mat `CI' = (0,0) \ `CI'
            }
            else if `"`e(subcmd)'"'=="pdf" {
                mat `CI' = `CI'[1,1...] \ `CI'
            }
        }
        mata: rd_svmat("`CI'", ("`ll'", "`ul'"), 0)
        qui replace `ll' = `b' if `ll'>=. in 1/`=rowsof(`CI')' // zero variance
        qui replace `ul' = `b' if `ul'>=. in 1/`=rowsof(`CI')' // zero variance
    }
    else {
        c_local hasci
        c_local hashci
        exit
    }
    if "`noci'"==""  c_local hasci hasci
    else             c_local hasci
    if "`nohci'"=="" c_local hashci hashci
    else             c_local hashci
end

program _GRAPH_olab // returns [y]oaxis, [y]olabopts, options
    _parse comma xy 0 : 0
    local XY = strupper("`xy'")
    local xy0 "`xy'"
    if "`xy0'"=="" local xy0 x
    // otitle (only allowed once)
    syntax [, `XY'OTItle(str asis) * ]
    local options0: copy local options
    if `"``xy'otitle'"'!="" {
        _parse comma `xy'otitle 0 : `xy'otitle
        syntax [, * ]
        if `"``xy'otitle'"'=="" local `xy'otitle `""""'
        local otitle `xy0'title(``xy'otitle', axis(2) `options')
    }
    local options: copy local options0
    // olabel(), otick(), oline() (repeated options allowed)
    local olabopts
    local 0 `", `options'"'
    while (1) {
        __GRAPH_olab `xy' `0'
        if (`"`oopts'"'=="") continue, break
        local olabopts `olabopts' `oopts'
        local 0 `", `options'"'
    }
    // returns
    c_local options `options'
    if (`"`olabopts'`otitle'"'=="") exit
    if `"`otitle'"'=="" local otitle `xy0'title("", axis(2))
    c_local `xy'olabopts `xy0'label(none, axis(2)) `olabopts' `otitle'
        // xlabel(none) is specified so that no automatic default labels are
        // generated; this is needed because olabopts may not contain an xlabel()
        // option or only an xlabel(, add) option
    c_local `xy'oaxis `xy0'axis(1 2) // add to each plot so that axes do not move
end

program __GRAPH_olab // returns oopts and options
    _parse comma xy 0 : 0
    local XY = strupper("`xy'")
    syntax [, `XY'OLABel `XY'OLABel2(str asis) `XY'OTICk(str asis) `XY'OLIne(str asis) * ]
    if "`xy'"=="y" {
        local olabel  `"`yolabel'"'
        local olabel2 `"`yolabel2'"'
        local otick   `"`yotick'"'
        local oline   `"`yoline'"'
    }
    if `"`olabel'"'!="" & `"`olabel2'"'!="" {
        local options `xy'olabel(`olabel2') `options'
        local olabel2
    }
    c_local oopts // reset
    c_local options `options'
    // olabel
    if `"`olabel2'"'!="" local olabel olabel
    if `"`olabel'"'!="" {
        local 0 `"`olabel2'"'
        syntax [anything] [, at FORmat(passthru) NOPRUNE prune(passthru) * ]
        local olabel `"`anything'"'
        if `"`olabel'"'=="" local olabel "#6"
        local olabopts `"`options'"'
    }
    // otick
    if `"`otick'"'!="" {
        local 0 `"`otick'"'
        syntax [anything] [, * ]
        local otick `"`anything'"'
        local otickopts `"`options'"'
    }
    // oline
    if `"`oline'"'!="" {
        local 0 `"`oline'"'
        syntax [anything] [, * ]
        local oline `"`anything'"'
        local olineopts `"`options'"'
    }
    // get positions
    if `"`olabel'`otick'`oline'"'=="" exit  // nothing to do
    if `"`e(atx)'"'=="" {
        capt confirm matrix e(ogrid)
        if _rc==1 exit _rc
        if _rc {
            di as txt "(matrix {bf:e(ogrid)} does not exist; cannot compute outcome positions)"
            exit
        }
    }
    OLABEL `olabel', `at' tick(`otick') line(`oline') `format' `noprune' `prune' `xy'
    local olabel `"`r(label)'"'
    local otick  `"`r(tick)'"'
    local oline  `"`r(line)'"'
    // returns
    if "`xy'"=="" local xy x
    if `"`olabel'"'!="" {
        local oopts `xy'label(`olabel', axis(2) `olabopts')
    }
    if `"`otick'"'!="" {
        local oopts `oopts' `xy'tick(`otick', axis(2) `otickopts')
    }
    if `"`oline'"'!="" {
        local oopts `oopts' `xy'line(`oline', axis(2) `olineopts') 
    }
    c_local oopts `oopts'
end

program _GRAPH_xyti // return xti, yti
    if `"`e(by)'"'=="" {
        local xti `"`e(refvar)'"'
        if `"`e(pooled)'"'!="" {
            local xti `"`xti' (pooled)"'
        }
        c_local xti `"`xti'"'
        c_local yti `"`e(depvar)'"'
    }
    else {
        if `"`e(by0lab)'"'!="" {
            local xti `"`e(by0lab)'"'
        }
        else {
            local xti `"`e(by)' = `e(by0)'"'
        }
        if `"`e(pooled)'"'!="" {
            local xti `"`xti' (pooled)"'
        }
        if `"`e(by1lab)'"'!="" {
            local yti `"`e(by1lab)'"'
        }
        else {
            local yti `"`e(by)' = `e(by1)'"'
        }
        if `"`e(balcontrast)'"'!="" {
            if `"`e(balref)'"'!="" local yti `"`xti' (balanced)"'
            else                   local xti `"`yti' (balanced)"'
        }
        c_local xti `"`xti'"'
        c_local yti `"`yti'"'
    }
end

program GRAPH_pdf
    // syntax
    syntax [, Level(passthru) CITRANSform NOCI ci(name) CIOPTs(str) ///
        NOREFline REFline(str) NOHISTogram HISTopts(str) ///
        addplot(str asis) * ]
    _GRAPH_pdf_histopts, `histopts'
     
    // obtain original scale labels
    _GRAPH_olab, `options'
    
    // separate twopts from other graph options
    _GRAPH_get_gropts `options'
    local twopts `olabopts' `twopts'
    
    // get data
    local npdf = e(n)
    tempname B AT
    mat `B' = e(b)'
    mat `AT' = e(at)'
    mat `AT' = `AT'[1...,1]
    if `"`e(discrete)'"'!="" {
        if `AT'[1,1]!=0 {
            mat `B' = `B'[1,1] \ `B'
            mat `AT' = 0 \ `AT'
            local ++npdf
            local offset offset
        }
    }
    local nhist = e(n_hist)
    if `nhist'>=. {
        local nhist
        local nohistogram nohistogram
    }
    
    // check number of obs and expand data if necessary
    local n = rowsof(`B')
    if `n'>_N {
        preserve
        qui set obs `n'
    }
    
    // store results to data
    tempvar pdf at
    mata: rd_svmat("`B'", "`pdf'", 0)
    mata: rd_svmat("`AT'", "`at'", 0)
    
    // obtain CI if available
    tempvar ll ul
    _GRAPH_get_CI `pdf' `ll' `ul' `n' "`offset'" `"`level'"' "`citransform'" ///
        "`noci'" "`ci'" "`nhist'" "`nohci'"
    
    // compile graph
    if `"`e(discrete)'"'!="" local connect connect(stepstair)
    else                     local connect
    local plots
    // - refline
    if "`norefline'"=="" {
        local yline yline(1, `refline')
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
            */ pstyle(ci) `connect' `ciopts')
    }
    // - pdf
    local plots `plots' /*
        */ (line `pdf' `at' in 1/`npdf', `oaxis'/*
        */ pstyle(p1) `connect' `options')
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
    syntax [, Level(passthru) CITRANSform NOCI ci(name) CIOPTs(str) ///
        NOREFline REFline(str) ///
        addplot(str asis) * ]
     
    // obtain original scale labels
    _GRAPH_olab, `options'
    
    // separate twopts from other graph options
    _GRAPH_get_gropts `options'
    local twopts `olabopts' `twopts'
    
    // get data
    local n = e(n_hist)
    tempname B AT
    mat `B' = e(b)'
    mat `AT' = e(at)'
    mat `AT' = `AT'[1...,1]
    if `"`e(discrete)'"'!="" {
        mat `B' =  `B' \ `B'[`n',1]
        mat `AT' = 0 \ `AT'
        local ++n
        local offset offset
    }
    
    // check number of obs and expand data if necessary
    if `n'>_N {
        preserve
        qui set obs `n'
    }
    
    // store results to data
    tempvar pdf at
    mata: rd_svmat("`B'", "`pdf'", 0)
    mata: rd_svmat("`AT'", "`at'", 0)
    
    // obtain CI if available
    tempvar ll ul
    _GRAPH_get_CI `pdf' `ll' `ul' `n' "`offset'" `"`level'"' "`citransform'" ///
        "`noci'" "`ci'"
    if "`hasci'"!="" & `"`e(discrete)'"'!="" {
        tempvar atci
        qui gen double `atci' = (`at' + `at'[_n+1])/2 in 1/`n'
    }
    else local atci `at'
    
    // compile graph
    local plots
    // - refline
    if "`norefline'"=="" {
        local yline yline(1, `refline')
    }
    // - histogram bars
    if `"`e(discrete)'"'!="" {
        local plots `plots' /*
            */ (bar `pdf' `at' in 1/`n', `oaxis'/*
            */  bartype(spanning) pstyle(histogram) `options')
    }
    else {
        local hwidth = e(hwidth) 
        local plots `plots' /*
            */ (bar `pdf' `at' in 1/`n', `oaxis'/*
            */ barwidth(`hwidth') pstyle(histogram) `options')
    }
    // - CI
    if "`hasci'"!="" {
        local plots `plots' /*
            */ (rcap `ll' `ul' `atci' in 1/`n', `oaxis'/*
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
    syntax [, Level(passthru) CITRANSform NOCI ci(name) CIOPTs(str) ///
        NOREFline REFline(str) NOORIGin ///
        addplot(str asis) * ]

    // obtain original scale labels
    _GRAPH_olab, `options'
    _GRAPH_olab y, `options'
    local olabopts `olabopts' `yolabopts'
    local oaxis `oaxis' `yoaxis'
    
    // separate twopts from other graph options
    _GRAPH_get_gropts `options'
    local twopts `olabopts' `twopts'
    
    // get coordinates
    tempname B AT
    mat `B' = e(b)'
    mat `AT' = e(at)'
    mat `AT' = `AT'[1...,1]
    if "`noorigin'"=="" {
        if `"`e(origin)'"'!="" {
            mat `B'  = 0 \ `B'
            mat `AT' = 0 \ `AT'
            local offset offset
        }
    }
    
    // check number of obs and expand data if necessary
    local n = rowsof(`B')
    if `n'>_N {
        preserve
        qui set obs `n'
    }
    
    // store results to data
    tempvar cdf at
    mata: rd_svmat("`B'", "`cdf'", 0)
    mata: rd_svmat("`AT'", "`at'", 0)
    
    // obtain CI if available
    tempvar ll ul
    _GRAPH_get_CI `cdf' `ll' `ul' `n' "`offset'" `"`level'"' "`citransform'" ///
        "`noci'" "`ci'"
    
    // compile graph
    local plots
    // - refline
    if "`norefline'"=="" {
        local plots `plots' /*
            */ (scatteri 0 0 1 1, `oaxis'/*
            */ connect(l) ms(i) lstyle(xyline) `refline')
    }
    // - CI
    if "`hasci'"!="" {
        local plots `plots' /*
            */ (rarea `ll' `ul' `at' in 1/`n', `oaxis'/*
            */ pstyle(ci) `ciopts')
    }
    // - cdf
    local plots `plots' /*
        */ (line `cdf' `at' in 1/`n', `oaxis'/*
        */ pstyle(p1) `options')
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
    syntax [, at TICk(numlist sort) LIne(numlist sort) FORmat(str) y ///
        NOPRUNE prune(numlist max=1) ]
    if "`noprune'"!=""    local prune .
    else if "`prune'"=="" local prune 0.1
    local tick_x `"`tick'"'
    local line_x `"`line'"'
    if "`y'"!="" {
        if `"`e(subcmd)'"'!="cdf" {
            di as err "option {bf:y} only allowed after {bf:reldist cdf}"
            exit 499
        }
    }
    if `"`format'"'=="" local format "%6.0g"
    confirm format `format'
    if (substr(`"`lhs'"',1,1)=="#") {
        local label = substr(`"`lhs'"',2,.)
        local 0 `", label(`label')"'
        syntax [, LABel(numlist int >=2 max=1) ]
        local label "#`label'"
    }
    else if "`at'"!="" {
        local 0 `", label(`lhs')"'
        syntax [, LABel(numlist sort >=0 <=1) ]
        local label_x `"`label'"'
    }
    else {
        local 0 `", label(`lhs')"'
        syntax [, LABel(numlist sort) ]
        local label_x `"`label'"'
    }
    if `"`label'`tick'`line'"'=="" exit // nothing to do
    local atx = (`"`e(atx)'"'!="")
    if (`atx'==0) {
        capt confirm matrix e(ogrid)
        if _rc==1 exit _rc
        if _rc {
            di as txt "matrix {bf:e(ogrid)} does not exist; cannot" /*
                */ " compute outcome positions"
            exit 499
        }
        if "`y'"!="" {
            if rowsof(e(ogrid))<2 {
                di as err "matrix {bf:e(ogrid)} has only one row; cannot" /*
                    */ " compute outcome positions for comparison group"
                exit 499
            }
        }
    }
    mata: rd_olab("`y'"!="", `atx', "label", "`format'", `prune', "`at'"!="")
    mata: rd_olab("`y'"!="", `atx', "tick", "", ., 0)
    mata: rd_olab("`y'"!="", `atx', "line", "", ., 0)
    return local label   `"`label'"'
    return local label_x `"`label_x'"'
    return local tick    `"`tick'"'
    return local tick_x  `"`tick_x'"'
    return local line    `"`line'"'
    return local line_x  `"`line_x'"'
end

program Parse_syntax    // preprocess syntax: two-sample vs. paired
                        // returns under syntax 1: 0, depvar, by, swap, pooled
                        // returns under syntax 2: 0, depvar, refvar
    syntax varlist(min=1 max=2 numeric) [if] [in] [fw iw pw] ///
        [, by(varname numeric) swap POOLed * ]
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
        c_local pooled `pooled'
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
    if "`pooled'"!="" {
        di as err "{bf:pooled} not allowed in syntax 2"
        exit 198
    }
    c_local 0 `if' `in' [`weight'`exp'], `options'
    c_local depvar: word 1 of `varlist'
    c_local refvar: word 2 of `varlist'
end

program Parse_adjust // parse the adjust() option
                     // returns adj1, ads0, adjmean, adjsd, adjlog, adjmult
    capt n syntax [anything] [, mean sd LOGarithmic MULTiplicative ]
    if _rc==1 exit _rc
    if _rc {
        di as err "error in option {bf:adjust()}"
        exit 198
    }
    if "`logarithmic'"!="" & "`multiplicative'"!="" {
        di as err "only one of {bf:logarithmic} and {bf:multiplicative} allowed"
        di as err "error in option {bf:adjust()}"
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
                di as err "{bf:scale} not allowed with option {bf:multiplicative}"
                di as err "error in option {bf:adjust()}"
                exit 198
            }
        }
        else if `"`tok'"'==substr("shape",1,max(2,strlen(`"`tok'"'))) {
            local tok shape
        }
        else if `"`tok'"'!="" {
            di as err "'" `"`tok'"' "' not allowed"
            di as err "error in option {bf:adjust()}"
            exit 198
        }
        local adj`k' `adj`k'' `tok'
    }
    if "`adj0'`adj1'"!="" {
        local anything `adj0' `adj1'
        if "`multiplicative'"!="" {
            if `:list sizeof anything'>1 {
                di as err "only one keyword allowed with option {bf:multiplicative}"
                di as err "error in option {bf:adjust()}"
                exit 198
            }
        }
        else {
            if `:list sizeof anything'>2 {
                di as err "at most two keywords allowed"
                di as err "error in option {bf:adjust()}"
                exit 198
            }
        }
        local anything: list dups anything
        if "`anything'"!="" {
            di as err "duplicate keywords not allowed"
            di as err "error in option {bf:adjust()}"
            exit 198
        }
    }
    else { // no adjustments
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

program Parse_at   // parse n(), at(), atx, atx(), categorical, descrete
                   // returns n, ATX0, atx, AT0, at, categorical, discrete
    args n at atx atx2 discrete categorical
    if `"`discrete'`categorical'"'!="" {
        c_local discrete discrete // categorical implies discrete
        if `"`n'`at'`atx2'"'=="" {
            c_local atx atx // discrete/categorical implies atx by default
            exit
        }
    }
    if `"`atx'`atx2'"'!="" {
        if "`n'"!="" {
            di as err "{bf:n()} and {bf:atx()} not both allowed"
            exit 198
        }
        if `"`at'"'!="" {
            di as err "{bf:at()} and {bf:atx()} not both allowed"
            exit 198
        }
        if `"`atx2'"'=="" exit // atx without argument; use observed values
        c_local atx atx
        if `: list sizeof atx2'==1 {
            if `"`atx2'"'==substr("comparison", 1, max(strlen(`"`atx2'"'), 4)) {
                c_local atx "comparison"
                c_local atx2
                exit
            }
            if `"`atx2'"'==substr("reference", 1, max(strlen(`"`atx2'"'), 3)) {
                c_local atx "reference"
                c_local atx2
                exit
            }
            capt confirm matrix `atx2'
            if _rc==1 exit _rc
            if _rc==0 {
                if "`categorical'"!="" {
                    capt mata: rd_check_mat("`atx2'", 1)
                    if _rc==1 exit _rc
                    if _rc {
                        di as err "noninteger or negative values not allowed"/*
                            */ " in {bf:atx()} is {bf:categorical} is specified"
                        exit 125
                    }
                }
                c_local ATX0 `atx2'
                c_local atx2
                exit
            }
            capt confirm name `atx2'
            if _rc==1 exit _rc
            if _rc==0 { // is a valid name
                di as err `"atx() invalid -- matrix '`atx2'' not found"'
                exit 111
            }
        }
        local 0 `", atx(`atx2')"'
        syntax [, atx(numlist sort) ]
        if "`categorical'"!="" {
            capt numlist "`atx'", integer range(>=0)
            if _rc==1 exit _rc
            if _rc {
                di as err "noninteger or negative values not allowed"/*
                    */ " in {bf:atx()} is {bf:categorical} is specified"
                exit 125
            }
        }
        c_local atx2 "`atx'"
        exit
    }
    if `"`at'"'!="" {
        if "`n'"!="" {
            di as err "{bf:n()} and {bf:at()} not both allowed"
            exit 198
        }
        if `: list sizeof at'==1 {
            capt confirm matrix `at'
            if _rc==1 exit _rc
            if _rc==0 {
                capt mata: rd_check_mat("`at'", 0)
                if _rc==1 exit _rc
                if _rc {
                    di as err "values provided in {bf:at()} must be in [0,1]"
                    exit 125
                }
                c_local AT0 `at'
                c_local at
                exit
            }
            capt confirm name `at'
            if _rc==1 exit _rc
            if _rc==0 { // is a valid name
                di as err `"at() invalid -- matrix '`at'' not found"'
                exit 111
            }
        }
        local 0 `", at(`at')"'
        syntax [, at(numlist sort >=0 <=1) ]
        c_local at "`at'"
        exit
    }
    Parse_n "`n'" 101
    c_local n `n'
end

program Parse_cat_notallowed
    args discrete categorical adjust histogram
    if "`categorical'"!="" {
        if `"`adjust'"'!="" {
            di as err "{bf:adjust()} and {bf:categorical} not both allowed"
            exit 198
        }
        if `"`histogram'"'!="" {
            di as err "{bf:histogram()} and {bf:categorical} not both allowed"
            exit 198
        }
    }
    if "`discrete'"!="" {
        if `"`adjust'"'!="" {
            di as err "{bf:adjust()} and {bf:discrete} not both allowed"
            exit 198
        }
        if `"`histogram'"'!="" {
            di as err "{bf:histogram()} and {bf:discrete} not both allowed"
            exit 198
        }
    }
end

program Parse_ogrid 
    args noogrid ogrid atx
    if "`atx'"!="" {
        if "`ogrid'"!="" {
            di as err "{bf:ogrid()} and {bf:atx()} not both allowed"
            exit 198
        }
        exit
    }
    if "`noogrid'"!="" {
        if "`ogrid'"!="" {
            di as err "{bf:ogrid()} and {bf:noogrid} not both allowed"
            exit 198
        }
        exit
    }
    if "`ogrid'"=="" local ogrid 401
    if `ogrid'>=c(max_matsize) {
        di as err "{bf:ogrid()} must be smaller than"/*
            */" c(max_matsize) = {bf:`c(max_matsize)'}"
        exit(499)
    }
    c_local ogrid `ogrid'
end

program Parse_n 
    args n def nm opt
    if "`n'"==""   local n `def'
    if "`nm'" =="" local nm "n"
    if "`opt'"=="" local opt `nm'
    if `n'>=c(max_matsize) {
        di as err "{bf:`opt'()} must be smaller than"/*
            */" c(max_matsize) = {bf:`c(max_matsize)'}"
        exit(499)
    }
    c_local `nm' `n'
end

program Parse_balance
    gettoken by 0 : 0
    gettoken pooled 0 : 0
    if strtrim(`"`0'"')=="" exit
    if "`by'"=="" {
        di as err "option {bf:balance()} not allowed in syntax 2"
        exit 499
    }
    // parse "[method:] ..."
    _parse comma lhs 0 : 0
    capt _on_colon_parse `lhs'
    if _rc==0 {
        local method `"`s(before)'"'
        local lhs `"`s(after)'"'
    }
    if `"`method'"'=="" local method "ipw"
    if !inlist(`"`method'"', "ipw", "eb") {
        di as err "method '" `"`method'"' "' not allowed"
        di as err "error in option {bf:balance()}"
        exit 198
    }
    // parse "varlist [, options]"
    local 0 `"`lhs'`0'"'
    capt n syntax varlist(fv) [, CONTrast NOIsily REFerence ///
        GENerate(name) * ]
    if _rc==1 exit _rc
    if _rc {
        di as err "error in option {bf:balance()}"
        exit 198
    }
    if "`pooled'"!="" {
        if "`reference'"!="" {
            di as err "option {bf:pooled} not allowed with {bf:balance(, reference)}"
            exit 198
        }
    }
    c_local bal_varlist  `"`varlist'"'
    c_local bal_method   `method'
    c_local bal_noisily  `noisily'
    c_local bal_ref      `reference'
    c_local bal_contrast `contrast'
    c_local bal_opts     `"`options'"'
    c_local bal_wvar     `generate'
    if "`method'"=="eb" {
        local 0 `", `options'"'
        syntax [, BTOLerance(numlist max=1 >=0) DIFficult ///
            MAXIter(numlist integer max=1 >=0 <=16000) ///
            PTOLerance(numlist max=1 >=0) ///
            VTOLerance(numlist max=1 >=0) ]
        if "`btolerance'"=="" local btolerance 1e-5
        c_local bal_ebopts `"`btolerance' "`difficult'" "`maxiter'" "`ptolerance'" "`vtolerance'""'
    }
end

program Samplesetup // common function to prepare estimation sample
            // general returns: wvar, exp, wgt, N
            // returns if syntax 1: by1, by0, by1lab, by0lab, N1, N0
            // returns if syntax 2: (none)
            // returns if over: N_over, overlevels, over_labels
    args touse touse1 touse0 wvar depvar by swap refvar weight exp over
    if "`weight'"!="" {
        capt confirm variable `exp'
        if _rc {
            qui gen double `wvar' = `exp' if `touse'
        }
        else {
            unab exp: `exp', min(1) max(1)
            local wvar `exp'
        }
        local wgt "[`weight'=`wvar']"
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
    }
    else {              // syntax 2
        markout `touse' `refvar'
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
        di as err "{bf:logarithmic} adjustment only allowed if outcomes are strictly positive"
        exit 499
    }
end

program Check_categorical // outcome variable(s) must be integer >=0
    args touse depvar refvar categorical
    if "`categorical'"=="" exit
    capt assert (`depvar'>=0 & trunc(`depvar')==`depvar') if `touse'
    if _rc {
        di as err "`depvar': noninteger or negative values not allowed"/*
        */ " if {bf:categorical} is specified"
        exit 452
    }
    if "`refvar'"=="" exit
    capt assert (`refvar'>=0 & trunc(`refvar')==`refvar') if `touse'
    if _rc {
        di as err "`refvar': noninteger or negative values not allowed"/*
        */ " if {bf:categorical} is specified"
        exit 452
    }
end

program PrepareOver // common function to prepare cycling across over groups
    args N_over overlevels by touse touse1 touse0 _N _N1 _N0
    mat `_N' = J(`N_over',1,.)
    mat rown `_N' = `overlevels'
    qui gen byte `touse' = .
    if "`by'"!="" {
        mat `_N1' = `_N'
        mat `_N0' = `_N'
        qui gen byte `touse1' = .
        qui gen byte `touse0' = .
    }
end

program PrepareOverlevel // common function to handle specific over level
    args i o by touse touse1 touse0 TOUSE TOUSE1 TOUSE0 _N _N1 _N0 wgt
    qui replace `touse' = `TOUSE' & `o'
    _nobs `touse' `wgt'
    mat `_N'[`i',1] = r(N)
    if "`by'"!="" {
        qui replace `touse1' = `TOUSE1' & `o'
        qui replace `touse0' = `TOUSE0' & `o'
        _nobs `touse1' `wgt', min(0)
        mat `_N1'[`i',1] = r(N)
        _nobs `touse0' `wgt', min(0)
        mat `_N0'[`i',1] = r(N)
    }
end

program Parse_vce
    _parse comma vce opts : 0
    gettoken vce arg : vce
    if      `"`vce'"'==substr("analytic", 1, strlen(`"`vce'"'))       local vce "analytic"
    else if `"`vce'"'==substr("cluster", 1, max(2,strlen(`"`vce'"'))) local vce "cluster"
    else {
        di as err `"vce(`vce') not allowed"'
        exit 198
    }
    c_local vce `vce'
    if "`vce'"=="cluster" {
        local 0 `"`arg'"'
        capt n syntax varname
        if _rc==1 exit _rc
        if _rc {
            di as err "error in option {bf:vce()}"
            exit 198
        }
        local arg
        c_local vcetype Robust
        c_local clustvar `varlist'
        c_local vceopt vce(cluster `varlist') // for use with -total-
    }
    else {
        c_local vcetype
        c_local clustvar
        c_local vceopt
    }
    capt n Parse_densityopts `arg' `opts'
    if _rc==1 exit _rc
    if _rc {
        di as err "error in option {bf:vce()}"
        exit 198
    }
    c_local vcebwidth `bwidth'
    c_local vcebwtype `bwtype'
    c_local vcebwmethod `bwmethod'
    c_local vcebwdpi `bwdpi'
    c_local vcebwnord `bwnord'
    c_local vcebwadjust `bwadjust'
    c_local vcekernel `"`kernel'"'
    c_local vceadaptive `adaptive'
    c_local vceexact `exact'
    c_local vceboundary `boundary'
    c_local vcenapprox `napprox'
end

program Get_densityopts
    syntax namelist(min=1 max=2) [, *]
    local 0 `", `options'"'
    gettoken dnm onm : namelist
    local dopts ///
        BWidth(passthru) BWADJust(passthru) ///
        Kernel(passthru) ADAPTive(passthru) exact ///
        BOundary(passthru) NApprox(passthru)
    if "`onm'"!="" local dopts `dopts' *
    syntax [, `dopts' ]
    c_local `dnm' `bwidth' `bwadjust' `kernel' `adaptive' `exact' `boundary' `napprox'
    if "`onm'"=="" exit
    c_local `onm' `options'
end

program Check_densityopts
    Parse_densityopts `0'
end

program Parse_densityopts
    syntax [, ///
        BWidth(str) BWADJust(numlist >0 max=1) ///
        Kernel(string) ADAPTive(numlist int >=0 max=1) exact ///
        BOundary(str)  NApprox(numlist int >1 max=1) ]
    Parse_bwopt `bwidth'
    if "`bwdpi'"==""    local bwdpi 2
    if "`bwadjust'"=="" local bwadjust 1
    if `"`kernel'"'=="" local kernel "gaussian"
    if "`adaptive'"=="" local adaptive 0
    Parse_boundary, `boundary'
    if ("`napprox'"=="") local napprox 512
    c_local bwidth `bwidth'
    c_local bwtype `bwtype'
    c_local bwmethod `bwmethod'
    c_local bwdpi `bwdpi'
    c_local bwnord `bwnord'
    c_local bwadjust `bwadjust'
    c_local kernel `"`kernel'"'
    c_local adaptive `adaptive'
    c_local exact    `exact'
    c_local boundary `boundary'
    c_local napprox `napprox'
end

program Parse_bwopt  // returns: bwidth, bwtype, bwmethod, bwdpi, bwnord
    _parse comma bwidth 0 : 0
    capt n syntax [, NORD ]
    if _rc==1 exit _rc
    if _rc {
        di as err "error in option {bf:bwidth()}"
        exit 198
    }
    capt confirm number `bwidth'
    if _rc==1 exit _rc
    if _rc==0 {
        if `bwidth'<=0 {
            di as error "{bf:bwidth()} must be strictly positive"
            exit 198
        }
    }
    else {
        capt confirm scalar `bwidth'
        if _rc==1 exit _rc
        if _rc==0 {
            if scalar(`bwidth')<=0 {
                di as error "{bf:bwidth()} must be strictly positive"
                exit 198
            }
            if scalar(`bwidth')>=. {
                di as error "{bf:bwidth()} may not be missing"
                exit 198
            }
            local bwtype "scalar"
        }
        else {
            capt confirm matrix `bwidth'
            if _rc==1 exit _rc
            if _rc==0 {
                mata: st_local("invalid", any(st_matrix("`bwidth'"):<=0) ? "1" : "0")
                if `invalid' {
                    di as error "{bf:bwidth()} must be strictly positive"
                    exit 198
                }
                if matmissing(`bwidth') {
                    di as error "{bf:bwidth()} may not be missing"
                    exit 198
                }
                local bwtype "matrix"
            }
            else Parse_bwmethod, `bwidth'
        }
    }
    c_local bwidth `bwidth'
    c_local bwtype `bwtype'
    c_local bwmethod `bwmethod'
    c_local bwdpi `bwdpi'
    c_local bwnord `nord'
end

program Parse_bwmethod  // returns: bwidth, bwmethod, bwdpi
    capt n syntax [, Silverman Normalscale Oversmoothed SJpi Dpi Dpi2(numlist int >=0 max=1) ISJ ]
    if _rc==1 exit _rc
    if _rc {
        di as err "error in option {bf:bwidth()}"
        exit 198
    }
    if "`dpi2'"!="" local dpi dpi
    local bwmethod `silverman' `normalscale' `oversmoothed' `sjpi' `dpi' `isj'
    if "`bwmethod'"=="" local bwmethod "sjpi"
    if `: list sizeof bwmethod'>1 {
        di as err "too many methods specified"
        di as err "error in option {bf:bwidth()}"
        exit 198
    }
    c_local bwidth
    c_local bwmethod `bwmethod'
    c_local bwdpi `dpi2'
end

program Parse_boundary // returns: boundary
    capt n syntax [, RENorm REFlect lc ]
    if _rc==1 exit _rc
    if _rc {
        di as err "error in option {bf:boundary()}"
        exit 198
    }
    local boundary `renorm' `reflect' `lc'
    if "`boundary'"=="" local boundary "renorm"
    if `: list sizeof boundary'>1 {
        di as err "too many methods specified"
        di as err "error in option {bf:boundary()}"
        exit 198
    }
    c_local boundary `boundary'
end

program ComputeVCE
    args b V touse IFs weight exp vceopt over
    if "`weight'"!="" {
        if "`weight'"=="iweight" local wgt [pweight`exp']
        else                     local wgt [`weight'`exp']
    }
    if `"`over'"'!="" local overopt over(`over')
    qui total `IFs' `wgt' if `touse', `overopt' `vceopt'
    mat `V' = e(V)
    if `"`over'"'!="" {
        mata: rd_flip_V()
    }
    local coln: colfullnames `b'
    mat coleq `V' = ""
    mat roweq `V' = ""
    mat coln `V' = `coln'
    mat rown `V' = `coln'
    c_local rank = e(rank)
    c_local df_r = e(df_r)
    if e(vce)=="cluster" {
        c_local N_clust = e(N_clust)
    }
end

program Store_IFs, eclass
    args IFs ifgen
    if "`IFs'"==""   exit
    if "`ifgen'"=="" exit
    foreach IF of local IFs {
        gettoken ifvar ifgen : ifgen
        if "`ifvar'"=="" continue
        rename `IF' `ifvar'
    }
end

program PDF, eclass
    // syntax
    Parse_syntax `0'
    syntax [if] [in] [fw iw pw/], [ ///
        NOBReak NOMID DESCending NOSTAble ADJust(str) BALance(str) ///
        n(numlist int >1 max=1) at(str) atx ATX2(str) DISCRete CATegorical ///
        NOOGRID ogrid(numlist int >0 max=1) ///
        HISTogram HISTogram2(numlist int >0 max=1) alt ///
        vce(str) NOSE Level(cilevel) _ifgenerate(namelist) Replace * ]
    Get_densityopts dopts, `options'
    Parse_densityopts, `dopts'
    if "`bwtype'"=="matrix" {
        tempname BWIDTH
        scalar `BWIDTH' = `bwidth'[1,1]
        local bwidth `BWIDTH'
        local bwtype "scalar"
    }
    Parse_vce `vce'
    Parse_at "`n'" `"`at'"' "`atx'" `"`atx2'"' "`discrete'" "`categorical'"
    Parse_cat_notallowed "`discrete'" "`categorical'" `"`adjust'"' `"`histogram'`histogram2'"'
    Parse_adjust `adjust'
    Parse_balance "`by'" "`pooled'" `balance'
    Parse_ogrid "`noogrid'" "`ogrid'" "`atx'"
    if "`histogram'`histogram2'"!="" Parse_n "`histogram2'" 10 "nhist" "histogram"
    
    // mark sample
    marksample touse
    markout `touse' `depvar' `bal_varlist'
    if "`clustvar'"!="" {
        markout `touse' `clustvar', strok
    }
    tempvar touse1 touse0 wvar
    Samplesetup `touse' `touse1' `touse0' `wvar' `depvar' ///
        "`by'" "`swap'" "`refvar'" "`weight'" `"`exp'"' ""
    Check_adjlog `touse' `depvar' "`refvar'" "`by'" "`adjlog'"
    Check_categorical `touse' `depvar' "`refvar'" "`categorical'"
    
    // compute relative PDF
    mata: rd_generate_init("bal_wvar", 1)
    tempname b AT OGRID BW
    mata: rd_PDF(strtoreal("`n'"))
    
    // VCE
    if "`IFs'"!="" & "`_ifgenerate'"=="" {
        tempname V
        ComputeVCE `b' `V' `touse' "`IFs'" "`weight'" `"`exp'"' `"`vceopt'"'
    }
    
    // returns
    eret post `b' `V' [`weight'`exp'], obs(`N') esample(`touse')
    mata: rd_Post_common_e()
    eret local  subcmd      "pdf"
    eret local  title       "Relative density"
    eret matrix at          = `AT'
    if "`atx'"!="" {
        if "`atx'"!="atx" {
            eret local atx "atx"
            eret local atxopt `"`atx'"' // comparison or reference 
        }
        else {
            eret local atx "atx"
            eret local atxopt `"`atx2'`ATX0'"' // numlist or matname
        }
    }
    else {
        eret local atopt `"`at'`AT0'"' // numlist or matname
    }
    eret local discrete     "`discrete'"
    eret local categorical  "`categorical'"
    eret scalar n           = `n'
    if "`ogrid'"!="" {
        eret matrix ogrid   = `OGRID'
    }
    if "`discrete'"=="" {
        eret scalar bwidth   = `BW'
        if "`bwmethod'"=="dpi" {
            local bwmethod `bwmethod'(`bwdpi')
        }
        if "`bwmethod'"!="" {
            if "`bwnord'"!="" {
                local bwmethod `bwmethod', `bwnord'
            }
        }
        eret local  bwmethod "`bwmethod'"
        eret scalar bwadjust = `bwadjust'
        eret local  kernel   "`kernel'"
        eret scalar adaptive = `adaptive'
        eret local  exact    "`exact'"
        eret local  boundary "`boundary'"
        eret scalar napprox = `napprox'
    }
    if "`nhist'"!="" {
        eret scalar n_hist = `nhist'
        eret scalar hwidth = 1/`nhist'
        eret local alt     "`alt'"
    }
    
    // return balancing weights
    mata: rd_generate_return("bal_wvar")

    // store IFs
    Store_IFs "`IFs'" "`_ifgenerate'"
end

program HIST, eclass
    // syntax
    Parse_syntax `0'
    syntax [if] [in] [fw iw pw/], [ ///
        NOBReak NOMID DESCending NOSTAble ADJust(str) BALance(str) ///
        n(numlist int >0 max=1) alt DISCRete CATegorical ///
        NOOGRID ogrid(numlist int >0 max=1) ///
        vce(str) NOSE Level(cilevel) _ifgenerate(namelist) Replace ]
    Parse_vce `vce'
    Parse_adjust `adjust'
    Parse_balance "`by'" "`pooled'" `balance'
    Parse_cat_notallowed "`discrete'" "`categorical'" `"`adjust'"'
    if "`discrete'`categorical'"!="" {
        if "`n'"!="" {
            di as err "{bf:n()} not allowed with {bf:discrete} or {bf:categorical}"
            exit 198
        }
        local discrete discrete // categorical implies discrete
        local atx atx // required by rd_PDF()
    }
    else {
        Parse_n "`n'" 10
    }
    Parse_ogrid "`noogrid'" "`ogrid'"
    
    // mark sample
    marksample touse
    markout `touse' `depvar' `bal_varlist'
    if "`clustvar'"!="" {
        markout `touse' `clustvar', strok
    }
    tempvar touse1 touse0 wvar
    Samplesetup `touse' `touse1' `touse0' `wvar' `depvar' ///
        "`by'" "`swap'" "`refvar'" "`weight'" `"`exp'"' ""
    Check_adjlog `touse' `depvar' "`refvar'" "`by'" "`adjlog'"
    
    // compute relative PDF
    mata: rd_generate_init("bal_wvar", 1)
    tempname b AT OGRID
    if "`discrete'`categorical'"!="" {
        mata: rd_PDF(strtoreal("`n'"))
    }
    else {
        mata: rd_HIST(`n')
    }
    
    // VCE
    if "`IFs'"!="" & "`_ifgenerate'"=="" {
        tempname V
        ComputeVCE `b' `V' `touse' "`IFs'" "`weight'" `"`exp'"' `"`vceopt'"'
    }
    
    // returns
    eret post `b' `V' [`weight'`exp'], obs(`N') esample(`touse')
    mata: rd_Post_common_e()
    eret local  subcmd     "histogram"
    eret local  title      "Relative histogram"
    eret matrix at         = `AT'
    eret local atx         "`atx'"
    eret local discrete    "`discrete'"
    eret local categorical "`categorical'"
    if "`ogrid'"!="" {
        eret matrix ogrid = `OGRID'
    }
    eret scalar n_hist   = `n'
    if "`discrete'`categorical'"=="" {
        eret scalar hwidth   = 1/`n'
        eret local  alt        "`alt'"
    }
    
    // return balancing weights
    mata: rd_generate_return("bal_wvar")
    
    // store IFs
    Store_IFs "`IFs'" "`_ifgenerate'"
end

program CDF, eclass
    // syntax
    Parse_syntax `0'
    syntax [if] [in] [fw iw pw/], [ ///
        NOBReak NOMID DESCending NOSTAble ADJust(str) BALance(str) ///
        n(numlist int >1 max=1) at(str) atx ATX2(str) DISCRete CATegorical alt ///
        NOOGRID ogrid(numlist int >0 max=1) ///
        vce(str) NOSE Level(cilevel) _ifgenerate(namelist) Replace ]
    Parse_vce `vce'
    if "`categorical'"!="" {
        if `"`adjust'"'!="" {
            di as err "{bf:adjust()} and {bf:categorical} not both allowed"
            exit 198
        }
    }
    Parse_at "`n'" `"`at'"' "`atx'" `"`atx2'"' "`discrete'" "`categorical'"
    Parse_cat_notallowed "`discrete'" "`categorical'" `"`adjust'"'
    Parse_adjust `adjust'
    Parse_balance "`by'" "`pooled'" `balance'
    Parse_ogrid "`noogrid'" "`ogrid'" "`atx'"
    
    // mark sample
    marksample touse
    markout `touse' `depvar' `bal_varlist'
    if "`clustvar'"!="" {
        markout `touse' `clustvar', strok
    }
    tempvar touse1 touse0 wvar
    Samplesetup `touse' `touse1' `touse0' `wvar' `depvar' ///
        "`by'" "`swap'" "`refvar'" "`weight'" `"`exp'"' ""
    Check_categorical `touse' `depvar' "`refvar'" "`categorical'"
    Check_adjlog `touse' `depvar' "`refvar'" "`by'" "`adjlog'"
    
    // compute relative CDF
    mata: rd_generate_init("bal_wvar", 1)
    tempname b AT OGRID
    mata: rd_CDF(strtoreal("`n'"))
    
    // VCE
    if "`IFs'"!="" & "`_ifgenerate'"=="" {
        tempname V
        ComputeVCE `b' `V' `touse' "`IFs'" "`weight'" `"`exp'"' `"`vceopt'"'
    }
    
    // returns
    eret post `b' `V' [`weight'`exp'], obs(`N') esample(`touse')
    mata: rd_Post_common_e()
    eret local subcmd      "cdf"
    eret local title       "Cumulative relative distribution"
    eret matrix at         = `AT'
    if "`atx'"!="" {
        if "`atx'"!="atx" {
            eret local atx "atx"
            eret local atxopt `"`atx'"' // comparison or reference 
        }
        else {
            eret local atx "atx"
            eret local atxopt `"`atx2'`ATX0'"' // numlist or matname
        }
    }
    else {
        eret local atopt `"`at'`AT0'"' // numlist or matname
    }
    eret local discrete    "`discrete'"
    eret local categorical "`categorical'"
    eret local alt         "`alt'"
    eret local origin      "`origin'"
    if "`ogrid'"!="" {
        eret matrix ogrid  = `OGRID'
    }
    eret scalar n = `n'
    
    // return balancing weights
    mata: rd_generate_return("bal_wvar")
    
    // store IFs
    Store_IFs "`IFs'" "`_ifgenerate'"
end

program DIV, eclass
    // syntax
    Parse_syntax `0'
    syntax [if] [in] [fw iw pw/], [ ///
        ENtropy kl CHI2 CHISQuared tvd DISsimilarity all ///
        NOBReak NOMID DESCending NOSTAble ///
        ADJust(str) BALance(str) Over(varname numeric) ///
        n(numlist int >0 max=1) alt DISCRete CATegorical PDF ///
        COMpare COMpare2(str) ///
        vce(str) NOSE Level(cilevel) _ifgenerate(namelist) Replace * ]
    // which measures?
    if "`all'"!="" local stats entropy chi2 tvd
    else {
        if "`kl'"!="" local entropy entropy
        if "`chisquared'"!="" local chi2 chi2
        if "`dissimilarity'"!="" local tvd tvd
        local stats `entropy' `chi2' `tvd'
        if "`stats'"=="" local stats entropy
    }
    // other options
    if `"`compare2'"'!="" local compare compare
    Get_densityopts dopts, `options'
    Parse_densityopts, `dopts'
    if "`pdf'"!="" {
        local c_bwidth `"`bwidth'"'
        if "`bwtype'"=="matrix" {
            local bwtype0 "matrix"
            local bwidth0 `bwidth'
            tempname bwidth
            if "`compare'"!="" {
                tempname c_bwidth
                local c_bwcol = min(colsof(`bwidth0'), 2)
            }
            local bwtype "scalar"
        }
    }
    Parse_vce `vce'
    Parse_adjust `adjust'
    Parse_balance "`by'" "`pooled'" `balance'
    Parse_cat_notallowed "`discrete'" "`categorical'" `"`adjust'"'
    if "`discrete'`categorical'"!="" {
        if "`n'"!="" {
            di as err "{bf:n()} not allowed with {bf:discrete} or {bf:categorical}"
            exit 198
        }
        if "`pdf'"!="" {
            di as err "{bf:pdf} not allowed with {bf:discrete} or {bf:categorical}"
            exit 198
        }
        local discrete discrete // categorical implies discrete
        local atx atx // required by rd_PDF()
    }
    else {
        if "`pdf'"!=""  Parse_n "`n'" 100
        else            Parse_n "`n'" 20
    }
    DIV_parse_compare "`by'" "`pooled'" `"`compare2'"'
    
    // mark sample
    marksample touse
    markout `touse' `depvar' `over' `bal_varlist' `c_bal_varlist'
    if "`clustvar'"!="" {
        markout `touse' `clustvar', strok
    }
    tempvar touse1 touse0 wvar
    Samplesetup `touse' `touse1' `touse0' `wvar' `depvar' ///
        "`by'" "`swap'" "`refvar'" "`weight'" `"`exp'"' "`over'"
    Check_adjlog `touse' `depvar' "`refvar'" "`by'" "`adjlog'"
    if "`compare'"!="" {
        if `"`bal_contrast'`c_bal_contrast'"'!="" {
            di as err "{bf:balance(, contrast)} and {bf:compare()} not both allowed"
            exit 198
        }
    }
    
    // compute divergence
    mata: rd_generate_init("bal_wvar", 1)
    mata: rd_generate_init("bal_wvar", 1, "c_")
    local nstat: list sizeof stats
    if "`nose'"=="" {
        local J = `nstat'
        if "`compare'"!="" local J = `J' * 3
        forv i=1/`J' {
            tempvar IF
            qui gen double `IF' = 0 if `touse'
            local IFS `IFs' `IF'
        }
    }
    tempname b BW
    if "`over'"=="" {
        if "`bwtype0'"=="matrix" {
            scalar `bwidth ' = `bwidth0'[1,1]
            if "`compare'"!="" {
                scalar `c_bwidth' = `bwidth0'[1,`c_bwcol']
            }
        }
        mata: rd_DIV("`b'", "`BW'")
        if `nstat'==1 {
            mat coleq `b' = ""
        }
    }
    else {
        tempname btmp BWtmp
        local TOUSE  `touse'
        local TOUSE1 `touse1'
        local TOUSE0 `touse0'
        tempname touse touse1 touse0 _N _N1 _N0 
        PrepareOver `N_over' "`overlevels'" "`by'" ///
            `touse' `touse1' `touse0' `_N' `_N1' `_N0'
        local i 0
        foreach o of local overlevels {
            local ++i
            if "`bwtype0'"=="matrix" {
                local bwrow = min(rowsof(`bwidth0'), `i')
                scalar `bwidth ' = `bwidth0'[`bwrow',1]
                if "`compare'"!="" {
                    scalar `c_bwidth' = `bwidth0'[`bwrow',`c_bwcol']
                }
            }
            PrepareOverlevel `i' "`over'==`o'" "`by'" `touse' `touse1' ///
                `touse0' `TOUSE' `TOUSE1' `TOUSE0' `_N' `_N1' `_N0' "`wgt'"
            mata: rd_DIV("`btmp'", "`BWtmp'")
            mat `b' = nullmat(`b'), `btmp'
            if "`pdf'"!="" {
                mat rown `BWtmp' = "`o'"
                mat `BW' = nullmat(`BW') \ `BWtmp'
            }
        }
        local touse `TOUSE'
    }
    if "`pdf'"!="" {
        if "`compare'"!="" {
            mat coln `BW' = "main" "alternate"
        }
        else {
            mat coln `BW' = "main"
        }
    }
    
    // VCE
    if "`IFs'"!="" & "`_ifgenerate'"=="" {
        tempname V
        ComputeVCE `b' `V' `touse' "`IFs'" "`weight'" `"`exp'"' `"`vceopt'"' `"`over'"'
    }
    
    // returns
    eret post `b' `V' [`weight'`exp'], obs(`N') esample(`touse')
    mata: rd_Post_common_e()
    eret local subcmd      "divergence"
    eret local title       "Relative distribution divergence"
    eret local statistics  "`stats'"
    eret local discrete    "`discrete'"
    eret local categorical "`categorical'"
    if "`discrete'`categorical'"=="" {
        if "`pdf'"!="" {
            eret scalar n = `n'
            eret local pdf "pdf"
            eret matrix bwidth = `BW'
            if "`bwmethod'"=="dpi" {
                local bwmethod `bwmethod'(`bwdpi')
            }
            if "`bwmethod'"!="" {
                if "`bwnord'"!="" {
                    local bwmethod `bwmethod', `bwnord'
                }
            }
            eret local  bwmethod "`bwmethod'"
            eret scalar bwadjust = `bwadjust'
            eret local  kernel   "`kernel'"
            eret scalar adaptive = `adaptive'
            eret local  exact    "`exact'"
            eret local  boundary "`boundary'"
            eret scalar napprox = `napprox'
        }
        else {
            eret scalar n_hist = `n'
            eret local  alt "`alt'"
        }
    }
    eret local compare "`compare'"
    if "`compare'"!="" {
        eret local c_adjust     `"`c_adj1'"'
        eret local c_refadjust  `"`c_adj0'"'
        eret local c_adjmean    `"`c_adjmean'"'
        eret local c_adjsd      `"`c_adjsd'"'
        eret local c_adjlog     `"`c_adjlog'"'
        eret local c_adjmult    `"`c_adjmult'"'
        if `"`c_bal_varlist'"'!="" {
            eret local c_balance      `"`c_bal_varlist'"'
            eret local c_balmethod    `"`c_bal_method'"'
            eret local c_balref       `"`c_bal_ref'"'
            eret local c_balcontrast  `"`c_bal_contrast'"'
            eret local c_balopts      `"`c_bal_opts'"'
        }
    }
    
    // return balancing weights
    mata: rd_generate_return("bal_wvar")
    mata: rd_generate_return("bal_wvar", "c_")
    
    // store IFs
    Store_IFs "`IFs'" "`_ifgenerate'"
end

program DIV_parse_compare
    args by pooled 0
    if `"`0'"'=="" exit
    local 0 `", `0'"'
    capt n syntax [, ADJust(str) BALance(str) ]
    if _rc==1 exit _rc
    if _rc {
        di as err "error in option {bf:compare()}"
        exit 198
    }
    // adjust option
    capt n Parse_adjust `adjust'
    if _rc==1 exit _rc
    if _rc {
        di as err "error in option {bf:compare()}"
        exit 198
    }
    c_local c_adj1 `adj1'
    c_local c_adj0 `adj0'
    c_local c_adjmean `adjmean'
    c_local c_adjsd   `adjsd'
    c_local c_adjlog  `adjlog'
    c_local c_adjmult `adjmult'
    // balance option
    capt n Parse_balance "`by'" "`pooled'" `balance'
    if _rc==1 exit _rc
    if _rc {
        di as err "error in option {bf:compare()}"
        exit 198
    }
    c_local c_bal_varlist  `"`bal_varlist'"'
    c_local c_bal_method   `bal_method'
    c_local c_bal_noisily  `bal_noisily'
    c_local c_bal_ref      `bal_ref'
    c_local c_bal_contrast `bal_contrast'
    c_local c_bal_opts     `"`bal_opts'"'
    c_local c_bal_ebopts   `"`bal_ebopts'"'
    c_local c_bal_wvar     `bal_wvar'
end

program MRP, eclass
    // syntax
    Parse_syntax `0'
    syntax [if] [in] [fw iw pw/], [ ///
        NOBReak NOMID DESCending NOSTAble BALance(str) Over(varname numeric) ///
        SCale SCale2(str) MULTiplicative LOGarithmic REFerence ///
        vce(str) NOSE Level(cilevel) _ifgenerate(namelist) Replace ]
    Parse_vce `vce'
    if `"`scale2'"'!="" local scale scale
    if "`reference'"!="" local adj adj0
    else                 local adj adj1
    local `adj' location
    if "`scale'"!="" {
        if "`multiplicative'"!="" {
            di as err "{bf:scale} and {bf:multiplicative} not both allowed"
            exit 198
        }
        local `adj' ``adj'' scale
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
    Parse_balance "`by'" "`pooled'" `balance'
    
    // mark sample
    marksample touse
    markout `touse' `depvar' `over' `bal_varlist'
    if "`clustvar'"!="" {
        markout `touse' `clustvar', strok
    }
    tempvar touse1 touse0 wvar
    Samplesetup `touse' `touse1' `touse0' `wvar' `depvar' ///
        "`by'" "`swap'" "`refvar'" "`weight'" `"`exp'"' "`over'"
    Check_adjlog `touse' `depvar' "`refvar'" "`by'" "`adjlog'"
    
    // compute polarization statistics
    mata: rd_generate_init("bal_wvar", 1)
    if "`nose'"=="" {
        forv i = 1/3 {
            tempvar IF
            qui gen double `IF' = 0 if `touse'
            local IFS `IFs' `IF'
        }
    }
    tempname b btmp
    if "`over'"=="" {
        mata: rd_MRP("`b'")
    }
    else {
        local TOUSE  `touse'
        local TOUSE1 `touse1'
        local TOUSE0 `touse0'
        tempname touse touse1 touse0 _N _N1 _N0 
        PrepareOver `N_over' "`overlevels'" "`by'" ///
            `touse' `touse1' `touse0' `_N' `_N1' `_N0'
        local i 0
        foreach o of local overlevels {
            local ++i
            PrepareOverlevel `i' "`over'==`o'" "`by'" `touse' `touse1' ///
                `touse0' `TOUSE' `TOUSE1' `TOUSE0' `_N' `_N1' `_N0' "`wgt'"
            mata: rd_MRP("`btmp'")
            mat coleq `btmp' = "`o'"
            mat `b' = nullmat(`b'), `btmp'
        }
        local touse `TOUSE'
    }
    
    // VCE
    if "`IFs'"!="" & "`_ifgenerate'"=="" {
        tempname V
        ComputeVCE `b' `V' `touse' "`IFs'" "`weight'" `"`exp'"' `"`vceopt'"' `"`over'"'
    }
    
    // returns
    eret post `b' `V' [`weight'`exp'], obs(`N') esample(`touse')
    mata: rd_Post_common_e()
    eret local  subcmd   "mrp"
    eret local  title    "Median relative polarization"
    
    // return balancing weights
    mata: rd_generate_return("bal_wvar")
    
    // store IFs
    Store_IFs "`IFs'" "`_ifgenerate'"
end

program SUM, eclass
    // syntax
    Parse_syntax `0'
    syntax [if] [in] [fw iw pw/], [ ///
        NOBReak NOMID DESCending NOSTAble ///
        ADJust(str) BALance(str) Over(varname numeric) ///
        Statistics(str) Generate(name) ///
        vce(str) NOSE Level(cilevel) _ifgenerate(namelist) Replace ]
    Parse_vce `vce'
    Parse_adjust `adjust'
    Parse_balance "`by'" "`pooled'" `balance'
    mata: rd_generate_init("generate")
    foreach s of local statistics {
        SUM_parse_stats, `s'
        local stats `stats' `s'
    }
    local stats: list uniq stats
    if `"`stats'"'=="" local stats mean
    
    // mark sample
    marksample touse
    markout `touse' `depvar' `over' `bal_varlist'
    if "`clustvar'"!="" {
        markout `touse' `clustvar', strok
    }
    tempvar touse1 touse0 wvar
    Samplesetup `touse' `touse1' `touse0' `wvar' `depvar' ///
        "`by'" "`swap'" "`refvar'" "`weight'" `"`exp'"' "`over'"
    Check_adjlog `touse' `depvar' "`refvar'" "`by'" "`adjlog'"
    
    // compute relative ranks, statistics, and influence functions
    mata: rd_generate_init("bal_wvar", 1)
    if "`nose'"=="" {
        foreach stat of local stats {
            tempvar IF
            qui gen double `IF' = 0 if `touse'
            local IFS `IFs' `IF'
        }
    }
    tempname b
    if "`over'"=="" {
        mata: rd_SUM("`b'")
    }
    else {
        tempname btmp
        local TOUSE  `touse'
        local TOUSE1 `touse1'
        local TOUSE0 `touse0'
        tempname touse touse1 touse0 _N _N1 _N0 
        PrepareOver `N_over' "`overlevels'" "`by'" ///
            `touse' `touse1' `touse0' `_N' `_N1' `_N0'
        local i 0
        foreach o of local overlevels {
            local ++i
            PrepareOverlevel `i' "`over'==`o'" "`by'" `touse' `touse1' ///
                `touse0' `TOUSE' `TOUSE1' `TOUSE0' `_N' `_N1' `_N0' "`wgt'"
            mata: rd_SUM("`btmp'")
            mat coleq `btmp' = "`o'"
            mat `b' = nullmat(`b'), `btmp'
        }
        local touse `TOUSE'
    }
    
    // VCE
    if "`IFs'"!="" & "`_ifgenerate'"=="" {
        tempname V
        ComputeVCE `b' `V' `touse' "`IFs'" "`weight'" `"`exp'"' `"`vceopt'"' `"`over'"'
    }
    
    // returns
    eret post `b' `V' [`weight'`exp'], obs(`N') esample(`touse')
    mata: rd_Post_common_e()
    eret local subcmd     "summarize"
    eret local title      "Relative ranks"
    eret local statistics "`stats'"
    
    // store ranks in variable
    mata: rd_generate_return("generate")
    if "`generate'"!="" {
        lab var `generate' "Relative ranks"
        eret local generate `generate'
    }
    
    // return balancing weights
    mata: rd_generate_return("bal_wvar")
    
    // store IFs
    Store_IFs "`IFs'" "`_ifgenerate'"
end

program SUM_parse_stats
    syntax [, Mean MEDian sd Variance iqr * ]
    if `"`options'"'!="" {
        if (substr(`"`options'"',1,1)=="p") {
            local 0 = substr(`"`options'"',2,.)
            local 0 `", p(`0')"'
            syntax, p(numlist min=1 max=1 int >0 <100)
            local p p`p'
        }
        else exit 198
    }
    c_local s `mean' `median' `sd' `variance' `iqr' `p'
end

version 12
// struct
local DATA   rd_data
local Data   struct `DATA' scalar
local GRP    rd_data_grp
local Grp    struct `GRP' scalar
local BAL    rd_data_bal
local Bal    struct `BAL' scalar
local ADJ    rd_adj
local Adj    struct `ADJ' scalar
local GADJ   rd_adj_grp
local Gadj   struct `GADJ' scalar
// string
local SS     string scalar
local SR     string rowvector
local SM     string matrix
// real
local RS     real scalar
local RV     real vector
local RC     real colvector
local RR     real rowvector
local RM     real matrix
// counters
local Int    real scalar
local IntC   real colvector
local IntR   real rowvector
// boolean
local Bool   real scalar
local BoolR  real rowvector
local TRUE   1
local FALSE  0
// pointer
local PSRC   pointer(real colvector) scalar
local PGRP   pointer(`Grp') scalar
local PDF    class mm_density scalar
mata:
mata set matastrict on

/* Helper functions directly called by ado ----------------------------------*/

void Abbrev(`SS' nm, `SS' s, `Int' l, | `Bool' dots)
{
    `Int' l1
    
    if (l<=32) st_local(nm, abbrev(s, l))
    else {
        if (args()<4) dots = 0
        if (dots==0) st_local(nm, _Substr(s, 1, l))
        else {
            l1 = _Strlen(s)
            if (l1<=l) st_local(nm, s)
            else       st_local(nm, _Substr(s, 1, l-3)+"...")
        }
    }
}

void Strlen(`SS' nm, `SS' s, | `SS' s2)
{
    if (args()<3) st_local(nm, strofreal(_Strlen(s)))
    else          st_local(nm, strofreal(max((_Strlen(s),_Strlen(s2)))))
}

`SM' _Substr(`SM' s, `RM' b, `RM' l)
{
    if (stataversion()>=1400) return(Substr14(s, b, l))
    return(substr(s, b, l))
}

`SM' _Substr14(`SM' s, `RM' b, `RM' l)
{
    return(udsubstr(s, b, l))
}

`RM' _Strlen(`SM' s)
{
    if (stataversion()>=1400) return(_Strlen14(s))
    return(strlen(s))
}

`RM' _Strlen14(`SM' s)
{
    return(udstrlen(s))
}

void rd_GetCI(`RS' level0, `Int' citype)
{   // citype: 0 = regular, 1 = logit, 2 = probit, 3 = atanh, 4 = log
    `Int' i, r 
    `RS'  level
    `RR'  b, df, se, z
    `RM'  CI
    
    // obtain b and se
    b = st_matrix("e(b)")
    se = sqrt(diagonal(st_matrix("e(V)")))'
    r = length(b)
    // obtain critical value
    level = 1 - (1 - level0/100)/2
    df = .
    if (st_global("e(mi)")=="mi") {
        if (st_matrix("e(df_mi)")!=J(0,0,.))        df = st_matrix("e(df_mi)")
        else if (st_numscalar("e(df_r)")!=J(0,0,.)) df = st_numscalar("e(df_r)")
    }
    else if (st_numscalar("e(df_r)")!=J(0,0,.))     df = st_numscalar("e(df_r)")
    if (length(df)==1) z = df>2e17 ? invnormal(level) : invttail(df, 1-level)
    else {
        z = J(1, r, .)
        for (i=1; i<=r; i++) {
            z[i] = df[i]>2e17 ? invnormal(level) : invttail(df[i], 1-level)
        }
    }
    // compute ci
    if (citype==1) { // logit
        z = z :* se :/ (b:* (1 :- b))
        CI = invlogit(logit(b) :- z \ logit(b) :+ z)
    }
    else if (citype==2) { // probit
        z = z :* se :/ normalden(invnormal(b))
        CI = normal(invnormal(b) :- z \ invnormal(b) :+ z)
    }
    else if (citype==3) { // atanh
        z = z :* se :/ (1 :- b:^2)
        CI = tanh(atanh(b) :- z \ atanh(b) :+ z)
    }
    else if (citype==4) { // log
        z = z :* se :/ b
        CI = exp(ln(b) :- z \ ln(b) :+ z) 
    }
    else { // regular
        CI = (b :- z:*se \ b :+ z:*se)
    }
    // return result
    st_matrix(st_local("cimat"), CI)
}

void rd_check_mat(`SS' nm, `Int' kind)
{
    `RM' m
    
    m = st_matrix(nm)
    if (kind==1) {  // check whether positive integer
        assert(!any(m:<0))
        assert(!any(m:!=trunc(m)))
        return
    }
    // check whether in [0,1]
    assert(!any(m:<0))
    assert(!any(m:>1))
}

void rd_generate_init(`SS' nm, | `Bool' bal, `SS' prefix)
{   // initializes a tempvar
    // returns the varname in local macro named as strupper(nm)
    // fills in tempvar = w if bal!=0
    `SS'  tmpnm, w
    `Int' rc
    
    if (args()<2) bal = 0
    if (st_local(prefix+nm)=="") return
    if (st_local("replace")=="") {
        rc = _stata("confirm new variable " + st_local(prefix+nm))
        if (rc) exit(rc)
    }
    tmpnm = st_tempname()
    st_local(prefix+strupper(nm), tmpnm)
    if (!bal) {
        stata("qui gen double " + tmpnm + " = .")
        return
    }
    if (st_local("weight")!="") w = st_local("wvar")
    else                        w = "1"
    stata("qui gen double " + tmpnm + " = " + w + " if " + st_local("touse"))
}

void rd_generate_return(`SS' nm, | `SS' prefix)
{
    `SS'  tmpv
    `Int' idx
    
    if (st_local(prefix+nm)=="") return
    tmpv = st_local(prefix+strupper(nm))
    if (tmpv=="") return // should never happen
    if ((idx = _st_varindex(st_local(prefix+nm)))<.) st_dropvar(idx)
    st_varrename(tmpv, st_local(prefix+nm))
}

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
        st_global("e(swap)", st_local("swap"))
        st_global("e(pooled)", st_local("pooled"))
    }
    else {
        st_global("e(refvar)", st_local("refvar"))
    }
    st_global("e(nobreak)",    st_local("nobreak"))
    st_global("e(nomid)",      st_local("nomid"))
    st_global("e(descending)", st_local("descending"))
    st_global("e(nostable)",   st_local("nostable"))
    st_global("e(adjust)",     st_local("adj1"))
    st_global("e(refadjust)",  st_local("adj0"))
    st_global("e(adjmean)",    st_local("adjmean"))
    st_global("e(adjsd)",      st_local("adjsd"))
    st_global("e(adjlog)",     st_local("adjlog"))
    st_global("e(adjmult)",    st_local("adjmult"))
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
    if (st_local("bal_varlist")!="") {
        st_global("e(balance)",    st_local("bal_varlist"))
        st_global("e(balmethod)",  st_local("bal_method"))
        st_global("e(balref)",     st_local("bal_ref"))
        st_global("e(balcontrast)",st_local("bal_contrast"))
        st_global("e(balopts)",    st_local("bal_opts"))
    }
    st_global("e(predict)", "reldist predict")
    if (st_local("V")!="") {
        st_global("e(vcetype)", st_local("vcetype"))
        st_global("e(vce)", st_local("vce"))
        stata("ereturn scalar rank = " + st_local("rank"))
        stata("ereturn scalar df_r = " + st_local("df_r"))
        if (st_local("vce")=="cluster") {
            st_global("e(clustvar)", st_local("clustvar"))
            stata("ereturn scalar N_clust = " + st_local("N_clust"))
        }
    }
}

void rd_flip_V()
{
    `RS' l, i, j, I, J
    `RC' p
    
    I = st_numscalar("e(N_over)")
    J = st_numscalar("e(k_eq)")
    p = J(I*J, 1, .)
    l = 0
    for (i=1;i<=I;i++) {
        for (j=1;j<=J;j++) {
            p[++l] = (j-1)*I + i
        }
    }
    st_replacematrix(st_local("V"), st_matrix(st_local("V"))[p,p])
}

void rd_svmat(`SS' nm, `SR' vnms, `Bool' transpose)
{
    `RM' M

    if (transpose) M = st_matrix(nm)'
    else           M = st_matrix(nm)
    if (cols(M)>cols(vnms))      M = M[,1..cols(vnms)]
    else if (cols(M)<cols(vnms)) vnms = vnms[1..cols(M)]
    st_store((1,rows(M)), st_addvar("double",vnms), M)
}

void rd_olab(`Bool' cdf, `Bool' atx, `SS' nm, `SS' fmt, `RS' prune, `Bool' at)
{
    `RR' y, x, p, r
    
    if (atx) {
        y = st_matrix("e(at)")[2,]
        if (cdf) p = st_matrix("e(b)")
        else     p = st_matrix("e(at)")[1,]
        if (st_global("e(subcmd)")=="histogram") {
            p = ((0,p)[|1\cols(p)|] + p) / 2 // shift to midpoint of bin
        }
    }
    else {
        y = st_matrix("e(ogrid)")[1+cdf,]
        p = (0..length(y)-1) / (length(y)-1)
    }
    if (substr(st_local(nm),1,1)=="#") {
        x = strtoreal(substr(st_local(nm),2,.))
        if (x==2) x = minmax(y)
        else x = _mm_unique(min(y) \ _mm_quantile(y', mm_diff(0\p'), 
            (1::x-2)/(x-1), 1) \ max(y))'
        st_local(nm+"_x", invtokens(strofreal(x)))
        r = _rd_olab_pos(y, p, x)
    }
    else if (at) {
        prune = .
        r = strtoreal(tokens(st_local(nm)))
        // use inverted _rd_olab_pos() function (equal to quantile def=1)
        x = _rd_olab_pos(-p[length(p)..1], -y[length(y)..1], -r[length(r)..1])
        x = -x[length(x)..1]
    }
    else {
        x  = strtoreal(tokens(st_local(nm)))
        if (length(x)==0) {
            st_local(nm,"")
            return
        }
        r = _rd_olab_pos(y, p, x)
    }
    st_local(nm, _rd_olab_fmt(r, x, fmt, prune))
}

`RV' _rd_olab_pos(`RV' y, `RV' p, `RV' x)
{
    // get positions of x in p = CDF(y)
    // if x is between two y values: use p of lower y
    // if x is equal to y:           use max(p) of matching y's
    // this also implies: if x < min(y):  position = 0
    //                    if x > max(y):  position = 1
    `Int' i, n, j, r 
    `RV'  P
    
    n = length(x)
    P = J(rows(x), cols(x), .)
    for (i=1; i<=n; i++) {
        if (x[i] >= y[1]) break
        P[i] = 0
    }
    r = length(y)
    j = 1
    for (; i<=n; i++) {
        for (; j<r; j++) {
            if (y[j+1] > x[i]) break
        }
        P[i] = p[j]
    }
    return(P)
}

`SS' _rd_olab_fmt(`RR' p, | `RR' x, `SS' fmt, `RS' prune)
{
    `Int' i, r
    `RS'  p0
    `SR'  lbl
    
    if (fmt=="") return(invtokens(strofreal(p)))
    lbl = `"""' :+ strofreal(x, fmt)  :+ `"""'
    if (prune<.) {
        p0 = .
        r = length(p)
        for (i=1; i<=r;i++) {
            if (i<r) {
                if (p[i]==p[i+1]) {
                    // ties in p: print largest label
                    lbl[i] = `"" ""'
                    continue
                }
            }
            if ((p[i]-p0)<prune) {
                lbl[i] = `"" ""'
                continue
            }
            p0 = p[i]
        }
    }
    return(invtokens((strofreal(p) :+ " " :+ lbl)))
}

void rd_svylbl_b()
{
    `SM' cstripe
    
    cstripe = st_matrixcolstripe(st_local("b"))
    cstripe[,1] = cstripe[,1] :+ "@" :+ cstripe[,2]
    cstripe[,2] = J(rows(cstripe), 1, "_cons")
    st_matrixcolstripe(st_local("b"), cstripe)
}

void rd_svylbl_b_undo()
{
    `RC' pos
    `SM' cstripe
    
    cstripe = st_matrixcolstripe(st_local("b"))
    pos = strpos(cstripe[,1], "@")
    cstripe[,2] = substr(cstripe[,1],pos:+1,.)
    cstripe[,1] = substr(cstripe[,1],1,pos:-1)
    st_local("k_eq", strofreal(rows(uniqrows(cstripe[,1]))))
    st_matrixcolstripe(st_local("b"), cstripe)
}

/* Data preparation and some common functions -------------------------------*/

struct `ADJ' {
    `Bool'  true       // adjust() has been specified
    `Bool'  mean       // 0 use median, 1 use mean
    `Bool'  sd         // 0 use IQR, 1 use sd
    `Int'   link       // 0 linear/additive, 1 logarithmic, 2 multiplicative
}

struct `GADJ' {
    `Bool'  true       // group has adjustment
    `Bool'  location   // adjust location
    `Bool'  scale      // adjust scale
    `Bool'  shape      // adjust shape
}

struct `GRP' {
    `Int'   touse      // Stata variable marking sample
    `Int'   yvar       // Stata variable containing outcome data
    `Int'   desc       // -1 = descending; 1 = ascending
    `Bool'  stabl      // use stable sort order within ties
    `RC'    y          // outcome data
    `Int'   N          // N of observations
    `RC'    w          // weights
    `RS'    W          // sum of weights
    `Int'   wtype      // (see below)
    `Bool'  bal        // has balancing
    `Gadj'  adj        // location/scale/shape adjustments
    `RC'    lny        // log of outcome data
    `RC'    l          // location
    `RS'    s          // scale
    `Bool'  nose       // do not compute influence functions
    `RM'    IF         // influence functions
    `RC'    IFl        // influence function of location measure
    `RC'    IFs        // influence function of scale measure
    `IntC'  p          // permutation vector
}

struct `BAL' {
    `Bool'  ref        // balance reference group
    `Bool'  contrast   // compute RD of balanced vs. unbalanced
    `SS'    zvars      // names of balancing variables
    `SS'    T          // Stata variable marking treatment group
    `Int'   T_N        // treatment group: N of observations
    `RM'    T_IFZ      // treatment group: balancing model IFs
    `SS'    C          // Stata variable marking control group
    `RM'    Z          // control group: balancing variables (view)
    `RM'    IFZ        // control group: IFs of balancing model
    `RC'    w0         // control group: base weight
    `RC'    w          // control group: total weight (including everything)
    `RC'    wb         // control group: total weight - "pooled" component
}

struct `DATA' {
    `Bool'  by         // syntax 1
    `Bool'  tbreak     // break ties
    `Bool'  mid        // use midpoints for relative ranks
    `Bool'  pooled     // reference is pooled distribution
    `Int'   wtype      // weights: 0 none, 1 fw, 2 pw, 3 iw
    `Bool'  nose       // do not compute influence functions
    `Bal'   bal        // balancing results
    `Adj'   adj        // adjustment settings
    `Grp'   D          // comparison distribution data
    `Grp'   R          // reference distribution data
    `PGRP'  G1         // pointer to D or R (comparison distribution)
    `PGRP'  G0         // pointer to D or R (reference distribution)
    `PSRC'  Y1         // pointer to (adjusted) comparison outcome
    `PSRC'  Y0         // pointer to (adjusted) reference outcome
    `RC'    ranks      // relative ranks
}

void rd_getdata(`Data' data)
{
    `SS'   weight
    
    // setup
    data.by       = (st_local("by")!="")
    data.tbreak   = (st_local("nobreak")=="")
    data.mid      = (st_local("nomid")=="")
    data.D.desc   = data.R.desc = (st_local("descending")!="" ? -1 : 1)
    data.D.stabl  = data.R.stabl = (st_local("nostable")=="")
    data.pooled   = (st_local("pooled")!="")
    weight        = st_local("weight")
    data.wtype    = (weight=="fweight" ? 1 :
                    (weight=="pweight" ? 2 :
                    (weight=="iweight" ? 3 : 0)))
                    // iweights will be treated like pweights
    data.D.wtype  = data.R.wtype = data.wtype
    data.nose     = (st_local("nose")!="")
    data.D.nose   = data.R.nose = data.nose
    
    // balancing
    data.bal.ref      = (st_local("bal_ref")!="")
    data.bal.contrast = (st_local("bal_contrast")!="")
    rd_balance(data, data.bal)
    
    // comparison group data
    data.D.yvar = st_varindex(st_local("depvar"))
    if (data.by) {
        if (data.D.bal) {
            if (data.bal.ref) data.D.touse = st_varindex(st_local("touse0"))
            else              data.D.touse = st_varindex(st_local("touse1"))
        }
        else data.D.touse = st_varindex(st_local("touse1"))
    }
    else data.D.touse = st_varindex(st_local("touse"))
    _rd_getdata(data, data.D)
    
    // reference group data
    if (data.by) {
        data.R.yvar = data.D.yvar
        if (data.R.bal) {
            if (data.bal.ref) data.R.touse = st_varindex(st_local("touse0"))
            else              data.R.touse = st_varindex(st_local("touse1"))
        }
        else {
            if (data.pooled)  data.R.touse = st_varindex(st_local("touse"))
            else              data.R.touse = st_varindex(st_local("touse0"))
        }
    }
    else {
        data.R.yvar = st_varindex(st_local("refvar"))
        data.R.touse = st_varindex(st_local("touse"))
    }
    _rd_getdata(data, data.R)
    
    // check number of obs
    if (data.D.N<1 | data.R.N<1) {
        if (st_local("over")!="") {
            display("{err}insufficient observations for " + st_local("over") + 
                " = "+st_local("o"))
        }
        else display("{err}insufficient observations")
        display("{err}must have at least 1 observation per distribution")
        exit(2001)
    }
    
    // set wtype to pw if balanced without base weights
    if (data.wtype==0) {
        if (data.D.bal) {
            data.R.w = J(rows(data.R.y),1,1)
            data.D.wtype = data.R.wtype = data.wtype = 2
        }
        else if (data.R.bal) {
            data.D.w = J(rows(data.D.y),1,1)
            data.D.wtype = data.R.wtype = data.wtype = 2
        }
    }
    
    // apply location, scale, shape adjustments
    _rd_getadj(data.D.adj, "adj1")
    _rd_getadj(data.R.adj, "adj0")
    data.adj.true  = (data.D.adj.true | data.R.adj.true)
    data.adj.mean  = (st_local("adjmean")!="")
    data.adj.sd    = (st_local("adjsd")!="")
    data.adj.link  = 0 + (st_local("adjlog")!="") + 2*(st_local("adjmult")!="")
    _rd_adjust(data)
}

void _rd_getdata(`Data' data, `Grp' G)
{
    // outcome data
    G.y = st_data(., G.yvar, G.touse)
    G.N = rows(G.y)
    
    // get base weights
    if (G.wtype) {
        if (G.bal) {
            G.w = data.bal.w
            if (data.nose==0) {
                // compute net balancing weights; needed for IFs
                data.bal.w = data.bal.w :/ data.bal.w0
            }
        }
        else G.w = st_data(., st_local("wvar"), G.touse)
        G.W = quadsum(G.w)
    }
    else {
        if (G.bal) G.w = data.bal.w
        else       G.w = 1
        G.W = G.N
    }
    
    // order the data
    if (G.bal) {
        // order by base weights, not the balancing weights
        if (rows(data.bal.w0)!=1) {
            G.p  = mm_order((G.y,data.bal.w0), (1,2*G.desc), G.stabl)
            data.bal.w0 = data.bal.w0[G.p] // not really needed
        }
        else G.p = mm_order(G.y, 1, G.stabl)
        G.w = G.w[G.p]
        if (data.nose==0) {
            data.bal.w    = data.bal.w[G.p]
            data.bal.wb   = data.bal.wb[G.p]
            data.bal.Z    = data.bal.Z[G.p,]
            data.bal.IFZ  = data.bal.IFZ[G.p,]
        }
    }
    else if (rows(G.w)!=1) {
        G.p = mm_order((G.y,G.w), (1,2*G.desc), G.stabl)
        G.w = G.w[G.p]
    }
    else G.p = mm_order(G.y, 1, G.stabl)
    G.y = G.y[G.p]
    // redefine G.p so that it can be used to write data back to Stata
    G.p = invorder(G.p)
}

void rd_balance(`Data' data, `Bal' bal)
{
    // has balancing?
    data.D.bal = data.R.bal = 0
    if (st_local("bal_varlist")=="") return
    
    // expand bal_varlist (so that it will be stable)
    stata("fvexpand \`bal_varlist' if \`touse'")
    bal.zvars = st_global("r(varlist)")
    
    // set flag for distribution that will contain balancing
    if   (data.bal.ref==data.bal.contrast) data.D.bal = 1
    else                                   data.R.bal = 1
    
    // determine treatment group and control group
    if (data.bal.ref) {
        bal.T = st_local("touse1") // treatment group
        bal.C = st_local("touse0") // control group (to be balanced)
    }
    else {
        bal.T = st_local("touse0")  // treatment group
        bal.C = st_local("touse1")  // control group (to be balanced)
    }
    // compute balancing weights and IFs
    if (st_local("bal_method")=="eb") rd_balance_eb(data, bal)
    else                              rd_balance_ipw(data, bal)
    
    // store balancing weights
    if (st_local("BAL_WVAR")!="") {
        st_store(., st_local("BAL_WVAR"), bal.C, bal.w)
    }
}

void rd_balance_ipw(`Data' data, `Bal' bal)
{
    `SR' ps
    
    // run logit and obtain ps
    stata("quietly \`bal_noisily' logit " + bal.T + " " + bal.zvars +
        " if \`touse' \`wgt', \`bal_opts'")
    ps = st_tempname()
    stata("quietly predict double " + ps + " if e(sample), pr")
    
    // compute balancing weights (balanced group only)
    bal.w = st_data(., ps, bal.C)
    bal.w = bal.w :/ (1 :- bal.w)
    if (data.wtype) {
        // factor in base weights and rescale to size of treatment group
        bal.w0 = st_data(., st_local("wvar"), bal.C)
        bal.w  = bal.w :* bal.w0
        bal.w  = bal.w * (quadsum(st_data(., st_local("wvar"), bal.T)) / quadsum(bal.w))
    }
    else {
        // rescale to size of treatment group (sum of logit-IPW weights
        // only approximates the size of the treatment group)
        bal.w0 = 1
        bal.w  = bal.w * (rows(st_data(., bal.T, bal.T)) / quadsum(bal.w))
    }
    if (hasmissing(bal.w)) {
        printf("{err}warning: some observations lost during computation of balancing weights\n")
        printf("{err}         balancing may be poor")
        if (data.nose==0) printf("; standard errors may be invalid\n")
        else              printf("\n")
        _editmissing(bal.w, 0)
    }
    
    // fillin IFs
    if (data.nose==0) rd_balance_ipw_IF(data, bal, 
        st_data(., bal.T, st_local("touse")), 
        st_data(., ps, st_local("touse")))
    
    // update weights if -pooled- and normalize
    rd_balance_rescale(data, bal)
    
    // cleanup
    st_dropvar(ps)
}

void rd_balance_ipw_IF(`Data' data, `Bal' bal, `RC' Y, `RC' p)
{
    `Bool' cons
    `RC'   h, w
    `RM'   Z, IFZ
    pragma unset Z
    
    // compute IFs
    cons  = anyof(st_matrixcolstripe("e(b)")[,2], "_cons")
    st_view(Z, ., bal.zvars, st_local("touse"))
    h = (Y :- p)
    if (cons) h = Z :* h, h
    else      h = Z :* h
    w = p :* (1 :- p)
    if (data.wtype) w = w :* st_data(., st_local("wvar"), st_local("touse"))
    IFZ = h * invsym(quadcross(Z, cons, w, Z, cons))'
    
    // store results in data.bal
    bal.T_IFZ = select(IFZ, Y:==1)
    bal.T_N   = rows(bal.T_IFZ)
    bal.IFZ   = select(IFZ, Y:==0)
    bal.Z     = st_data(., bal.zvars, bal.C)
    if (cons) bal.Z = bal.Z, J(rows(bal.Z), 1, 1)
}

void rd_balance_eb(`Data' data, `Bal' bal)
{
    `RM' X1, X0
    `RC' w1
    `SR' opts
    transmorphic S
    pragma unset X1
    pragma unset X0
    
    // data
    st_view(X1, ., bal.zvars, bal.T)
    st_view(X0, ., bal.zvars, bal.C)
    if (data.wtype) {
        w1     = st_data(., st_local("wvar"), bal.T)
        bal.w0 = st_data(., st_local("wvar"), bal.C)
    }
    else bal.w0 = w1 = 1
    S = mm_ebal_init(X1, w1, X0, bal.w0)

    // settings
    opts = tokens(st_local("bal_ebopts"))
    if (opts[1]!="") mm_ebal_btol(S, strtoreal(opts[1]))
    if (opts[2]!="") mm_ebal_difficult(S, 1)
    if (opts[3]!="") mm_ebal_maxiter(S, strtoreal(opts[3]))
    if (opts[4]!="") mm_ebal_ptol(S, strtoreal(opts[4]))
    if (opts[5]!="") mm_ebal_vtol(S, strtoreal(opts[5]))
    if (st_local("bal_noisily")=="") mm_ebal_trace(S, "none")

    // estimation
    if (mm_ebal(S)==0) {
        printf("{err}warning: balancing tolerance not achieved\n")
        printf("{err}         balancing may be poor")
        if (data.nose==0) printf("; standard errors may be invalid\n")
        else              printf("\n")
    }
    bal.w = mm_ebal_W(S)
    if (hasmissing(bal.w)) {
        display("{err}unexpected error; balancing weights contain missing values")
        exit(error(499))
    }

    // fillin IFs
    if (data.nose==0) rd_balance_eb_IF(bal, X1, w1, X0)

    // update weights if -pooled- and normalize
    rd_balance_rescale(data, bal)
}

void rd_balance_eb_IF(`Bal' bal, `RM' X1, `RC' w1, `RM' X0)
{
    `RS' W, odds
    `RR' M 
    `RC' h
    `RM' G
    
    // beta
    M    = mean(X1, w1) // target moments
    h    = X0 :- M
    G    = invsym(quadcross(h, bal.w, X0))
    bal.T_IFZ = ((X1 :- M) * G') 
    bal.IFZ   = -(bal.w:/bal.w0) :* h * G'
    bal.T_N   = rows(bal.T_IFZ)
    bal.Z     = X0 // (no longer a view)
    
    // alpha
    odds = (rows(w1)==1 ? w1*rows(X1) : quadsum(w1)) /
           (rows(bal.w0)==1 ? bal.w0*rows(X0) : quadsum(bal.w0))
    h    = bal.w:/bal.w0 :- odds
    G    = quadcolsum(X0 :* bal.w)
    W    = quadsum(bal.w)
    bal.T_IFZ = bal.T_IFZ,  (1 :- bal.T_IFZ * G')/W
    bal.IFZ   = bal.IFZ,   -(h :+ odds :+ bal.IFZ * G')/W
    bal.Z     = bal.Z,      J(rows(bal.Z), 1, 1)
}

void rd_balance_rescale(`Data' data, `Bal' bal)
{
    `RS' c

    // at this point sum(bal.w) is equal to the size of the treatment group
    if (data.nose==0) bal.wb = bal.w
    if (data.wtype) {
        // add base weight if pooled
        if (data.pooled) bal.w = bal.w0 :+ bal.w
        // rescale to size of control group
        c = quadsum(bal.w0) / quadsum(bal.w)
        bal.w = bal.w * c
    }
    else {
        // add one if pooled
        if (data.pooled) bal.w = 1 :+ bal.w
        // rescale to size of control group
        c = rows(bal.w) / quadsum(bal.w)
        bal.w = bal.w * c
    }
    if (data.nose==0) bal.wb = bal.wb * c
}

void _rd_getadj(`Gadj' adj, `SS' lnm)
{
    `SR'    ADJ
    
    ADJ          = tokens(st_local(lnm))
    adj.true     = (length(ADJ)!=0)
    adj.location = anyof(ADJ, "location")
    adj.scale    = anyof(ADJ, "scale")
    adj.shape    = anyof(ADJ, "shape")
}

void _rd_adjust(`Data' data)
{
    `Int' get // 0: none, 1: location, 2: scale and location
    
    // step 1: prepare data
    get = 0
    _rd_adjust_check(data.D.adj, get)
    _rd_adjust_check(data.R.adj, get)
    if (get) {
        _rd_adjust_prep(data.D, data.adj, get)
        _rd_adjust_prep(data.R, data.adj, get)
        if (data.adj.link==2) {
            if ((data.R.l/data.D.l)<=0 | (data.R.l/data.D.l)>=.) {
                display("{err}invalid location adjustment factor; data not suitable for multiplicative adjustment")
                exit(499)
            }
        }
    }
    // step 2: obtain adjusted distributions
    _rd_adjust_get(data.Y1, data.G1, data.D, data.R, data.adj.link)
    _rd_adjust_get(data.Y0, data.G0, data.R, data.D, data.adj.link)
}

void _rd_adjust_check(`Gadj' adj, `Int' get)
{
    if (!adj.true)  return
    if (get==2)     return
    if (adj.shape) {
        if (adj.scale==0)         get = 2
        else if (adj.location==0) get = 1
    }
    else {
        if (adj.scale)   get = 2 
        else if (get==0) get = 1
    }
}

void _rd_adjust_prep(`Grp' G, `Adj' adj, `Int' get)
{
    if (adj.link==1) {
        G.lny = ln(G.y)
        if (adj.mean)   G.l = _rd_adjust_mean(G, G.lny)
        else            G.l = _rd_adjust_median(G, G.lny)
        if (get>1) {
            if (adj.sd) G.s = _rd_adjust_sd(G, G.lny)
            else        G.s = _rd_adjust_iqrange(G, G.lny)
        }
        return
    }
    if (adj.mean)       G.l = _rd_adjust_mean(G, G.y)
    else                G.l = _rd_adjust_median(G, G.y)
    if (get>1) {
        if (adj.sd)     G.s = _rd_adjust_sd(G, G.y)
        else            G.s = _rd_adjust_iqrange(G, G.y)
    }
}

`RS' _rd_adjust_mean(`Grp' G, `RC' y)
{
    `RS' l
    
    l = mean(y, G.w)
    if (G.nose) return(l)
    G.IFl = (y :- l) / G.W
    return(l)
}

`RS' _rd_adjust_median(`Grp' G, `RC' y)
{
    `RS' l, fx
    `RC' z
    
    l = _mm_median(y, G.w)
    if (G.nose) return(l)
    fx = _rd_kdens(y, G.w, G.wtype>=2, l, "exact") 
    z = (y :<= l)
    G.IFl = (mean(z, G.w) :- z) / (G.W * fx)
        // using mean(z) instead of .5 ensures that sum(IF)=0
    return(l)
}

`RS' _rd_adjust_sd(`Grp' G, `RC' y)
{
    `RS' v, s, m, c
    
    if (rows(y)==1) {
        s = 0
        if (G.nose) return(s)
        G.IFs = J(rows(y), 1, 0)
        return(s)
    }
    v = variance(y, G.w)
    if (G.wtype>=2) {
        v = v * (G.W-1) / (G.W - G.W/G.N)
    }
    s = sqrt(v)
    if (G.nose) return(s)
    m = mean(y, G.w)
    if (G.wtype>=2) c = 1 / (1 - 1/G.N)
    else            c = 1 / (1 - 1/G.W)
    G.IFs = (c:*(y :- m):^2 :- v) / (2 * s * G.W)
    return(s)
}

`RS' _rd_adjust_iqrange(`Grp' G, `RC' y)
{
    `RC' q, fx
    `RC' z1, z2
    
    q = _mm_quantile(y, G.w, (.25 \ .75), 2)
    if (G.nose) return(q[2]-q[1])
    fx = _rd_kdens(y, G.w, G.wtype>=2, q, "exact")
    z1 = (y :<= q[1])
    z2 = (y :<= q[2])
    G.IFs = ((mean(z2, G.w) :- z2)/fx[2] - (mean(z1, G.w) :- z1)/fx[1]) / G.W
        // using mean(z) instead of .75 and .25 ensures that sum(IF)=0
    return(q[2]-q[1])
}

void _rd_adjust_get(`PSRC' Y, `PGRP' G, `Grp' D, `Grp' R, `Int' link)
{
    if (!D.adj.true) {
        Y = &D.y; G = &D
        return
    }
    if (D.adj.shape) {
        if (D.adj.location & D.adj.scale) Y = &R.y
        else if (D.adj.location)          Y = _rd_adjust_s(R, D, link)
        else if (D.adj.scale)             Y = _rd_adjust_l(R, D, link)
        else                              Y = _rd_adjust_ls(R, D, link)
        G = &R
        return
    }
    if (D.adj.location & D.adj.scale)     Y = _rd_adjust_ls(D, R, link)
    else if (D.adj.location)              Y = _rd_adjust_l(D, R, link)
    else if (D.adj.scale)                 Y = _rd_adjust_s(D, R, link)
    G = &D
}

`PSRC' _rd_adjust_ls(`Grp' D, `Grp' R, `Int' link) // scale and location adjustment
{
    if (link==1) return(&(exp((D.lny :- D.l) * (R.s / D.s) :+ R.l)))
    if (link==2) return(&(D.y * (R.l / D.l))) // multiplicative; scale not relevant
                 return(&((D.y :- D.l) * (R.s / D.s) :+ R.l))
}

`PSRC' _rd_adjust_l(`Grp' D, `Grp' R, `Int' link) // location adjustment
{
    if (link==1) return(&(exp(D.lny :+ (R.l - D.l))))
    if (link==2) return(&(D.y * (R.l / D.l)))
                 return(&(D.y :+ (R.l - D.l)))
}

`PSRC' _rd_adjust_s(`Grp' D, `Grp' R, `Int' link) // scale adjustment
{
    if (link==1) return(&(exp((D.lny :- D.l) * (R.s / D.s) :+ D.l)))
    if (link==2) return(&D.y) // multiplicative; scale not relevant
                 return(&((D.y :- D.l) * (R.s / D.s) :+ D.l))
}

`Int' rd_get_at(`Data' data, `RC' at, `RC' atx, `Int' n)
{
    `SS'  _at, _AT0, _atx, _atx2, _ATX0
    `Int' method
    
    // setup
    _at   = st_local("at")
    _AT0  = st_local("AT0")
    _atx  = st_local("atx")
    _atx2 = st_local("atx2")
    _ATX0 = st_local("ATX0")
    method = (_atx!="") +
             (_atx=="comparison" | _atx=="reference" | _atx2!="" | _ATX0!="") +
             3*(st_local("discrete")!="") +
             3*(st_local("categorical")!="")
             // 0 continuous:  at
             // 1 continuous:  full atx
             // 2 continuous:  custom atx
             // 3 discrete:    at
             // 4 discrete:    full atx
             // 5 discrete:    custom atx
             // 6 categorical: at
             // 7 categorical: full atx
             // 8 categorical: custom atx
    
    // grid based on outcome values
    if (mod(method,3)==1) {
        atx = _rd_get_atx(data, "")
        at = _mm_relrank(*data.Y0, data.G0->w, atx)
    }
    else if (mod(method,3)==2) {
        if      (_atx!="atx") atx = _rd_get_atx(data, _atx)
        else if (_ATX0!="")   atx = _rd_get_at_mat(_ATX0)
        else                  atx = strtoreal(tokens(_atx2))'
        at = _mm_relrank(*data.Y0, data.G0->w, atx)
    }
    // grid based on probabilities
    else {
        if (_at!="")       at = strtoreal(tokens(_at))'
        else if (_AT0!="") at = _rd_get_at_mat(_AT0)
        else               at = (0::n-1) / (n-1)
        atx = _mm_quantile(*data.Y0, data.G0->w, at, 1)
        if (at[1]==0) {
            if (min(*data.Y1)<atx[1]) {
                // set origin to infimum (i.e. the largest observed value below
                // the range of Y0)
                atx[1] = max(select(*data.Y1, *data.Y1:<atx[1]))
            }
        }
    }
    // set n if not defined yet
    if (n>=.) {
        n  = rows(atx)
        st_local("n", strofreal(n, "%18.0g"))
    }
    // check whether n is too large
    _rd_get_at_check_n(n)
    
    // done
    return(method)
}

`RC' _rd_get_atx(`Data' data, `SS' atx)
{
    if (atx=="comparison") return(_mm_unique(*data.Y1))
    if (atx=="reference")  return(_mm_unique(*data.Y0))
    return(mm_unique(_mm_unique(*data.Y1) \ _mm_unique(*data.Y0)))
}

void _rd_get_at_check_n(`Int' n0)
{
    `Int' n, nhist
    
    // histogram points will be added to e(b) and must be taken into account
    nhist = strtoreal(st_local("nhist"))
    if (nhist<.) n = n0 + nhist
    else         n = n0
    
    // check whether too large
    if (n>=st_numscalar("c(max_matsize)")) {
        printf("{err}too many evaluation points;"
            + " must be smaller than c(max_matsize) = {bf:%g}\n"
            + "(number of evaluation points" 
            + (nhist<. ? " including histogram bins" : "")
            + " is %g)\n"
            , st_numscalar("c(max_matsize)"), n)
        exit(499)
    }
}

`RC' _rd_get_at_mat(`SS' nm)
{
    `RM' at
    
    at = st_matrix(nm)'
    if (cols(at)>rows(at)) at = at'
    if (cols(at)>1) at = at[,1]
    return(mm_unique(at))
}

void rd_relrank(`Data' data)
{
    data.ranks = _mm_relrank(*data.Y0, data.G0->w,
                             *data.Y1, data.mid, 0, data.tbreak, data.G1->w)
}

void rd_ogrid(`Data' data, `Bool' cdf)
{
    `Int' n
    `RC'  b
    `SM'  cstripe
    
    n = strtoreal(st_local("ogrid"))
    // check whether n is too large
    if (n<.) {
        cstripe = J(n,1,""), "q":+strofreal(1::n)
        if (cdf) b = _rd_ogrid(n, data, 0), _rd_ogrid(n, data, 1)
        else     b = _rd_ogrid(n, data, 0)
        st_matrix(st_local("OGRID"), b')
        st_matrixcolstripe(st_local("OGRID"), cstripe)
    }
}

`RC' _rd_ogrid(`Int' n, `Data' data, `Bool' grp)
{
    if (grp) return(_mm_quantile(*data.Y1, data.G1->w, (0::n-1)/(n-1), 1))
             return(_mm_quantile(*data.Y0, data.G0->w, (0::n-1)/(n-1), 1))
}

`RC' _rd_kdens(`RC' x, `RC' w, `RC' pw, `RC' at, | `SS' exact)
{
    `PDF' S
    
    if (mm_isconstant(x)) return(x[1]:==at)
    S.data(x, w, pw, 1)                     // x is sorted
    S.bw(st_local("vcebwidth")!="" 
        ? (st_local("vcebwtype")=="scalar" 
            ? st_numscalar(st_local("vcebwidth"))
            : (st_local("vcebwtype")=="matrix"
                ? st_matrix(st_local("vcebwidth"))[1,1] 
                : strtoreal(st_local("vcebwidth"))
                )
            )
        : st_local("vcebwmethod"), 
        strtoreal(st_local("vcebwadjust")),
        strtoreal(st_local("vcebwdpi")),
        "quietly"!="")  // suppress bw-selection failure note
    S.kernel(st_local("vcekernel"), strtoreal(st_local("vceadaptive")))
    S.n(strtoreal(st_local("vcenapprox")))
    //S.support(., st_local("vceboundary")=="lc" ? "linear" : st_local("vceboundary"))
    // => add option to vce() to specify support of data?
    if (args()<5) exact = st_local("vceexact")
    return(S.d(at, exact!=""))
}

void _rd_IF_init(`Data' data, `Int' n)
{
    data.D.IF = J(data.D.N, n, 0)
    data.R.IF = J(data.R.N, n, 0)
}

void _rd_IF(`Grp' G, `Int' j, `RC' h)
{   // append contribution to IF
     G.IF[,j] = G.IF[,j] + h
}

void _rd_IF_store(`Data' data, `Int' n, | `Bool' append)
{
    `IntR' idx
    
    idx = _rd_IF_store_idx(n, (args()<3 ? `FALSE' : append))
    if      (data.D.bal) _rd_IF_store_bal(idx, data.D, data.bal)
    else if (data.R.bal) _rd_IF_store_bal(idx, data.R, data.bal)
    st_store(., idx, data.D.touse, st_data(., idx, data.D.touse) + data.D.IF[data.D.p,])
    st_store(., idx, data.R.touse, st_data(., idx, data.R.touse) + data.R.IF[data.R.p,])
}

`IntR' _rd_IF_store_idx(`Int' n, `Bool' append)
{
    `IntR' idx
    `SR'   vnm
    `Int'  i

    if (append | st_local("IFs")=="") {
        vnm = st_tempname(n)
        idx = st_addvar("double", vnm)
        for (i=1;i<=n;i++) stata(sprintf("qui replace %s = 0 if %s", // fillin zeros
            vnm[i], st_local("TOUSE")!="" ? st_local("TOUSE") : st_local("touse")))
        if (st_local("IFs")!="") vnm = (st_local("IFs"), vnm)
        st_local("IFs", invtokens(vnm))
    }
    else idx = st_varindex(tokens(st_local("IFs")))
    return(idx)
}

void _rd_IF_store_bal(`IntR' idx, `Grp' G, `Bal' bal)
{
    `Int' j
    `RR'  delta
    
    j = length(idx)
    for (;j;j--) {
        delta = quadcolsum(bal.wb :* G.IF[,j] :* bal.Z)'
        G.IF[,j] = bal.w :* G.IF[,j] + bal.IFZ * delta
        st_store(., idx[j], bal.T, bal.T_IFZ * delta)
    }
}

`RC' _rd_IF_x_to_y(`RC' y, `Grp' D, `Grp' R, `Int' link)
{
    if (link==2) return(__rd_IF_x_to_y(y, D, R, 1))           // multiplicative
    if (link==1) return(exp(__rd_IF_x_to_y(ln(y), D, R, 0)))  // logarithmic
    return(__rd_IF_x_to_y(y, D, R, 0))                        // additive
}

`RC' __rd_IF_x_to_y(`RC' y, `Grp' D, `Grp' R, `Bool' mult)
{
    `BoolR' aD, aR
    `RC'    x
    
    // adjustment settings
    aD = (D.adj.location, D.adj.scale, D.adj.shape)
    aR = (R.adj.location, R.adj.scale, R.adj.shape)
    // no adjustment
    if (aD==(0,0,0) & aR==(0,0,0)) return(y)
    // multiplicative adjustment
    if (mult) {
        // loc:<none> | <none>:loc
        if ((aD==(1,0,0) & aR==(0,0,0)) | (aD==(0,0,0) & aR==(1,0,0)))
            x = y * (R.l / D.l)
        // shape:<none> | <none>:shape
        else if ((aD==(0,0,1) & aR==(0,0,0)) | (aD==(0,0,0) & aR==(0,0,1)))
            x = y * (D.l / R.l)
        else _error(3498) // not reached
        return(x)
    }
    // linear adjustment
    // loc:<none> | <none>:loc
    if ((aD==(1,0,0) & aR==(0,0,0)) | (aD==(0,0,0) & aR==(1,0,0)))
        x = y :+ (R.l - D.l)
    // loc+scale:<none> | <none>:loc+scale | loc:scale | scale:loc
    else if ((aD==(1,1,0) & aR==(0,0,0)) | (aD==(0,0,0) & aR==(1,1,0)) |
             (aD==(1,0,0) & aR==(0,1,0)) | (aD==(0,1,0) & aR==(1,0,0)))
        x = (y :- D.l) * (R.s/D.s) :+ R.l
    // scale:<none>
    else if (aD==(0,1,0) & aR==(0,0,0))
        x = (y :- D.l) * (R.s/D.s) :+ D.l
    // <none>:scale
    else if (aD==(0,0,0) & aR==(0,1,0))
        x = (y :- R.l) * (R.s/D.s) :+ R.l
    // shape:<none> | <none>:shape
    else if ((aD==(0,0,1) & aR==(0,0,0)) | (aD==(0,0,0) & aR==(0,0,1)))
        x = (y :- R.l) * (D.s/R.s) :+ D.l
    // scale+shape:<none> | <none>:scale+shape
    else if ((aD==(0,1,1) & aR==(0,0,0)) | (aD==(0,0,0) & aR==(0,1,1)))
        x = y :+ (D.l - R.l)
    // loc+shape:<none> | shape:loc
    else if ((aD==(1,0,1) & aR==(0,0,0)) | (aD==(0,0,1) & aR==(1,0,0)))
        x = (y :- R.l) * (D.s/R.s) :+ R.l
    // <none>:loc+shape | loc:shape
    else if ((aD==(0,0,0) & aR==(1,0,1)) | (aD==(1,0,0) & aR==(0,0,1)))
        x = (y :- D.l) * (D.s/R.s) :+ D.l
    // shape:scale
    else if (aD==(0,0,1) & aR==(0,1,0))
        x = y :+ ((D.l-R.l) * R.s/D.s)
    // scale:shape
    else if (aD==(0,1,0) & aR==(0,0,1))
        x = y :+ ((D.l-R.l) * D.s/R.s)
    else _error(3498) // not reached
    return(x)
}

/* PDF estimation -----------------------------------------------------------*/

void rd_PDF(`Int' n)
{
    `RC'    at, atx
    `Int'   method
    `Data'  data
    pragma unset at
    pragma unset atx
    
    // prepare data
    rd_getdata(data)
    
    // evaluation grid
    method = rd_get_at(data, at, atx, n)
    
    // estimation
    if (method>=3) rd_PDFd(data, n, at, atx, method) // discrete
    else           rd_PDFc(data, n, at, atx, method) // continuous
    
    // outcome grid
    rd_ogrid(data, 0)
}

void rd_PDFd(`Data' data, `Int' n, `RC' at, `RC' atx, `Int' method)
{
    `RC'  p1, p0
    `SM'  cstripe
    pragma unset p1
    pragma unset p0
    
    // estimation
    if (mod(method,3)==1) _rd_PDFd(p1, p0, *data.Y1, data.G1->w, atx, at)
    else                  _rd_PDFd_map(p1, p0, data, at, atx, method)
    
    // remove evaluation points based on x-values that have zero frequency in
    // reference distribution
    if (mod(method,3)) {
        if (anyof(p0,0)) _rd_PDFd_rm(p1, p0, at, atx, n)
    }
    
    // return results
    if (mod(method,3)) {    // atx
        if (method>5)       // categorical
             cstripe = (J(n,1,""), strofreal(atx):+("."+st_varname(data.D.yvar)))
        else cstripe = (J(n,1,""), "x":+strofreal(1::n))
    }
    else cstripe = (J(n,1,""), "p":+strofreal(1::n))
    st_matrix(st_local("b"), (p1 :/ p0)')
    st_matrixcolstripe(st_local("b"), cstripe)
    st_matrix(st_local("AT"), (at, atx)')
    st_matrixcolstripe(st_local("AT"), cstripe)
    st_matrixrowstripe(st_local("AT"), (J(2,1,""), ("p" \ "x")))
    
    // compute influence functions
    if (data.nose==0) {
        _rd_PDFd_IF(p1, p0, atx, n, data)
        _rd_IF_store(data, n)
    }
}

void _rd_PDFd_map(`RC' P1, `RC' P0, `Data' data, `RC' at, `RC' atx, `Int' method)
{
    `Int'  i, j, n, r
    `RC'   p1, p0, cdf, x
    pragma unset p1
    pragma unset p0
    
    // compute relative density at observed values
    x   = mm_unique(_mm_unique(*data.Y1) \ _mm_unique(*data.Y0))
    cdf = _mm_relrank(*data.Y0, data.G0->w, x)
    _rd_PDFd(p1, p0, *data.Y1, data.G1->w, x, cdf)
    
    // map rd to evaluation grid
    n = rows(at)
    P1 = P0 = J(n, 1, 0)
    j = 1
    // - mat to at()
    if (mod(method,3)==0) {
        // first get rid of points that do not exist in refdist and were 
        // only included to get the computations for the other points right
        cdf = select(cdf, p0:!=0)
        p1  = select(p1,  p0:!=0)
        p0  = select(p0,  p0:!=0)
        r = rows(p0)
        for (i=1; i<=n; i++) {
            for  (; j<r; j++) {
                if (cdf[j]>=at[i]) break
            }
            P1[i] = p1[j]
            P0[i] = p0[j]
        }
        return
    }
    // - map to atx()
    r = rows(p0)
    for (i=1; i<=n; i++) {
        for  (; j<r; j++) {
            if (x[j]>=atx[i]) break
        }
        if (x[j]==atx[i]) { // only fill in if exact match
            P1[i] = p1[j]
            P0[i] = p0[j]
        }
    }
}

void _rd_PDFd(`RC' p1, `RC' p0, `RC' Y, `RC' w, `RC' atx, `RC' at)
{
    p1 = _mm_relrank(Y, w, atx)
    p1 = mm_diff(0\p1)
    p0 = mm_diff(0\at)
}

void _rd_PDFd_rm(`RC' p1, `RC' p0, `RC' at, `RC' atx, `Int' n)
{
    `Int'  i, j
    `IntC' p
    
    p = J(n,1,.)
    j = 0
    for (i=1;i<=n;i++) {
        if (p0[i]==0) {
            printf("{txt}(x = %g omitted due to zero frequency in reference distribution)\n", atx[i])
            continue
        }
        p[++j] = i
    }
    if (j==0) {
        display("{err}no remaining evaluation points")
        exit(499)
    }
    p = p[|1\j|]
    p1 = p1[p]; p0 = p0[p]; at = at[p]; atx = atx[p]; n = rows(p)
    st_local("n", strofreal(n, "%18.0g"))
}

void _rd_PDFd_IF(`RC' p1, `RC' p0, `RC' atx, `Int' n, `Data' data)
{
    `Int' j
    
    _rd_IF_init(data, n)
    for (j=1;j<=n;j++) {
        _rd_IF(data.D, j, 1/data.D.W * 1/p0[j] * ((data.D.y:==atx[j]) :- p1[j]))
        _rd_IF(data.R, j, 1/data.R.W * p1[j]/(p0[j]^2) * (p0[j] :- (data.R.y:==atx[j])))
    }
}

void rd_PDFc(`Data' data, `Int' n, `RC' at, `RC' atx, `Int' method)
{
    `RS'  h
    `RC'  b
    `SM'  cstripe
    pragma unset h // will be set by _rd_PDFc()
    
    // density estimation
    b = _rd_PDFc(n, data, at, h)  // also fills in IFs
    
    // return results
    if (st_local("nhist")!="") cstripe = J(n, 1, "pdf")
    else                       cstripe = J(n, 1, "")
    if (method)  cstripe = cstripe, "x" :+ strofreal(1::n)
    else         cstripe = cstripe, "p" :+ strofreal(1::n)
    st_matrix(st_local("b"), b')
    st_matrixcolstripe(st_local("b"), cstripe)
    st_matrix(st_local("AT"), (at, atx)')
    st_matrixcolstripe(st_local("AT"), cstripe)
    st_matrixrowstripe(st_local("AT"), (J(2,1,""), ("p" \ "x")))
    st_numscalar(st_local("BW"), h)

    // store influence functions
    if (data.nose==0) _rd_IF_store(data, n)
    
    // append histogram
    if (st_local("nhist")!="") {
        b = _rd_HIST(strtoreal(st_local("nhist")), data, at, atx) // also computes IFs
        cstripe = cstripe \ (J(rows(b),1,"histogram"), "h":+strofreal(1::rows(b)))
        st_matrix(st_local("b"), (st_matrix(st_local("b"))' \ b)')
        st_matrixcolstripe(st_local("b"), cstripe)
        st_matrix(st_local("AT"), (st_matrix(st_local("AT"))' \ (at, atx))')
        st_matrixcolstripe(st_local("AT"), cstripe)
        // store influence functions
        if (data.nose==0) _rd_IF_store(data, length(b), "append"!="")
    }
}

`RC' _rd_PDFc(`Int' n, `Data' data, `RC' at, `RS' h)
{
    `RC'  b
    `PDF' S
    
    // compute relative ranks
    rd_relrank(data)
    
    // density estimation
    S.data(data.ranks, data.G1->w, data.wtype>=2, 1)
    S.bw(st_local("bwidth")!="" ? 
            (st_local("bwtype")=="scalar" ? 
                st_numscalar(st_local("bwidth")) : 
                strtoreal(st_local("bwidth"))) :
            st_local("bwmethod"), 
        strtoreal(st_local("bwadjust")), 
        strtoreal(st_local("bwdpi")))
    S.kernel(st_local("kernel"), strtoreal(st_local("adaptive")))
    S.support((0,1), st_local("boundary")=="lc" ? "linear" : st_local("boundary"),
        st_local("bwnord")=="")
    S.n(strtoreal(st_local("napprox")))
    if (S.h()>=.) {
        display("{txt}(bandwidth estimation failed; setting bandwidth to 0.1)")
        S.bw(.1 * S.kh() / mm_kdel0_gaussian())
    }
    b = S.d(at, st_local("exact")!="")
    
    // influence functions
    if (data.nose==0) _rd_PDFc_IF(b, at, n, S, data)
    
    // return results
    h = S.h() // bandwidth
    return(b) // density
}

void _rd_PDFc_IF(`RC' b, `RC' at, `Int' n, `PDF' S, `Data' data)
{
    `Int' j
    `RC'  z, h, fR
    `RM'  cdf
    pragma unused b
    
    // initialize IFs
    _rd_IF_init(data, n)
    
    // CDF of reference distribution at unique values
    cdf = _mm_ecdf2(*data.Y0, data.G0->w)[,2]
    cdf = cdf, mm_diff(0 \ cdf) // CDF steps in second column
    
    // compute reference distribution densities if adjust()
    if (data.adj.link==1) {
        fR = _rd_kdens(data.G0->lny, data.G0->w, data.wtype>=2, 
            __rd_IF_x_to_y(data.G1->lny, data.D, data.R, 0))
    }
    else if (data.adj.true) {
        fR = _rd_kdens(data.G0->y, data.G0->w, data.wtype>=2, 
            __rd_IF_x_to_y(data.G1->y, data.D, data.R, data.adj.link==2))
    }
    
    // compute IFs
    h = S.h() * S.l()
    for (j=1;j<=n;j++) {
        // first part of IF
        z = S.K(S.X(), at[j], h, 1)
        _rd_IF(*data.G1, j, (z :- mean(z, data.G1->w))/data.G1->W)
            // using mean(z) instead of b[j] ensures that sum(IF)=0
        // second part of IF
        _rd_PDFc_IF2(data, j, cdf, fR, 
            /*delta = */data.G1->w/data.G1->W :* S.Kd(S.X(), at[j], h, 1))
    }
}

void _rd_PDFc_IF2(`Data' data, `Int' j, `RM' cdf, `RC' fR, `RC' delta)
{
    _rd_IF(*data.G0, j, 
        (_rd_PDFc_IF2_lambda(*data.Y0, cdf, *data.Y1, data.ranks, delta) 
         :- sum(delta :* data.ranks)) / data.G0->W)
    if (!data.adj.true) return
    _rd_PDFc_IF2_adj(data, data.D, data.R, j, delta :* fR)
}

`RC' _rd_PDFc_IF2_lambda(`RC' y, `RM' cdf, `RC' x, `RC' r, `RC' d)
{
    `Int' i, j, j1, k
    `RS'  yi
    `RC'  S, D
    
    // setup
    j = rows(d)
    D = mm_colrunsum(d[j::1], 0, 1)[j::1] // reverse running sum of d
    D = D \ 0 // set D[rows(d)+1] to zero
    i = rows(y)
    S = J(i, 1, 0)
    k = rows(cdf)
    // skip observations for which y>max(x)
    while (i) {
        yi = y[i]
        if (yi<=x[j]) break
        i--
        for (;i;i--) {
            if (y[i]!=yi) break
        }
        k-- // move to next level of y
    }
    // fill in S for remaining observations
    while (i) {
        yi = y[i]
        // find smallest x that is at least as large as yi, using uppermost
        // observation if x==yi and x has duplicates
        for (;j>1;j--) {
            if (x[j-1]<yi) break
            if (x[j]==yi) break
        }
        // case 1: target x is between two y-values (no exact match)
        if (x[j]!=yi) {
            S[i] = D[j]
        }
        // case 2: target x has an exact match in y; must handle 
        // mid-distribution adjustments and breaking of ties
        else {
            S[i] = D[j+1]
            j1 = j
            for (; j>1; j--) {
                // find duplicates in x
                if (x[j-1]!=yi) break
            }
            if (j==j1) {
                // case 2a: just a single x
                S[i] = S[i] + d[j] * (1-(cdf[k,1]-r[j])/cdf[k,2])
            }
            else {
                // case 2b: multiple x; use range subscripts
                S[i] = S[i] + sum(d[|j\j1|] :* (1:-(cdf[k,1]:-r[|j\j1|])/cdf[k,2]))
            }
        }
        // process duplicates of yi
        i--
        for (;i;i--) {
            if (y[i]!=yi) break
            S[i] = S[i+1]
        }
        k-- // move to next level of y
    }
    return(S)
}

void _rd_PDFc_IF2_adj(`Data' data, `Grp' D, `Grp' R, `Int' j, `RC' dfR)
{
    `BoolR' aD, aR
    `RC'    y
    `RC'    tau
    
    // adjustment settings
    aD = (D.adj.location, D.adj.scale, D.adj.shape)
    aR = (R.adj.location, R.adj.scale, R.adj.shape)
    if (data.adj.link==1) y = data.G1->lny
    else                  y = data.G1->y
    // multiplicative adjustment
    if (data.adj.link==2) {
        // loc:<none> | <none>:loc
        if ((aD==(1,0,0) & aR==(0,0,0)) | (aD==(0,0,0) & aR==(1,0,0))) {
            tau = sum(dfR :* y) / D.l
            _rd_IF(D, j, -tau * R.l/D.l * D.IFl)
            _rd_IF(R, j,  tau           * R.IFl)
        }
        // shape:<none> | <none>:shape
        else if ((aD==(0,0,1) & aR==(0,0,0)) | (aD==(0,0,0) & aR==(0,0,1))) {
            tau = sum(dfR :* y) / R.l
            _rd_IF(D, j,  tau           * D.IFl)
            _rd_IF(R, j, -tau * D.l/R.l * R.IFl)
        }
        else _error(3498) // not reached
    }
    // linear t(y) or logarithmic adjustment exp(t(ln y)
    else {
        // loc:<none> | <none>:loc
        if ((aD==(1,0,0) & aR==(0,0,0)) | (aD==(0,0,0) & aR==(1,0,0))) {
            tau = sum(dfR)
            _rd_IF(D, j, -tau * D.IFl)
            _rd_IF(R, j,  tau * R.IFl)
        }
        // loc+scale:<none> | <none>:loc+scale | loc:scale | scale:loc
        else if ((aD==(1,1,0) & aR==(0,0,0)) | (aD==(0,0,0) & aR==(1,1,0)) |
                 (aD==(1,0,0) & aR==(0,1,0)) | (aD==(0,1,0) & aR==(1,0,0))) {
            tau = sum(dfR)
            _rd_IF(D, j, -tau * R.s/D.s * D.IFl)
            _rd_IF(R, j,  tau           * R.IFl)
            tau = sum(dfR :* (y :- D.l)) / D.s
            _rd_IF(D, j, -tau * R.s/D.s * D.IFs)
            _rd_IF(R, j,  tau           * R.IFs)
        }
        // scale:<none>
        else if (aD==(0,1,0) & aR==(0,0,0)) {
            _rd_IF(D, j, sum(dfR) * (1-R.s/D.s) * D.IFl)
            tau = sum(dfR :* (y :- D.l)) / D.s
            _rd_IF(D, j, -tau * R.s/D.s * D.IFs)
            _rd_IF(R, j,  tau           * R.IFs)
        }
        // <none>:scale
        else if (aD==(0,0,0) & aR==(0,1,0)) {
            _rd_IF(R, j, sum(dfR) * (1-R.s/D.s) * R.IFl)
            tau = sum(dfR :* (y :- R.l)) / D.s
            _rd_IF(D, j, -tau * R.s/D.s * D.IFs)
            _rd_IF(R, j,  tau           * R.IFs)
        }
        // shape:<none> | <none>:shape
        else if ((aD==(0,0,1) & aR==(0,0,0)) | (aD==(0,0,0) & aR==(0,0,1))) {
            tau = sum(dfR)
            _rd_IF(D, j,  tau           * D.IFl)
            _rd_IF(R, j, -tau * D.s/R.s * R.IFl)
            tau = sum(dfR :* (y :- R.l)) / R.s
            _rd_IF(D, j,  tau           * D.IFs)
            _rd_IF(R, j, -tau * D.s/R.s * R.IFs)
        }
        // scale+shape:<none> | <none>:scale+shape
        else if ((aD==(0,1,1) & aR==(0,0,0)) | (aD==(0,0,0) & aR==(0,1,1))) {
            tau = sum(dfR)
            _rd_IF(D, j,  tau * D.IFl)
            _rd_IF(R, j, -tau * R.IFl)
        }
        // loc+shape:<none> | shape:loc
        else if ((aD==(1,0,1) & aR==(0,0,0)) | (aD==(0,0,1) & aR==(1,0,0))) {
            _rd_IF(R, j, sum(dfR) * (1-D.s/R.s) * R.IFl)
            tau = sum(dfR :* (y :- R.l)) / R.s
            _rd_IF(D, j,  tau           * D.IFs)
            _rd_IF(R, j, -tau * D.s/R.s * R.IFs)
        }
        // <none>:loc+shape | loc:shape
        else if ((aD==(0,0,0) & aR==(1,0,1)) | (aD==(1,0,0) & aR==(0,0,1))) {
            _rd_IF(D, j, sum(dfR) * (1-D.s/R.s) * D.IFl)
            tau = sum(dfR :* (y :- D.l)) / R.s
            _rd_IF(D, j,  tau           * D.IFs)
            _rd_IF(R, j, -tau * D.s/R.s * R.IFs)
        }
        // shape:scale
        else if (aD==(0,0,1) & aR==(0,1,0)) {
            tau = sum(dfR)
            _rd_IF(D, j,  tau * R.s/D.s * D.IFl)
            _rd_IF(R, j, -tau * R.s/D.s * R.IFl)
            _rd_IF(D, j, -tau * (D.l-R.l)*R.s/D.s^2 * D.IFs)
            _rd_IF(R, j,  tau * (D.l-R.l)/D.s       * R.IFs)
        }
        // scale:shape
        else if (aD==(0,1,0) & aR==(0,0,1)) {
            tau = sum(dfR)
            _rd_IF(D, j,  tau * D.s/R.s * D.IFl)
            _rd_IF(R, j, -tau * D.s/R.s * R.IFl)
            _rd_IF(D, j,  tau * (D.l-R.l)/R.s       * D.IFs)
            _rd_IF(R, j, -tau * (D.l-R.l)*D.s/R.s^2 * R.IFs)
        }
        else _error(3498) // not reached
    }
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
    // estimation
    b = _rd_HIST(n, data, at, atx)  // also fills in IFs
    
    // return results
    cstripe = J(n,1,""), "h":+strofreal(1::n)
    st_matrix(st_local("b"), b')
    st_matrixcolstripe(st_local("b"), cstripe)
    st_matrix(st_local("AT"), (at, atx)')
    st_matrixcolstripe(st_local("AT"), cstripe)
    st_matrixrowstripe(st_local("AT"), (J(2,1,""), ("p" \ "x")))
    
    // store influence functions
    if (data.nose==0) _rd_IF_store(data, n)
    
    // outcome grid
    rd_ogrid(data, 0)
}

`RC' _rd_HIST(`Int' n, `Data' data, | `RC' at, `RC' atx)
{   // fills in at and atx if specified
    `Int'  i
    `RC'   b
    `RM'   IFD, IFR
    
    // obtain CDF at thresholds
    at = (1::n-1) / n
    if (st_local("alt")!="") b = _rd_CDF_alt(data, at)
    else                     b = _rd_CDF_ipolate(data, at)
    
    // obtain IFs of CDF at thresholds
    if (data.nose==0) {
        _rd_CDF_IF(b, at, n-1, data, data.D, data.R, *data.G1, *data.G0)
    }
    // compute density of histogram bins from CDF
    b = ((b\1) :- (0\b)) * n
    // compute IFs
    if (data.nose==0) {
        IFD = n * data.D.IF
        IFR = n * data.R.IF
        data.D.IF = J(rows(IFD), n, 0)
        data.R.IF = J(rows(IFR), n, 0)
        for (i=1;i<=n;i++) {
            if (i==1) {
                data.D.IF[,i] = IFD[,i]
                data.R.IF[,i] = IFR[,i]
            }
            else if (i==n) {
                data.D.IF[,i] = -IFD[,i-1]
                data.R.IF[,i] = -IFR[,i-1]
            }
            else {
                data.D.IF[,i] = IFD[,i] - IFD[,i-1]
                data.R.IF[,i] = IFR[,i] - IFR[,i-1]
            }
        }
    }
    // set at and atx to bin midpoints
    if (args()>2) {
        at = .5/n \ (at :+ .5/n)
        if (args()>3) atx = _mm_quantile(*data.Y0, data.G0->w, at, 1)
    }
    return(b)
}

/* CDF estimation -----------------------------------------------------------*/

void rd_CDF(`Int' n)
{
    `RC'   b, at, atx
    `Int'  method
    `SM'   cstripe
    `Data' data
    pragma unset at
    pragma unset atx
    
    // prepare data
    rd_getdata(data)
    
    // evaluation grid
    method = rd_get_at(data, at, atx, n)
    
    // estimate
    if (st_local("alt")!="") b = _rd_CDF_alt(data, at)
    else {
        if (mod(method,3))   b = _mm_relrank(*data.Y1, data.G1->w, atx)
        else                 b = _rd_CDF_ipolate(data, at)
    }
    
    // return results
    if (mod(method,3)) {    // atx
        if (method>5)       // categorical
             cstripe = (J(n,1,""), strofreal(atx):+("."+st_varname(data.D.yvar)))
        else cstripe = (J(n,1,""), "x":+strofreal(1::n))
    }
    else cstripe = (J(n,1,""), "p":+strofreal(1::n))
    st_matrix(st_local("b"), b')
    st_matrixcolstripe(st_local("b"), cstripe)
    st_matrix(st_local("AT"), (at, atx)')
    st_matrixcolstripe(st_local("AT"), cstripe)
    st_matrixrowstripe(st_local("AT"), (J(2,1,""), ("p" \ "x")))
    
    // compute influence functions
    if (data.nose==0) {
        _rd_CDF_IF(b, at, n, data, data.D, data.R, *data.G1, *data.G0)
        _rd_IF_store(data, n)
    }
    
    // indicator for whether zero coordinate should be added in graph
    //    only if: - evaluation has been done at outcome values
    //             - first y-coordinate is not zero
    //             - if first evaluation point is not larger than min of Y
    if (mod(method,3)) {
        if (at[1]>0) {
            if (mod(method, 3)==1) st_local("origin", "origin")
            else if (atx[1]<=min(*data.Y1 \ *data.Y0)) st_local("origin", "origin")
        }
    }
    
    // outcome grid
    rd_ogrid(data, 1)
}

`RC' _rd_CDF_alt(`Data' data, `RC' at)
{
    `Int' i, j
    `RS'  ati
    `RC'  r, b
    `RM'  cdf
    
    // compute cdf of relative ranks
    data.tbreak = 0 // !!!
    data.mid    = 0 // !!!
    rd_relrank(data)
    cdf = _mm_ecdf2(data.ranks, data.G1->w)
    r = cdf[,1]; cdf = cdf[,2]
    
    // pad r and cdf if r does not include 0 or 1
    if (r[1]>0) {
        cdf = 0 \ cdf
        r   = 0 \ r
    }
    if (r[rows(r)]<1) {
        cdf = cdf \ 1
        r   = r   \ 1
    }
    
    // interpolate (note: r is unique)
    j = rows(r)
    i = rows(at)
    b = J(i,1,.)
    for (; i; i--) {
        ati = at[i]
        for (; j; j--) {
            if (r[j]<=ati) break
        }
        if (r[j]==ati) b[i] = cdf[j]
        else           b[i] = cdf[j] + (cdf[j+1]-cdf[j]) * (ati-r[j])/(r[j+1]-r[j])
    }
    return(b)
}

`RC' _rd_CDF_ipolate(`Data' data, `RC' at)
{
    `Int' i, j, k
    `RS'  ati
    `RC'  x, cdf0, cdf1, b
    
    // obtain exact cdf
    x    = mm_unique(_mm_unique(*data.Y1) \ _mm_unique(*data.Y0))
    cdf1 = 0 \ _mm_relrank(*data.Y1, data.G1->w, x)
    cdf0 = 0 \ _mm_relrank(*data.Y0, data.G0->w, x)

    // interpolate
    j = rows(cdf0)
    i = rows(at)
    b = J(i,1,.)
    for (; i; i--) {
        ati = at[i]
        for (; j; j--) {
            if (cdf0[j]<=ati) break
        }
        // if requested r hits a point in cdf0
        if (cdf0[j]==ati) {
            // case 1: r = 0
            //  => use ceiling of possible upright segment at origin
            if (ati==0) {
                b[i] = cdf1[j]
                continue
            }
            // check for upright segment
            for (k = j-1; k; k--) {
                if (cdf0[k]<ati) break
            }
            // case 2: no upright segment
            if ((++k)==j) {
                b[i] = cdf1[j]
                continue
            }
            // case 3: upright segment at r < 1
            //  => get midpoint of upright segment
            if (ati<1) b[i] = (cdf1[k] + cdf1[j])/2
            // case 4: upright segment at r = 0
            //  => use floor of upright segment
            else b[i] = cdf1[k]
            j = k
            continue
        }
        // if requested r is between two points of cdf0: interpolate
        b[i] = cdf1[j] + (cdf1[j+1]-cdf1[j]) * (ati-cdf0[j])/(cdf0[j+1]-cdf0[j])
    }
    return(b)
}

void _rd_CDF_IF(`RC' b, `RC' at, `Int' n, `Data' data, `Grp' D, `Grp' R, `Grp' G1, `Grp' G0)
{
    `Int' j
    `RC'  z, qD, qR, fD, fR
    pragma unused b
    
    // initialize IFs
    _rd_IF_init(data, n)
    
    // transform quantiles to raw scales and obtain density estimates
    qR = _mm_quantile(G0.y, G0.w, at)
    fR = _rd_kdens(G0.y, G0.w, data.wtype>=2, qR)
    qD = _rd_IF_x_to_y(qR, R, D, data.adj.link)
    fD = _rd_kdens(G1.y, G1.w, data.wtype>=2, qD)
    
    // compute IFs
    for (j=1;j<=n;j++) {
        z = (G1.y :<= qD[j])
        _rd_IF(G1, j, (z :- mean(z, G1.w)) / G1.W)
            // using mean(z) instead of b[j] ensures that sum(IF)=0
        z = (G0.y :<= qR[j])
        _rd_CDF_IF2(data, G0, D, R, j, qD[j], qR[j], fD[j],
            /*IF(q)=*/(mean(z, G0.w) :- z) / (fR[j]*G0.W))
            // using mean(z) instead of at[j] ensures that sum(IF)=0
    }
}

void _rd_CDF_IF2(`Data' data, `Grp' G0, `Grp' D, `Grp' R, `Int' j, `RS' qD, `RS' qR, 
    `RS' fD, `RC' IFq)
{
    `BoolR' aD, aR
    `RS'    q, f0, f1
    
    // no adjustment
    if (!data.adj.true) {
        _rd_IF(G0, j, fD * IFq)
        return
    }
    // adjustment settings
    aD = (D.adj.location, D.adj.scale, D.adj.shape)
    aR = (R.adj.location, R.adj.scale, R.adj.shape)
    // multiplicative adjustment
    if (data.adj.link==2) {
        // loc:<none> | <none>:loc
        //  t(q) = q * mu_D / mu_R
        if ((aD==(1,0,0) & aR==(0,0,0)) | (aD==(0,0,0) & aR==(1,0,0))) {
            _rd_IF(G0, j, fD * D.l/R.l         * IFq)
            _rd_IF( D, j, fD * qR/R.l          * D.IFl)
            _rd_IF( R, j, fD * -qR * D.l/R.l^2 * R.IFl)
        }
        // shape:<none> | <none>:shape
        //  t(q) = q * mu_R / mu_D
        else if ((aD==(0,0,1) & aR==(0,0,0)) | (aD==(0,0,0) & aR==(0,0,1))) {
            _rd_IF(G0, j, fD * R.l/D.l         * IFq)
            _rd_IF( D, j, fD * -qR * R.l/D.l^2 * D.IFl)
            _rd_IF( R, j, fD * qR/D.l          * R.IFl )
        }
        else _error(3498) // not reached
    }
    // linear t(q) or logarithmic adjustment exp(t(ln q)
    else {
        if (data.adj.link) {
            q = ln(qR)
            f0 = fD * qD/qR; f1 = fD * qD
        }
        else {
            q = qR
            f0 = f1 = fD
        }
        // loc:<none> | <none>:loc
        //  t(q) = q - mu_R + mu_D
        if ((aD==(1,0,0) & aR==(0,0,0)) | (aD==(0,0,0) & aR==(1,0,0))) {
            _rd_IF(G0, j,  f0 * IFq)
            _rd_IF( D, j,  f1 * D.IFl)
            _rd_IF( R, j, -f1 * R.IFl)
        }
        // loc+scale:<none> | <none>:loc+scale | loc:scale | scale:loc
        //  t(q) = (q - mu_R)*s_D/s_R + mu_D
        else if ((aD==(1,1,0) & aR==(0,0,0)) | (aD==(0,0,0) & aR==(1,1,0)) |
                 (aD==(1,0,0) & aR==(0,1,0)) | (aD==(0,1,0) & aR==(1,0,0))) {
            _rd_IF(G0, j, f0 * D.s/R.s            * IFq)
            _rd_IF( D, j, f1                      * D.IFl)
            _rd_IF( D, j, f1 * (q-R.l)/R.s        * D.IFs)
            _rd_IF( R, j, f1 * -D.s/R.s           * R.IFl)
            _rd_IF( R, j, f1 * -(q-R.l)*D.s/R.s^2 * R.IFs)
        }
        // scale:<none>
        //  t(q) = (q - mu_D)*s_D/s_R + mu_D
        else if (aD==(0,1,0) & aR==(0,0,0)) {
            _rd_IF(G0, j, f0 * D.s/R.s            * IFq)
            _rd_IF( D, j, f1 * (1 - D.s/R.s)      * D.IFl)
            _rd_IF( D, j, f1 * (q-D.l)/R.s        * D.IFs)
            _rd_IF( R, j, f1 * -(q-D.l)*D.s/R.s^2 * R.IFs)
        }
        // <none>:scale
        //  t(q) = (q - mu_R)*s_D/s_R + mu_R
        else if (aD==(0,0,0) & aR==(0,1,0)) {
            _rd_IF(G0, j, f0 * D.s/R.s            * IFq)
            _rd_IF( D, j, f1 * (q-R.l)/R.s        * D.IFs)
            _rd_IF( R, j, f1 * (1 - D.s/R.s)      * R.IFl)
            _rd_IF( R, j, f1 * -(q-R.l)*D.s/R.s^2 * R.IFs)
        }
        // shape:<none> | <none>:shape
        //  t(q) = (q - mu_D)*s_R/s_D + mu_R
        else if ((aD==(0,0,1) & aR==(0,0,0)) | (aD==(0,0,0) & aR==(0,0,1))) {
            _rd_IF(G0, j, f0 * R.s/D.s            * IFq)
            _rd_IF( D, j, f1 * -R.s/D.s           * D.IFl)
            _rd_IF( D, j, f1 * -(q-D.l)*R.s/D.s^2 * D.IFs)
            _rd_IF( R, j, f1                      * R.IFl)
            _rd_IF( R, j, f1 * (q-D.l)/D.s        * R.IFs)
        }
        // scale+shape:<none> | <none>:scale+shape
        //  t(q) = q - mu_D + mu_R
        else if ((aD==(0,1,1) & aR==(0,0,0)) | (aD==(0,0,0) & aR==(0,1,1))) {
            _rd_IF(G0, j,  f0 * IFq)
            _rd_IF( D, j, -f1 * D.IFl)
            _rd_IF( R, j,  f1 * R.IFl)
        }
        // loc+shape:<none> | shape:loc
        //  t(q) = (q - mu_R)*s_R/s_D + mu_R
        else if ((aD==(1,0,1) & aR==(0,0,0)) | (aD==(0,0,1) & aR==(1,0,0))) {
            _rd_IF(G0, j, f0 * R.s/D.s            * IFq)
            _rd_IF( D, j, f1 * -(q-R.l)*R.s/D.s^2 * D.IFs)
            _rd_IF( R, j, f1 * (1 - R.s/D.s)      * R.IFl)
            _rd_IF( R, j, f1 * (q-R.l)/D.s        * R.IFs)
         }
        // <none>:loc+shape | loc:shape
        //  t(q) = (q - mu_D)*s_R/s_D + mu_D
        else if ((aD==(0,0,0) & aR==(1,0,1)) | (aD==(1,0,0) & aR==(0,0,1))) {
            _rd_IF(G0, j, f0 * R.s/D.s            * IFq)
            _rd_IF( D, j, f1 * (1 - R.s/D.s)      * D.IFl)
            _rd_IF( D, j, f1 * -(q-D.l)*R.s/D.s^2 * D.IFs)
            _rd_IF( R, j, f1 * (q-D.l)/D.s        * R.IFs)
        }
        // shape:scale
        //  t(q) = q + (mu_R-mu_D)*s_R/s_D
        else if (aD==(0,0,1) & aR==(0,1,0)) {
            _rd_IF(G0, j, f0                        * IFq)
            _rd_IF( D, j, f1 * -R.s/D.s             * D.IFl)
            _rd_IF( D, j, f1 * -(R.l-D.l)*R.s/D.s^2 * D.IFs)
            _rd_IF( R, j, f1 * R.s/D.s              * R.IFl)
            _rd_IF( R, j, f1 * (R.l-D.l)/D.s        * R.IFs)
        }
        // scale:shape
        //  t(q) = q + (mu_R-mu_D)*s_D/s_R
        else if (aD==(0,1,0) & aR==(0,0,1)) {
            _rd_IF(G0, j, f0                        * IFq)
            _rd_IF( D, j, f1 * -D.s/R.s             * D.IFl)
            _rd_IF( D, j, f1 * (R.l-D.l)/R.s        * D.IFs)
            _rd_IF( R, j, f1 * D.s/R.s              * R.IFl)
            _rd_IF( R, j, f1 * -(R.l-D.l)*D.s/R.s^2 * R.IFs)
        }
        else _error(3498) // not reached
    }
}

/* Divergence estimation ---------------------------------------------------*/

void rd_DIV(`SS' bnm, `SS' BWnm)
{
    `Int'  k
    `RS'   h, h0
    `RC'   b, b0, p
    `RM'   IF, IF0
    `SR'   stats
    `SM'   cstripe
    `Data' data
    pragma unset data
    pragma unset h
    pragma unset h0
    pragma unset IF
    pragma unset IF0
    
    // stats
    stats = tokens(st_local("stats"))
    k = length(stats)
    cstripe = stats'
    // estimate main model
    b = _rd_DIV(data, stats, h)
    if (st_local("compare")=="") {
        // return results if no alternate model
        if (st_local("over")!="") {
            if (k==1) cstripe = ("", st_local("o")+"."+st_local("over"))
            else      cstripe = (J(length(b),1,st_local("o")), cstripe)
        }
        else cstripe = J(length(b),1,""), cstripe
        if (st_local("pdf")!="") st_matrix(BWnm, h) 
        st_matrix(bnm, b')
        st_matrixcolstripe(bnm, cstripe)
        if (data.nose==0) _rd_IF_store(data, length(b))
        return
    }
    // get IFs from main model
    if (data.nose==0) _rd_DIV_IF(data, length(b), IF)
    // estimate alternate model
    _rd_DIV_lswap(0)
    b0 = _rd_DIV(data=`DATA'(), stats, h0)
    if (data.nose==0) _rd_DIV_IF(data, length(b0), IF0)
    _rd_DIV_lswap(1)
    // return results
    if (st_local("pdf")!="") st_matrix(BWnm, (h, h0)) 
    b = b \ b0 \ b-b0
    if (st_local("over")!="") {
        if (k==1) cstripe = (J(3,1,st_local("o")), ("main" \ "alternate" \ "difference"))
        else      cstripe = (st_local("o"):+"_":+cstripe, J(k,1,"main")) \ 
                            (st_local("o"):+"_":+cstripe, J(k,1,"alternate")) \ 
                            (st_local("o"):+"_":+cstripe, J(k,1,"difference"))
    }
    else {
        if (k==1) cstripe = (J(3,1,""), ("main" \ "alternate" \ "difference"))
        else      cstripe = (cstripe, J(k,1,"main")) \ 
                            (cstripe, J(k,1,"alternate")) \ 
                            (cstripe, J(k,1,"difference"))
    }
    p = mm_seq(1, k*3, k) // reorder results
    if (k==2) p = p \ 1:+p
    if (k==3) p = p \ 1:+p \ 2:+p
    st_matrix(bnm, b[p]')
    st_matrixcolstripe(bnm, cstripe[p,])
    if (data.nose==0) {
        IF = IF, IF0, IF-IF0
        st_store(., _rd_IF_store_idx(length(b), 0), st_local("touse"), IF[,p])
    }
}

`RM' _rd_DIV_IF(`Data' data, `Int' n, `RM' IF)
{   // generates the IFs of the model and reads them back in
    `SS'    tmp
    `IntR'  idx
    
    tmp = st_local("IFs")
    st_local("IFs", "")
    _rd_IF_store(data, n)
    idx = st_varindex(tokens(st_local("IFs")))
    IF = st_data(., idx, st_local("touse"))
    st_dropvar(idx)
    st_local("IFs", tmp)
}

`RC' _rd_DIV(`Data' data, `SR' stats, `RS' h)
{
    `Int'  n, j, k
    `Int'  entropy, chi2, tvd
    `RC'   b, d, at, atx, p0, p1
    `RM'   IFD, IFR
    pragma unset p1
    pragma unset p0
    
    // prepare data
    rd_getdata(data)
    
    // compute divergence
    k = length(stats)
    entropy = sum((stats:=="entropy") :* (1..k))    // stats assumed unique
    chi2    = sum((stats:=="chi2")    :* (1..k))
    tvd     = sum((stats:=="tvd")     :* (1..k))
    b = J(k, 1, .)
    // - continuous data
    if (st_local("discrete")=="" & st_local("categorical")=="") {
        n = strtoreal(st_local("n"))
        if (st_local("pdf")!="") {
            at = (0::n-1)/n :+ .5/n // regular grid with half step on each side
            d = _rd_PDFc(n, data, at, h)  // also fills in IFs
        }
        else {
            d = _rd_HIST(n, data)  // also fills in IFs
        }
        if (entropy) b[entropy] = sum(d :* ln(d)) / n 
        if (chi2)    b[chi2]    = sum((d:-1):^2) / n
        if (tvd)     b[tvd]     = sum(abs(d:-1))/2  / n
        if (data.nose==0) {
            IFD = J(rows(data.D.y), k, 0)
            IFR = J(rows(data.R.y), k, 0)
            if (entropy) {
                IFD[,entropy] = rowsum((1:+ln(d'))/n :* data.D.IF)
                IFR[,entropy] = rowsum((1:+ln(d'))/n :* data.R.IF)
            }
            if (chi2) {
                IFD[,chi2] = rowsum((d':-1)*(2/n) :* data.D.IF)
                IFR[,chi2] = rowsum((d':-1)*(2/n) :* data.R.IF)
            }
            if (tvd) {
                IFD[,tvd] = rowsum(sign(d':-1)/(2*n) :* data.D.IF)
                IFR[,tvd] = rowsum(sign(d':-1)/(2*n) :* data.R.IF)
            }
        }
    }
    // - discrete/categorical data
    else {
        atx = _rd_get_atx(data, "") // get levels of data
        at  = _mm_relrank(*data.Y0, data.G0->w, atx)
        n   = rows(atx)
        _rd_PDFd(p1, p0, *data.Y1, data.G1->w, atx, at)
        if (anyof(p0,0)) _rd_PDFd_rm(p1, p0, at, atx, n)
        if (entropy) b[entropy] = sum(p1 :* (ln(p1) - ln(p0)))
        if (chi2)    b[chi2]    = sum((p1 - p0):^2 :/ p0)
        if (tvd)     b[tvd]     = sum(abs(p1 - p0))/2
        if (data.nose==0) {
            _rd_IF_init(data, n)
            for (j=1;j<=n;j++) {
                _rd_IF(data.D, j, ((data.D.y:==atx[j]) :- p1[j])/data.D.W)
                _rd_IF(data.R, j, ((data.R.y:==atx[j]) :- p0[j])/data.R.W)
            }
            IFD = J(rows(data.D.y), k, 0)
            IFR = J(rows(data.R.y), k, 0)
            if (entropy) {
                IFD[,entropy] = rowsum( (1:+ln(p1):-ln(p0))' :* data.D.IF)
                IFR[,entropy] = rowsum(-(p1:/p0)' :* data.R.IF)
            }
            if (chi2) {
                IFD[,chi2] = rowsum(2*(p1:/p0:-1)' :* data.D.IF)
                IFR[,chi2] = rowsum((1:-(p1:/p0):^2)' :* data.R.IF)
            }
            if (tvd) {
                IFD[,tvd] = rowsum( sign(p1:-p0)'/2 :* data.D.IF)
                IFR[,tvd] = rowsum(-sign(p1:-p0)'/2 :* data.R.IF)
            }
        }
    }
    
    // return results
    if (data.nose==0) {
        data.D.IF = IFD
        data.R.IF = IFR
    }
    return(b)
}

void _rd_DIV_lswap(`Bool' revert)
{
    `Int' i
    `SR'  lnm
    
    lnm = ("bwidth", "adj1", "adj0", "adjmean", "adjsd", "adjlog", "adjmult",
        "bal_varlist", "bal_method", "bal_noisily", "bal_ref", "bal_contrast", 
        "bal_opts", "bal_ebopts", "BAL_WVAR")
    if (revert) {
        for (i=length(lnm); i; i--) {
            st_local(lnm[i], st_local("tmp_"+lnm[i]))
            st_local("tmp_"+lnm[i], "")
        }
        return
    }
    for (i=length(lnm); i; i--) {
        st_local("tmp_"+lnm[i], st_local(lnm[i]))
        st_local(lnm[i], st_local("c_"+lnm[i]))
    }
}

/* MRP estimation -----------------------------------------------------------*/

void rd_MRP(`SS' bnm)
{
    `RR'   b
    `RC'   d
    `Data' data
    
    // prepare data
    rd_getdata(data)
    rd_relrank(data)
    
    // estimation
    d = data.ranks :- 0.5
    b = J(1,3,.)
    b[2] = 8 * mean( -d :* (d:<0), data.G1->w) - 1 // LRP
    b[3] = 8 * mean(  d :* (d:>0), data.G1->w) - 1 // URP
    b[1] = (b[2] + b[3]) / 2  // MRP (= 4 * mean(abs(d), data.G1->w) - 1)
    
    // return results
    st_matrix(bnm, b)
    st_matrixcolstripe(bnm, (J(3,1,""), tokens("MRP LRP URP")'))
    
    // compute influence functions
    if (data.nose==0) {
        _rd_MRP_IF(b, d, data)
        _rd_IF_store(data, 3)
    }
}

void _rd_MRP_IF(`RR' b, `RC' d, `Data' data)
{
    `RC' fR
    `RM' cdf
    
    // initialize IFs
    _rd_IF_init(data, 3)

    // CDF of reference distribution at unique values
    cdf = _mm_ecdf2(*data.Y0, data.G0->w)[,2]
    cdf = cdf, mm_diff(0 \ cdf) // CDF steps in second column
    
    // compute reference distribution densities
    if (data.adj.link==1) {
        fR = _rd_kdens(data.G0->lny, data.G0->w, data.wtype>=2, 
            __rd_IF_x_to_y(data.G1->lny, data.D, data.R, 0))
    }
    else {
        fR = _rd_kdens(data.G0->y, data.G0->w, data.wtype>=2, 
            __rd_IF_x_to_y(data.G1->y, data.D, data.R, data.adj.link==2))
    }
    
    // MRP
    _rd_IF(data.D, 1, 1/data.G1->W * ((4*abs(d) :- 1) :- b[1]))
    _rd_PDFc_IF2(data, 1, cdf, fR, 4/data.G1->W * (data.G1->w :* sign(d)))
    
    // LRP
    _rd_IF(data.D, 2, 1/data.G1->W * (((8 * -d :* (d:<0)) :- 1) :- b[2]))
    _rd_PDFc_IF2(data, 2, cdf, fR, -8/data.G1->W * data.G1->w :* (d:<0))
    
    // URP
    _rd_IF(data.D, 3, 1/data.G1->W * (((8 *  d :* (d:>0)) :- 1) :- b[3]))
    _rd_PDFc_IF2(data, 3, cdf, fR, 8/data.G1->W * data.G1->w :* (d:>0))
}

/* Summary statistics estimation --------------------------------------------*/

void rd_SUM(`SS' bnm)
{
    `Int'  i, j, n
    `SR'   stats
    `SS'   stat
    `RC'   p, q
    `RR'   b
    `Data' data
    
    // prepare data
    rd_getdata(data)
    rd_relrank(data)
    
    // collect quantiles
    stats = tokens(st_local("stats"))
    n = cols(stats)
    j = 0
    for (i=1;i<=n;i++) {
        stat = stats[i]
        if (substr(stat,1,1)=="p") ++j
        else if (stat=="median")   ++j
        else if (stat=="iqr")   {; ++j; ++j; }
    }
    p = J(j, 1, 0)
    j = 0
    for (i=1;i<=n;i++) {
        stat = stats[i]
        if (substr(stat,1,1)=="p") p[++j] = strtoreal(substr(stat,2,.))/100
        else if (stat=="median")   p[++j] = .5
        else if (stat=="iqr")   {; p[++j] = .75; p[++j] = .25; }
    }
    q = _mm_quantile(data.ranks, data.G1->w, p, 2)
    
    // fill in stats
    stats = tokens(st_local("stats"))
    n = cols(stats)
    b = J(1,n,.)
    j = 0
    for (i=1;i<=n;i++) {
        stat = stats[i]
        if (substr(stat,1,1)=="p") b[i] = q[++j]
        else if (stat=="median")   b[i] = q[++j]
        else if (stat=="iqr")   {; b[i] = q[++j]; b[i] = b[i] - q[++j]; }
        else if (stat=="mean")     b[i] = mean(data.ranks, data.G1->w)
        else if (stat=="variance") b[i] = rd_SUM_var(data)
        else if (stat=="sd")       b[i] = sqrt(rd_SUM_var(data))
        else _error(3498) // not reached
    }

    // return results
    if (st_local("GENERATE")!="") {
        st_store(., st_local("GENERATE"), data.G1->touse, data.ranks[data.G1->p])
    }
    st_matrix(bnm, b)
    st_matrixcolstripe(bnm, (J(n,1,""), stats'))
    
    // compute influence functions
    if (data.nose==0) {
        _rd_SUM_IF(data, stats, q, p, b, n)
        _rd_IF_store(data, n)
    }
}

real scalar rd_SUM_var(`Data' data)
{
    if (rows(data.ranks)==1) return(0)
    if (data.wtype>=2) return(variance(data.ranks, data.G1->w) * 
        (data.G1->W - 1) / (data.G1->W - data.G1->W / data.G1->N))
    return(variance(data.ranks, data.G1->w))
}

void _rd_SUM_IF(`Data' data, `SR' stats, `RC' q, `RC' p, `RR' b, `Int' n)
{
    `Int'  i, j
    `SS'   stat
    `RC'   fR
    `RM'   cdf
    `RM'   IFD, IFR
    pragma unset cdf
    pragma unset fR
    
    // compute IFs for quantiles
    if (rows(p)) {
        _rd_CDF_IF(q, p, rows(p), data, data.R, data.D, *data.G0, *data.G1)
        IFD = data.D.IF
        IFR = data.R.IF
    }
    
    // initialize IFs
    _rd_IF_init(data, n)
    
    // compute IFs
    j = 0
    for (i=1;i<=n;i++) {
        stat = stats[i]
        if (substr(stat,1,1)=="p") _rd_SUM_q_IF(data, i, IFD, IFR, ++j)
        else if (stat=="median")   _rd_SUM_q_IF(data, i, IFD, IFR, ++j)
        else if (stat=="iqr")      _rd_SUM_iqr_IF(data, i, IFD, IFR, ++j)
        else if (stat=="mean")     _rd_SUM_mean_IF(data, i, b[i], cdf, fR)
        else if (stat=="variance") _rd_SUM_var_IF(data, i, b[i], cdf, fR)
        else if (stat=="sd")       _rd_SUM_sd_IF(data, i, b[i], cdf, fR)
        else _error(3498) // not reached
    }
}

void _rd_SUM_q_IF(`Data' data, `Int' i, `RM' IFD, `RM' IFR, `Int' j)
{
    data.D.IF[,i] = IFD[,j]
    data.R.IF[,i] = IFR[,j]
}

void _rd_SUM_iqr_IF(`Data' data, `Int' i, `RM' IFD, `RM' IFR, `Int' j)
{
    data.D.IF[,i] = IFD[,j] - IFD[,j+1]
    data.R.IF[,i] = IFR[,j] - IFR[,j+1]
    ++j
}

void _rd_SUM_IF_fR(`Data' data, `RM' cdf, `RC' fR)
{
    if (rows(cdf)) return // already computed
    
    // CDF of reference distribution at unique values
    cdf = _mm_ecdf2(*data.Y0, data.G0->w)[,2]
    cdf = cdf, mm_diff(0 \ cdf) // CDF steps in second column
    
    // compute reference distribution densities if adjust()
    if (data.adj.link==1) {
        fR = _rd_kdens(data.G0->lny, data.G0->w, data.wtype>=2, 
            __rd_IF_x_to_y(data.G1->lny, data.D, data.R, 0))
    }
    else if (data.adj.true) {
        fR = _rd_kdens(data.G0->y, data.G0->w, data.wtype>=2, 
            __rd_IF_x_to_y(data.G1->y, data.D, data.R, data.adj.link==2))
    }
}

void _rd_SUM_mean_IF(`Data' data, `Int' i, `RS' b, `RM' cdf, `RC' fR)
{
    _rd_SUM_IF_fR(data, cdf, fR)
    // first part of IF
    _rd_IF(*data.G1, i, (data.ranks :- b) / data.G1->W)
    // second part of IF
    _rd_PDFc_IF2(data, i, cdf, fR, (data.wtype ? data.G1->w/data.G1->W : 
        J(data.G1->N, 1, data.G1->w/data.G1->W))) 
}

void _rd_SUM_var_IF(`Data' data, `Int' i, `RS' b, `RM' cdf, `RC' fR)
{
    `RC' rdev
    `RS' c
    
    _rd_SUM_IF_fR(data, cdf, fR)
    // first part of IF
    rdev = data.ranks :- mean(data.ranks, data.G1->w)
    if (data.wtype>=2) c = 1 / (1 - 1/data.G1->N)
    else               c = 1 / (1 - 1/data.G1->W)
    if (rows(data.ranks)==1) c = 1
    _rd_IF(*data.G1, i, (c:*rdev:^2 :- b) / data.G1->W)
    // second part of IF
    _rd_PDFc_IF2(data, i, cdf, fR, 2/data.G1->W * c :* data.G1->w :* rdev)
}

void _rd_SUM_sd_IF(`Data' data, `Int' i, `RS' b, `RM' cdf, `RC' fR)
{
    _rd_SUM_var_IF(data, i, b^2, cdf, fR)
    data.D.IF[,i] = data.D.IF[,i] / (2*b)
    data.R.IF[,i] = data.R.IF[,i] / (2*b)
}

end
exit

