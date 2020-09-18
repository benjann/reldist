*! version 1.0.0  01sep2020  Ben Jann

program reldist_svyr, eclass properties(svyr)
    version 12
    _parse comma lhs 0 : 0
    syntax [, svydensityopts(str) * ]
    reldist `lhs', `options'
    tempname b V
    mat `b' = e(b)
    mat `V' = I(`=colsof(`b')')
    mata: rd_svylbl_b()
    ereturn repost b=`b' V=`V', resize
    eret local cmd "reldist_svyr"
    eret local svydensityopts `"`svydensityopts'"'
end

version 12
mata:
mata set matastrict on

void rd_svylbl_b()
{
    string matrix cstripe
    
    cstripe = st_matrixcolstripe(st_local("b"))
    cstripe[,1] = cstripe[,1] :+ "@" :+ cstripe[,2]
    cstripe[,2] = J(rows(cstripe), 1, "_cons")
    st_matrixcolstripe(st_local("b"), cstripe)
}

end
exit

