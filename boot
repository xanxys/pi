# echo program
_stdio_getbyte<(cont)
!cont>(char)
    _stdio_putbyte<(char,cont)
    cont>()
        _stdio_putbyte<([10],cont)
        cont>()
            _stdio_getbyte<(cont)

# test boolean function
new(cont)
    false<(cont)
    cont>(b)
        pa>()
            print_a<(kill)
        pb>()
            print_b<(kill)
        if<(b,pa,pb)

# test util.
!kill>(k)

# "a\n"
!print_a>(k)
    new(cont)
        _stdio_putbyte<([97],cont) 
        cont>()
            _stdio_putbyte<([10],k)

# "b\n"
!print_b>(k)
    new(cont)
        _stdio_putbyte<([98],cont) 
        cont>()
            _stdio_putbyte<([10],k)




## standard data structure library (w/ CPS protocol)
# list function (cons v maybe list)

# boolean
!true>(k)
    new(b)
        b>(then,else)
            then<()
        k<(b)

!false>(k)
    new(b)
        b>(then,else)
            else<()
        k<(b)

!if>(cond,then,else)
    cond<(then,else)


# maybe
!just>(x,k)
    new(m)
        m>(j,n)
            j<(x)
        k<(m)

!nothing>(k)
    new(m)
        m>(j,n)
            n<()
        k<(m)

!maybe>(m,cont_j,cont_n)
    m<(cont_j,cont_n)


# cons
!cons>(car,cdr,cont)
    new(t)
        t<(car,cdr)
        cont<(t)

!car>(cc,cont)
    cc>(car,cdr)
        cont<(car)

!cdr>(cc,cont)
    cc>(car,cdr)
        cont<(cdr)

