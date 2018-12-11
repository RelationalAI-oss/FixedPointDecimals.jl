using BitIntegers

Base.widen(::Type{Int128}) = Int256

# =========================================
# ======= Add Int256 division =============
# =========================================
a = 323423242342; b = 231
FloatType = Float64; T=typeof(a)
b = typeof(a)(b)
nothing
function iterative_float_div(FloatType::Type{<:Real}, a::T, b::T) where {T<:Integer}
    #a,b = widen(a), widen(b)
    #T=typeof(a)
    @assert b != 0
    bfloat = convert(FloatType, b)
    r = one(FloatType)/bfloat

    residual = a
    result = zero(a)

    while (abs(residual) >= abs(b))
        residual_float = convert(FloatType, residual)
        c = residual_float * r
        residual_approx = convert(T, trunc(c))
        result += residual_approx
        approx = residual_approx * b
        residual -= approx
    end

    return result
end
iterative_float_div(FloatType::Type{<:Real}, a::T,b::U) where{T<:Integer, U<:Integer} = iterative_float_div(FloatType, promote(a,b)...)

# Converted from Float64(::Int128) here:
# https://github.com/JuliaLang/julia/blob/master/base/float.jl#L94
# Note that the implementations are almost identical, save for how many bits to shift.
function Float64(x::Int256)
    x == 0 && return 0.0
    s = ((x >>> (256-64)) % UInt64) & 0x8000_0000_0000_0000 # sign bit
    x = abs(x) % UInt256
    n = 256-leading_zeros(x) # ndigits0z(x,2)
    if n <= 53  # Mantissa (significand) has 53 bytes.
        y = ((x % UInt64) << (53-n)) & 0x000f_ffff_ffff_ffff
    else
        y = ((x >> (n-54)) % UInt64) & 0x001f_ffff_ffff_ffff # keep 1 extra bit
        y = (y+1)>>1 # round, ties up (extra leading bit in case of next exponent)
        y &= ~UInt64(trailing_zeros(x) == (n-54)) # fix last bit to round to even
    end
    d = ((n+1022) % UInt64) << 52
    reinterpret(Float64, s | d + y)
end
Float64(Int128(-2))
Float64(Int256(-2))
Float64(Int128(2))
Float64(Int256(2))
Float64(typemax(Int128))
Float64(Int256(typemax(Int128)))
Float64(Int256(typemax(Int128))*2)
Float64(-Int256(typemax(Int128))*2)
Float64(typemax(Int256)) ≈ typemax(Int256)


# ----------------------------

# Copied exactly from the definition for unsafe_trunc(::UInt128, ::Float64):
# https://github.com/JuliaLang/julia/blob/master/base/float.jl#L315
# The implementations are identical, since the size of the output is irrelevant in the calculations.
function Base.unsafe_trunc(::Type{UInt256}, x::Float64)
    xu = reinterpret(UInt64,x)
    k = Int(xu >> 52) & 0x07ff - 1075
    xu = (xu & 0x000f_ffff_ffff_ffff) | 0x0010_0000_0000_0000
    if k <= 0
        UInt256(xu >> -k)
    else
        UInt256(xu) << k
    end
end
function Base.unsafe_trunc(::Type{Int256}, x::Float64)
    copysign(unsafe_trunc(UInt256,x) % Int256, x)
end

for Ti in (UInt256, Int256)
    for Tf in (Float32, Float64)
        if Ti <: Unsigned || sizeof(Ti) < sizeof(Tf)
            @eval begin
                function (::Type{$Ti})(x::$Tf)
                    if ($(Tf(typemin(Ti))) <= x <= $(Tf(typemax(Ti)))) && (round(x, RoundToZero) == x)
                        return unsafe_trunc($Ti,x)
                    else
                        throw(InexactError($(Expr(:quote,Ti.name.name)), $Ti, x))
                    end
                end
            end
        else
            @eval begin
                function (::Type{$Ti})(x::$Tf)
                    if ($(Tf(typemin(Ti))) <= x < $(Tf(typemax(Ti)))) && (round(x, RoundToZero) == x)
                        return unsafe_trunc($Ti,x)
                    else
                        throw(InexactError($(Expr(:quote,Ti.name.name)), $Ti, x))
                    end
                end
            end
        end
    end
end


#using BenchmarkTools
#for T in (Int8,Int16,Int32,Int64,Int128,Int256)
#    a = abs(rand(T))
#    b = T(trunc(sqrt(a)))
#    println(T)
#    v = @btime iterative_float_div(Float64,$a,$b)
#    v = @btime $a ÷ $b
#    @assert a÷b == v
#end

# Now that we've shown they're equivalent, let's use iterative_float_div for div:
Base.div(a::Int256,b::Int256) = iterative_float_div(Float64, a, b)

# =========================================
# ======= Add Int256 rem =============
# =========================================

#  need `rem` to fix all the BigInts in FixedDecimal{Int128} multiplication!
# TODO: is this good enough? We could probably do faster?
function float_rem(FloatType::Type{<:Real}, a::T, b::T) where {T<:Integer}
    a - ((a ÷ b) * b)
end

Base.rem(a::Int256, b::Int256) = float_rem(Float64, a, b)

# Custom divrem to prevent doing the `div` twice (it's expensive).
function Base.divrem(a::Int256, b::Int256)
    d = div(a,b)
    r = a - (d * b)
    return d,r
end
# Custom fldmod to prevent doing the `div` twice (it's expensive).
function Base.fldmod(a::Int256, b::Int256)
    d = fld(a,b)
    r = a - (d * b)
    return d,r
end
Base.fldmod(a::Int256, b::Integer) = fldmod(promote(a,b)...)
Base.fldmod(a::Integer, b::Int256) = fldmod(promote(a,b)...)

# Prevent unneeded rem call here:
function Base.isodd(a::Int256)
    return Base.isodd(a % Int8)  # only depends on the final bit! :)
end
