using FixedPointDecimals
import FixedPointDecimals: FD, value

using Test

numdigits = 9
FD64 = FD{Int64, numdigits}

bigfloat_min = BigFloat(typemin(FD64))/2
bigfloat_max = BigFloat(typemax(FD64))/2
bigfloat_range = range(bigfloat_min,length=100_000,stop=bigfloat_max)


@testset "Fuzz tests" begin
    N = 10_000

    @testset "+" begin
        for _ in 1:N
            a,b = round.(rand(bigfloat_range, 2), digits=numdigits)
            @test FD64(a + b) == FD64(a) + FD64(b)
        end
    end
    @testset "-" begin
        for _ in 1:N
            a,b = round.(rand(bigfloat_range, 2), digits=numdigits)
            @test FD64(a - b) == FD64(a) - FD64(b)
        end
    end
    @testset "*" begin
        for _ in 1:N
            # Choose random numbers a,b to prevent overflow:
            # Choose a random number of digits for a, then a random number of digits for b
            # such at digits(a) + digits(b) <= (numdigits-1)
            adigits = rand(1:numdigits-1)
            bdigits = numdigits-adigits-1
            rand_ndigits(ndigits) = rand(range(big"1.0", stop=big"10.0"^(ndigits), length=10000)) * (rand()>0.5 ? -1 : 1)
            a,b = round.(rand_ndigits.((adigits,bdigits)), digits=numdigits)
            @test FD64(a * b) == FD64(a) * FD64(b)
        end
    end
    @testset "/" begin
        for _ in 1:N
            a,b = round.(rand(bigfloat_range/10^(numdigits/2), 2), digits=numdigits)
            @test FD64(a / b) == FD64(a) / FD64(b)
        end
    end
    @testset "÷" begin
        for _ in 1:N
            a,b = round.(rand(bigfloat_range, 2), digits=numdigits)
            @test FD64(a ÷ b) == FD64(a) ÷ FD64(b)
        end
    end
end
