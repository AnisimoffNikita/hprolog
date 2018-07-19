ints(0).
ints(X) :- ints(Y), X is Y + 1.

perfect_number(X) :- ints(X), Y is X - 1, calculatesum_divisors_till(Sum, X, Y), Sum = X.

calculatesum_divisors_till(0, _, 0).
calculatesum_divisors_till(Sum, NumberToDivide, Till) :- Till > 0,
                Rem is NumberToDivide mod Till,  Rem = 0,  Ts is Till - 1, 
                calculatesum_divisors_till(SumPrev, NumberToDivide, Ts),
	        Sum is SumPrev + Till.

calculatesum_divisors_till(Sum, NumberToDivide, Till) :- Till > 0, 
                Rem is NumberToDivide mod Till, Rem > 0, Ts is Till - 1, 
                calculatesum_divisors_till(Sum, NumberToDivide, Ts).
