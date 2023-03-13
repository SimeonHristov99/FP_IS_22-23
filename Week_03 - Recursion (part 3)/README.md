# Task 1

A narcissistic number is a number which is the sum of its own digits, each raised to the power of the number of digits. Define a predicate that checks whether a given whole number is a narcissistic number.

> **Implementation detail**: Use only linearly recursive procedures.

For example, take 153 (3 digits):

    1^3 + 5^3 + 3^3 = 1 + 125 + 27 = 153

and 1634 (4 digits):

    1^4 + 6^4 + 3^4 + 4^4 = 1 + 1296 + 81 + 256 = 1634

Test cases:

```haskell
print $ isNarcissistic 7 == True
print $ isNarcissistic 12 == False
print $ isNarcissistic 370 == True
print $ isNarcissistic 371 == True
print $ isNarcissistic 1634 == True
```

# Task 2

Define a function that accepts two whole numbers `x` and `n` and calculates the following sum **via a linearly iterative process**:

    1 + x + x^2 + x^3 + ... + x^n

Test cases:

```haskell
print $ calculateSum 5 0 == 1
print $ calculateSum 5 1 == 6
print $ calculateSum 10 1 == 11
print $ calculateSum 1 11 == 12
print $ calculateSum 2 11 == 4095
```

# Task 3

Write a function that finds the maximum digit in a number.

Test cases:

```haskell
print $ findMax 55345 == 5
print $ findMax 14752 == 7
print $ findMax 329450 == 9
print $ findMax 9521 == 9
```

# Task 4

Define a function `sumNumbers start finish` that returns the sum of all numbers in the interval `[start, finish]` whose digits are ordered in descending order **via a linearly recursive process**.

Test cases:

```haskell
print $ sumNumbers 1 9 == 45
print $ sumNumbers 199 203 == 200
print $ sumNumbers 219 225 == 663
print $ sumNumbers 225 219 == 663
```

# Task 5

Define a predicate that accepts two non-negative inputs - `x` and `y` and checks whether `x` is a sub-number of `y`.

Test cases:

```haskell
print $ subNum 123 5123783 == True -- x = 123, y = 5123783
print $ subNum 0 0 == True
print $ subNum 10 101 == True
print $ subNum 101 101 == True
print $ subNum 10 0 == False
print $ subNum 1253 5123783 == False
print $ subNum 12 0 == False
```

# Task 6

A digital root is the recursive sum of all the digits in a number. Given `n`, take the sum of the digits of `n`. If that value has more than one digit, continue reducing in this way until a single-digit number is produced. This is only applicable to the natural numbers.

Test cases:

```haskell
print $ digitalRoot 16 == 7
-- => 1 + 6
-- => 7
print $ digitalRoot 942 == 6
-- => 9 + 4 + 2
-- => 15 ...
-- => 1 + 5
-- => 6
print $ digitalRoot 132189 == 6
print $ digitalRoot 493193 == 2
```

# Task 7

Define the function sine, that accepts a whole number and a real number (representing radians) and returns the `n`-th partial sum of `sin(x)`.

Reference the formula for writing `sin(x)` around the origin:

![Alt text](pictures/Task5.png?raw=true "Task5")

Test cases:

```haskell
print $ mySin 100 1 == 0.8414709848078965 -- n = 100, x = 1
print $ mySin 100 0.5 == 0.479425538604203
```

# For home

# Task 1

Define a function that removes **the first** occurrence of a digit in a number by going from right to left.

Test cases:

```haskell
print $ removeFistOccurrence 110 1 == 10
print $ removeFistOccurrence 15365 5 == 1536
print $ removeFistOccurrence 15360 0 == 1536
print $ removeFistOccurrence 15300 0 == 1530
print $ removeFistOccurrence 15365 1 == 5365
print $ removeFistOccurrence 35365 3 == 3565
print $ removeFistOccurrence 1212 1 == 122
print $ removeFistOccurrence 1212 2 == 121
print $ removeFistOccurrence (removeFistOccurrence 1212 1) 1 == 22
```

# Task 2

Define a function that sorts a number in descending order.

Test cases:

```haskell
print $ sortN 1714 == 7411
print $ sortN 123450 == 543210
print $ sortN 123405 == 543210
print $ sortN 123045 == 543210
print $ sortN 120345 == 543210
print $ sortN 102345 == 543210
print $ sortN 8910 == 9810
print $ sortN 321 == 321
print $ sortN 29210 == 92210
print $ sortN 1230 == 3210
print $ sortN 55345 == 55543
print $ sortN 14752 == 75421
print $ sortN 329450 == 954320
print $ sortN 9125 == 9521
```

# Task 3

Define a function that accepts two real numbers and calculates the `n`-th partial sum from the following sequence:

![Alt text](pictures/sequence.png?raw=true "sequence")

Test cases:

```haskell
-- you may get slightly different results eg. -1.047619047619100 on test 4 <- not a problem
print $ calcSeriesSum 1 0 == -2.0 -- x = 1, n = 0
print $ calcSeriesSum 1 1 == -0.6666666666666667
print $ calcSeriesSum 1 2 == -1.2000000000000002
print $ calcSeriesSum 1 3 == -1.047619047619048
print $ calcSeriesSum 1 4 == -1.0814814814814817
print $ calcSeriesSum 1 5 == -1.0753246753246755
print $ calcSeriesSum 1 6 == -1.0762718762718764
```

# Task 4

Define a function `sumDivisibleNumbers start finish k` that returns the sum of all numbers from the interval [`start`, `finish`] whose digits sum up to a number that is evenly divisible by `k`.

> **Implementation detail**: Create a linearly recursive process.

Test cases:

```haskell
print $ sumDivisibleNumbers 0 10 5 == 5
print $ sumDivisibleNumbers 0 100 5 == 990
print $ sumDivisibleNumbers 100 0 5 == 990
```

# Task 5

A number - `x`, is a pentagonal number if we can plot `x` poits in the form of a pentagon on a plain.

For example:

![Alt text](pictures/pentagon.png?raw=true "pentagon")

Define a function that accepts a natural number - `n`, and returns the `n`-th pentagonal number.

> **Implementation detail**: Create a linearly iterative process.

Test cases:

```haskell
print $ p 1 == 1
print $ p 2 == 5
print $ p 3 == 12
print $ p 4 == 22
print $ p 5 == 35
print $ p 6 == 51
```
