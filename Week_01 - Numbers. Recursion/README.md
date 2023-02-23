# Task 1

Define functions that return:

- the smaller of two whole numbers:
  - add a test with negative numbers;
  - using `if-else`;
  - using guards;
  - using a built-in function.
- the last digit of a number
- the quotient of the division of two whole numbers
- the quotient and remainder of the division of two whole numbers
- a whole number without its last digit
- the quotient and remainder of the division of two real numbers
- the quotient of the division of two real numbers
- the average of two whole numbers
- the number rounded to the second digit after the `.`

Test cases:

```haskell
print $ min 5 6 == 5
print $ minIf 15 60 == 15
print $ minIf 60 15 == 15
print $ minGuard 15 60 == 15
print $ minGuard 60 15 == 15
print $ minBuiltIn 60 15 == 15

print $ lastDigit 154 == 4

print $ quotientWhole 64 2 == 32
print $ divWhole 154 17 == 9.058823529411764

print $ removeLastDigit 154 == 15    

print $ divReal 154.451 10.01 == 15.42967032967033
print $ quotientReal 154.21 17.17 == 8

print $ avgWhole 5 1542 == 773.5

print $ roundTwoDig 3.1413465345321 == 3.14
print $ roundTwoDigButWithMagic 3.1413465345321 == 3.14 -- partial function application and composition (defining a function at functional level)
```

# Task 2

Define a predicates that check:

- whether two numbers are not equal:
  - **in one line without** using if-else;
  - using guards.
- whether a whole number *x* is between two whole numbers - *start* and *finish* **in one line without** using if-else.

Test cases:

```haskell
print $ areNotEqualOneLine 5 2 == True
print $ areNotEqualOneLine 5 5 == False

print $ areNotEqualGuards 5 2 == True
print $ areNotEqualGuards 5 5 == False

print $ inside 1 5 4 == True -- start = 1, finish = 5, x = 4
print $ inside 5 1 4 == True
print $ inside 10 50 20 == True
print $ inside 10 50 1 == False
```

# Task 3

Define a recursive and an iterative function for calculating the factorial of a non-negative number.

- solve with a linearly recursive process;
- solve using a linearly iterative process.

Test cases:

```haskell
print $ factRec 11 == 39916800
--print $ factRec (-11) -- error: x was negative
print $ factIter 11 == 39916800
--print $ factIter (-11) -- error: x was negative
```

# Task 4

Define a recursive and an iterative function for calculating the number at index *i* in the Fibonacci sequence (0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, ...).

> **Implementation detail**: The input will always be valid, i.e. greater than -1.

Test cases:

```haskell
print $ fibRec 11 == 89
print $ fibIter 11 == 89
print $ fibIter 110 == 43566776258854844738105
```

# For home

# Task 1

Define two predicates that return whether a number is even:

- using if-else;
- using guards. <- The preferred way!

Test cases:

```haskell
print $ isEvenIf 2 == "Yes"
print $ isEvenIf 15452 == "Yes"
print $ isEvenIf 321 == "No"

print $ isEvenGuards 2 == "Yes"
print $ isEvenGuards 15452 == "Yes"
print $ isEvenGuards 321 == "No"
```

# Task 2

Define two functions that return the sum of the cubes of two **whole** numbers:

- using a power operator;
- **without** using a power operator.

Test cases:

```haskell
print $ sumCubesPow 5 1 == 126
print $ sumCubesPow 10 50 == 126000

print $ sumCubesNoPow 5 1 == 126
print $ sumCubesNoPow 10 50 == 126000
```

# Task 3

Define a function that returns the average of the sum of the squares of two **whole** numbers.

Test cases:

```haskell
print $ sqAvg 5 0 == 12.5
print $ sqAvg 10 13 == 134.5
```

# Task 4

Define a function for calculating the GCD of two whole numbers.

> **Note**: Normally, you would use the built-in function `gcd`.

- solve using guards;
- solve using pattern matching; <- The preferred way!

Recap:

```text
The Euclidean Algorithm for finding GCD(x,y) is as follows:
If x = 0 then GCD(x,y)=y.
If y = 0 then GCD(x,y)=x.
Write x in quotient remainder form (x = A⋅y + R). Find GCD(y,R) (since GCD(x,y)=GCD(y,R)).
```

Test cases:

```haskell
print $ myGcdG 5 13 == 1
print $ myGcdG 13 1235 == 13

print $ myGcdPM 5 13 == 1
print $ myGcdPM 13 1235 == 13
```

# Task 5

Define two predicates that check whether a year is a leap year.

The first should be called *isLeapYearOneLine*. It should implement *Method I* using **boolean logical operators** <- the preferred way.

The second should be called *isLeapYearGuards*. It should implement *Method II* using **guards**.

Recap:

```text
Method I: A year is leap year if it is a multiple of 400 or it is a multiple of 4 but not of 100.

Method II:
    if year is divisible by 400 then is_leap_year
    else if year is divisible by 100 then not_leap_year
    else if year is divisible by 4 then is_leap_year
    else not_leap_year 
```

Test cases:

```haskell
print $ isLeapYearOneLine 2020 == True
print $ isLeapYearOneLine 1988 == True
print $ isLeapYearOneLine 1600 == True
print $ isLeapYearOneLine 2400 == True
print $ isLeapYearOneLine 2023 == False
print $ isLeapYearOneLine 1700 == False
print $ isLeapYearOneLine 1800 == False
print $ isLeapYearOneLine 2100 == False

print $ isLeapYearGuards 2020 == True
print $ isLeapYearGuards 1988 == True
print $ isLeapYearGuards 1600 == True
print $ isLeapYearGuards 2400 == True
print $ isLeapYearGuards 2023 == False
print $ isLeapYearGuards 1700 == False
print $ isLeapYearGuards 1800 == False
print $ isLeapYearGuards 2100 == False
```

# Task 6

John has a backpack. With it he can carry `k` kilograms. An item from the supermarket weighs `w` kilograms.

Define a predicate that accepts three numbers - `c` (number of products), `k` and `w` and returns whether John is capable of buying all the products in one trip to the supermarket.

> **Implementation detail**: Use guards!

Test cases:

```haskell
print $ canCarry 5 15 3 == "Yes"
print $ canCarry 1 5 4 == "Yes"
print $ canCarry 13 25 2 == "No"
print $ canCarry 24 104.44 21.12 == "No"
print $ canCarry 51 34.75 19.852 == "No"
print $ canCarry 42 95.11 0.51 == "Yes"

--print $ canCarry (-13) 25 2 -- error: The number of products was negative
--print $ canCarry 13 (-25) 2 -- error: John's hosting capacity was negative
--print $ canCarry 13 25 (-2) -- error: The weight of a product was negative
```

# Task 7

Each **day** a plant is growing by *upSpeed* meters. Each **night** that plants height decreases by *downSpeed* meters due to the lack of sun light. Initially, plant is 0 meters tall. We plant the seed at the beginning of a day. We want to know when the height of the plant will reach a certain level.

Test cases:

```haskell
print $ growingPlant 5 2 5 == 1
print $ growingPlant 5 2 6 == 2
print $ growingPlant 10 9 4 == 1
print $ growingPlant 100 10 910 == 10 -- upSpeed=100, downSpeed=10, desiredHeight=910
```

Explanations:

- for `upSpeed = 100`, `downSpeed = 10` and `desiredHeight = 910`, the output should be `10`.

```text
After day 1   --> 100
After night 1 --> 90
After day 2   --> 190
After night 2 --> 180
After day 3   --> 280
After night 3 --> 270
After day 4   --> 370
After night 4 --> 360
After day 5   --> 460
After night 5 --> 450
After day 6   --> 550
After night 6 --> 540
After day 7   --> 640
After night 7 --> 630
After day 8   --> 730
After night 8 --> 720
After day 9   --> 820
After night 9 --> 810
After day 10  --> 910
```

- for `upSpeed = 10`, `downSpeed = 9` and `desiredHeight = 4`, the output should be `1`.

```text
Because the plant reaches the desired height at day 1 (10 meters).

After day 1 --> 10
```

# Task 8

A snail crawls up a column. During the day it crawls up some distance. During the night it sleeps, so it slides down for some distance (less than it crawls up during the day).

Your function accepts three arguments:

- The height of the column (meters);
- The distance that the snail crawls during the day (meters);
- The distance that the snail slides down during the night (meters).

Calculate the number of days the snail will need to reach the top of the column.

Test cases:

```haskell
print $ snail 3 2 1 == 2
print $ snail 10 3 1 == 5
print $ snail 10 3 2 == 8
print $ snail 100 20 5 == 7
print $ snail 5 10 3 == 1
```

# Task 9

Define a function that reverses a **non-negative** number by implementing a **linearly iterative process**.

Test cases:

```haskell
print $ rev 1 == 1
print $ rev 123 == 321
print $ rev 987654321 == 123456789
```

# Task 10

Define a function that takes as inputs your potential grades on homeworks and exams and outputs your final grade for this subject **rounded** to the second digit after the `.`. If the final grade is less than `2`, return `2`.

Here is the formula:

```text
O = 1/2 TK + 1/4 IT + 1/4 IZ
```

where

```text
TK = 1/4 D + 3/8 KT + 3/8 KZ

and

D is the average grade from 3 homeworks
KT is the average grade from 2 theory exams
KZ is the average grade from 2 practice exams
IT is the grade from the theory final
IZ is the grade from the practice final
```

Additional notes:

- **You'll pass the course if: D >= 3 and IZ >= 3 and IT >= 3 and O >= 3!**
- If you get KZ >= 4,50 and **on each** practical exam you get KZ<sub>i</sub> >= 4,00, then you don't have to go to the final on the practice part. That would mean that IZ = KZ.
- If you get KT >= 4,50 and **on each** theory exam you get KT<sub>i</sub> >= 4,00, then you don't have to go to the final on the theory part. That would mean that IT = KT.
- `0` is a possible grade!

Assume the order of the inputs is the following:

`D1 D2 D3 KZ1 KZ2 KT1 KT2 IZ IT`

Test cases:

```haskell
print $ finalGrade 3 4 4 4.25 4.50 3.75 4.25 5 4.25 == 4.34
print $ finalGrade 6 6 6 4.50 5 4.50 4.75 5 4.75    == 4.95
print $ finalGrade 6 0 4 6 6 5 4.75 6 4.75          == 5.14
print $ finalGrade 4.25 0 3 2 0 0 0 0 0             == 2
print $ finalGrade 5.50 6 6 6 5.50 5.25 4 5.50 4    == 5.05
print $ finalGrade 6 6 6 5.50 5.50 4 5 5.50 5       == 5.25
print $ finalGrade 6 6 6 5.25 6 4 4 5.63 3.50       == 4.84
```
