⍝ prime number tester
⍝ Output: 1 = not prime number 0 = prime

maybe_prime ← 29 ⍝ Number to test

prime ← 1
:If maybe_prime = 1
    prime ← 0
:Else
    counter ← 1
    :While counter < (maybe_prime - 1)
        counter ← counter + 1
        :If (counter|maybe_prime) = 0
            prime ← 0
            counter ← maybe_prime
        :Else :End
    :EndWhile
:End

prime ⍝ Output