⍝ testing while loop statements
counter ← 0
other_counter ← 0
:While counter < 100
    other_counter ← 0
    :While other_counter < 10
        counter ← counter + 1
        other_counter ← other_counter + 1
    :EndWhile    
:EndWhile
counter