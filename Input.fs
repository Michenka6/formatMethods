module book.Input

let factorialInput =
    "
    y := 1;
    do x > 0 -> 
        y := y * x; 
        x := x - 1 
    od"

let nthPowerof2Input =
    "
    y := 1;
    do x > 0 -> 
        y := y * 2; 
        x := x - 1 
    od"

let gcdInput =
    "
    do x != y -> 
        if  x < y   -> y := y - x 
        []  x > y   -> x := x - y 
        fi
    od"

let modulusInput =
    "
    do x >= y -> 
        x := x - y
    od"

let nthFibInput =
    "
    y := 1;
    if n<= 2 -> 
        skip
    [] n > 2 -> 
        x := 1;
        n := n - 2;
        do n > 0 -> 
            y := x + y;
            x := y - x;
            n := n - 1 
        od
    fi"

let nonDeterministicInput =
    "
    if x <= 0 ->
        y := -1 
    [] x >= 0 -> 
        y := 1 
    fi"

let stuckProgramInput =
    "
    if x < 0 ->
        y := -1 
    [] x > 0 -> 
        y := 1 
    fi"

let insertionSortInput =
    "
    i := 1;
    do i < n ->
        j := i;
        do j > 0 && A[j - 1] > A[ j ] -> 
            A[ j ] := A[j - 1] + A[ j ] ;
            A[j - 1] := A[ j ] - A[j - 1];
            A[ j ] := A[ j ] - A[j - 1];
            j := j - 1 
        od; 
        i := i + 1
    od"

let insertionSort2Input =
    "
    i := 1;
    do i < n ->
        j := i;
        do j > 0 && A[j - 1] > A[ j ] -> 
            t := A[j - 1];
            A[j - 1] := A[ j ];
            A[ j ] := t;
            j := j - 1 
        od; 
        i := i + 1
    od"

let bubbleSortInput =
    "
    i := 1;
    do i < n ->
        j := 1;
        do j < n ->
            if j > 0 && A[j - 1] > A[ j ] -> 
                A[ j ] := A[j - 1] + A[ j ];
                A[j - 1] := A[ j ] - A[j - 1];
                A[ j ] := A[ j ] - A[j - 1]
            [] !(j > 0 && A[j - 1] > A[ j ]) ->
                skip 
            fi;
            j := j + 1
        od;
        i := i + 1
    od"

let innerProductInput =
    "
    i:=0;
    j:=0;
    do i<n -> 
        if j < m -> 
            y := y + A[i] * B[j];
            j:=j+1
        [] j >= m ->
            i:=i+1;
            j:=0
        fi
    od"


let outerProductInput =
    "
    i:=0;
    j:=0;
    do i < n -> 
        do j < m ->
        C[i*m + j] := A[i] * B[j];
        j := j + 1
        od;
        i := i+1;
        j := 0
    od"

let maxInput =
    "
    𝚒𝚏 𝚡 ≥ 𝚢 →
        𝚣 ∶= 𝚡
    [] 𝚢 > 𝚡 →
        𝚣 ∶= 𝚢
    𝚏𝚒"
