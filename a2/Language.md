Each line is either an assignment or just a value or a comment (or empty or a paranthese)
```
    <name> := <value>
    <value> # will be printed
    # Comment
```
Where `<name>` consists of only letters and `<value>` can be 
    a constant like e.g. 1, -6, 0 or
    a list
    another variable name or
    a function call or
    a function definition

List variables are defined as follows.
```
    <name> := [<value>, <value>, <value>]
```
Nested lists are not allowed, so all of these values have to evaluate to an integer

Functions can be defined as follows 
```
    # No input parameters
    () => {
        # More code
    }
    
    # Input parameters
    (first, second) => {
        # More code
    }
    
    # and also named (basically stored in a variable)
    named := () => {
        # More code
    }
```

and called like this
```
    # No input parameters
    empty{}
    
    # Input parameters
    parametherized { 1, 2 }
```

or both in one line
```
    # No input parameters
    () => {
        # More code
    }{} # here we need to explizitly pass enoty arguments to call function and not just define it
    
    # Input parameters
    (first, second) => {
        # More code
    } {1, 2}
```

There are a variety of predefined functions:
```
   # Input always has to evaluate to int:
   plus {a, b}
   minus{a, b}
   mult{a, b}
   div{a, b}  # a / b
   modulo{a, b}  # a % b
   print{a}
   
   eq{1, 2} ==> 0
   gt
   lt
   or
   and{1, 1} ==> 1
   not
   cond{logic, expr, expr}
   
    
   # Input always has to evaluate to List of int and an function taking an int as input
  each{list, function}
   
   #or with anonymous function
   each{a, (nr) => {
        # More code
    }}
```

Here is an example programm that calculates the average of a list

```
   list := [1, 1, 1, 2, 2, 3, 3, 4, 5, 6, 7, 8, 9]
   
   sum := 0
   len := 0
   
   each{list, (nr)=>{
        plus{sum, nr}
        plus{len, 1}
   }}
   
   calculate := (nr)=>{
        plus{sum, nr}
        plus{len, 1}
   }
   each{list, calculate}
   
   avg := div{sum, len}
   
   print{avg}
```
