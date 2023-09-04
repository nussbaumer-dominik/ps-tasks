```
<expr> ::= <basic>
    | <expr> <basic>

<basic> ::= <value>
    | <assignment>

<assignment> ::= <name> `:=` <expr>

<names> ::= <name>
    | <names> ’,’ <name>
 
<values> ::= <value>
    | <values> ’,’ <value>   
<value> ::= <integer>
    | <name>
    | ’[’ <values> ’]’
    | <functionCall>
    | <functionDefinition>
    
<functionCall> ::= <name>'{'<values>'}'
<functionDefinition> ::= ’(’<names>’)=>{’ <expr> ’}’
```