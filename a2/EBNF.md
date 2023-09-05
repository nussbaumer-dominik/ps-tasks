```
<expr> ::= <basic>
    | <expr> <basic>

<basic> ::= <value>
    | <assignment>
    | <EOL>

<assignment> ::= <name> `:=` <expr><EOL>

<names> ::= <name>
    | <names> ’,’ <name>
 
<values> ::= <value>
    | <values> ’,’ <value>   
<value> ::= <integer>
    | <name>
    | <functionCall>
    | ’[’ <values> ’]’
    | <functionDefinition>
    
<functionCall> ::= <name>'{'<values>'}'
<functionDefinition> ::= ’(’<names>’)=>{’ <expr> ’}’
```