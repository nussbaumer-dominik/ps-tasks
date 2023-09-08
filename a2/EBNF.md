```
<prog> ::= <basic>
    | <basic> <prog>

<basic> ::= <assignment>
    | <value>
    | <EOL>

<assignment> ::= <name> `:=` <basic>

<names> ::= <name>
    | <names> ’,’ <name>
 
<values> ::= <value>
    | <values> ’,’ <value>   
    
<value> ::= <integer>
    | <name>
    | <functionCall>
    | ’[’ <values> ’]’  // list
    | <functionDefinition>
    
<functionCall> ::= <name>'{'<values>'}'
<functionDefinition> ::= ’(’<names>’)=>{’ <expr> ’}’
```
