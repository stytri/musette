## About

A reduced variant of my **oboe**[1] language inspired by, and with equivalent functionality to, **picol**[2].

Although overall size was not a determining factor, I budgeted, and achieved, **1000** lines or less. It could be made smaller, but I am happy with it as it is.

[1] https://github.com/stytri/oboe

[2] https://github.com/stytri/picol

### Why "musette" ?
https://en.wikipedia.org/wiki/Piccolo_oboe

## Description

All operators (including delimeters and brackets) are binary operators, parsing is left to right, use parenthesis to specify precedence.

### Precedence

From low to high:
* **sequence** (delimeted expressions)
* **operation** (binary operator expressions)
* **binding** (adjacent primarys invoke the **`.`** operator, albeit at higher precedence)
* **primary** (terminal-tokens and bracketed expressions)

### Comments

'**`#`**'    Comment to end of line

### Delimeters

'**`,`**'    Primarily used to delimit function arguments.

'**`;`**'    Primarily used to delimit expression statements.

'**`:`**'    Primarily used to delimit
alternate consequents of conditional expressions.

### Operators

'**`+`**'    addition

'**`-`**'    subtraction

'**`*`**'    multiplication

'**`/`**'    division

'**`%`**'    modulo

'**`|`**'    bitwise OR

'**`^`**'    bitwise XOR

'**`&`**'    bitwise AND

'**`<<`**'    bitwise left shift

'**`>>`**'    bitwise right shift

'**`||`**'    logical OR

'**`&&`**'    logical AND

'**`==`**'    equal

'**`!=`**'    not equal

'**`<`**'    less than

'**`<=`**'    less than or equal

'**`>`**'    greater than

'**`>=`**'    greater than or equal

'**`?`**'    if : left-hand-expression is the condition, right-hand-expression is the consequent(s). Automatically enters a new scope. A **`;`** delimeter in the condition indicates that the left-hand-part is intialization, and the right-hand-part is the evaluated condition.

'**`?*`**'    while : left-hand-expression is the condition, right-hand-expression is the loo body. Automatically enters a new scope. A **`;`** delimeter in the condition indicates that the left-hand-part is intialization, and the right-hand-part is the evaluated condition.

'**`?/`**'    break : left-hand-expression specifies a pre-condition, or **`_`** if there is no condition; right-hand-expression is evaluated if the pre-condition is satisfied.

'**`?.`**'    continue : left-hand-expression specifies a pre-condition, or **`_`** if there is no condition; right-hand-expression is evaluated if the pre-condition is satisfied.

'**`=>`**'    lambda : creates anonymous functions; left-hand-expression specifies a parameter list, right-hand-expression is the function body.

'**`.`**'    apply : applies one expression to another. If the left-hand-expression is an identifier it specifies a named-dunction, and the right-hand-expression is a list of function arguments. Otherwise the left and right-hand-expressions are multiplied together.

'**`=`**'    assign : creates a symbol named by the identifier in the left-hand-expression ans assigns it the value of the right-hand-expression. If the right-hand-expression is an anonymous-function then a named-function is created.

'**`<:`**'    output : outputs the string representation of its arguments; output is automatically terminated with a new-line. Arguments are not delimeted; if an expression is given as an argument, it should be enclosed in either **`{`** **`}`**, or **`[`** **`]`** brackets.

'**`:>`**'    input : reads a line from stdin, and tokenizes it. Integer and string tokens only are acceptable; they are assigned in order to the identifiers given as arguments. As per output, arguments are separated by spaces.

'**\`**'_identifier_'**\`**'    User defined named operator, where _identifier_ is the name of a function that _should_ take 2 aruments.

### Brackets

'**`(`**' '**`)`**'    Primarily used to indicate a sub-expression; is not representated in the execution tree.

'**`[`**' '**`]`**'    Primarily used to indicate a sub-expression; is represented in the execution tree.

'**`{`**' '**`}`**'    Primarily used to indicate a sub-expression in a new scope; is represented in the execution tree.

### Terminals

_integer_    decimal, binary(**`0b`** prefix), hexadecimal(**`0x`** prefix) and octal(**`0`** prefix).

_string_    Between 2 double-quote (**`"`**) characters. Escape (and control) code representation included.

_identifier_    **C**-style identifier rules apply.

_bracketed-expression_   An expression, in brackets.

### Identifiers

'**`VERSION`**'    Gives the program's version number as an _integer_.

'**`_`**'    Represents **Z**ero **E**mpty **N**ull (ZEN), so that we do not use too many parenthesis -  this is not lisp ;)

## Functions

Functions are created by assignment of a lambda expression.

A function with two arguments can be used as a binary operator by encapsulating the function name with a backtick:
```
and=(a,b)=>(a!=0)*(b!=0);
x=3;
y=6;
(x`and`y)? <: "true";
```
