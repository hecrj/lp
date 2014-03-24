compres grammar and interpreter
 
By default the interpreter will be compiled in interactive mode.
This means that it's going to read the input stream line by line,
executing the instructions and saving data in memory like a real
interpreter.
If you define the constant NO_INTERACTIVE at compile time
(with -DNO_INTERACTIVE) the resulting interpreter will read the
entire input and then it will only evaluate the nodes where some
information needs to be printed, using the provided findASTCompraDef
function.
 
I've decided to do an interactive version because I think that trying to
imitate real interpreters is more interesting and challenging.
 
I've also added another way to define lists:
{ product: amount, ... }

I was not sure whether the 'PRODUCTES' instruction should print the number
of products or print the list contents, therefore I added an instruction
to print the contents of a list: 'MOSTRAR'.

Additionally, you can define the constant DEBUG at compile time and the
resulting interpreter will show the garbage that is collected.

Hector Ramon Jimenez - LP - Spring 2014 - FIB (UPC)
