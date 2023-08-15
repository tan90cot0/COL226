# COL266 Assignment 4
# Aryan Dua
# VMC Machine Evaluator

### File Contents
> * 'AST.sml' - Main file.
> * 'test.sml' - contains 3 test cases

### How to Run
1. Traverse to the required directory.
2. Open the SML interactive environment
3. Type the commands -- 
	use "ast.sml";
	use "test.sml";
	execute cmdseq;

4. The output will be displayed on the terminal.

### Auxiliary Functions and Data

> I have used a lot of auxiliary functions, like printArr, edit_stack, edit_stack_while, edit_stack_ite, translate, etc. They are self-explanatory. I have used the names of the functions as specified in the assignment pdf.

> I have also created an auxiliary datatype - datatype     TreeNode =Leaf of string | Node of string * TreeNode list

### Other Design Decisions
1. I have declared errors in the case of boolean operations with integers(type checking) and also in the case of division by 0. For index out of range, the program automatically throws an error.
2. I have declared the inital memory size to be 1000.
3. The rules have been updated according to the next available token. 

### Other Implementation Decisions
1. I have manually entered the data into my new datatype so that evaluation could become easier. It is quite straightforward to convert the previous assignments AST to this assignments Tree. I have used a similar but a little different implementation to evaluate WHILE and ITE. I go through the stack of tockens and I merge the sequence of commands for either of them(2 for ITE and 1 for WHILE) into 1 command and store the to_string form in an array. Then while evaluating I can access it. It takes care of nested Whiles, nested Ifs, nested ifs within whiles, etc.

### Acknowledgements

1. All code is written purely by me alone.