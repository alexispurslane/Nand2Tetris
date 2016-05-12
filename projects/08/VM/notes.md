# THINGS TO DO
1) Look at the implementations of `pop-to`, `push-to`, `bool-op`, `stack-op`,
   and `single-op`, and make sure they match the algorithems below...
   If they already do and I can't find the bug, cry.

# OPERATIONS
##  FOR PUSH

* Update current stack location with segment[index]
* Increment the stack pointer

##  FOR POP

* Deincriment the stack pointer
* Put the current stack value in segment[index]

##  FOR ALL TWO OP OPERATIONS

* Deincrement the stack pointer
* Put the current stack value in register D
* Deincrement the stack pointer
* Operate on the current memory and register D
* Put the result in the current memory location.
* Increment the stack pointer

##  FOR ONE OP OPERATIONS
* Deincrement the stack pointer
* Put the result of the operator on memory in memory.
* Increment the stack pointer

## FOR GOTO
* Jump to label

## FOR IF-GOTO
* Jump to label if the top value of the stack isn't zero.
* Deincrement the stack pointer
