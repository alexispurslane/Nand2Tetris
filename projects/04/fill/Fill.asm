// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input. 
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel. When no key is pressed, the
// program clears the screen, i.e. writes "white" in every pixel.

// Put your code here.
(LOOP)
	@SCREEN
	D=A
	@POINTER
	M=D
	@24576
	D=M
	@BLACK
	D;JNE
	@CLEAR
	0;JMP
	@LOOP
	0;JMP
(CLEAR)
	@POINTER
	D=M
	@24575
	M=A
	D=M-D
	@LOOP
	D;JEQ      // IF D == 24576 THEN GOTO LOOP
	@POINTER
	A=M
	M=0       // MEM[POINTER] = 0
	@POINTER
	M=M+1      // POINTER = POINTER + 1
	@CLEAR
	0;JMP      // GOTO CLEAR
(BLACK)
	@POINTER
	D=M
	@24575
	M=A
	D=M-D
	@LOOP
	D;JEQ      // IF D == 24576 THEN GOTO LOOP
	@POINTER
	A=M
	M=-1       // MEM[POINTER] = -1
	@POINTER
	M=M+1      // POINTER = POINTER + 1
	@BLACK
	0;JMP      // GOTO BLACK
	
