@VM$outer$RETURN0
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1


@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1


@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1


@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1


@SP
D=M
@0
D=D-A
@5
D=D-A
@ARG
M=D
@SP
D=M
@LCL
M=D
@Sys.init
0;JMP
(VM$outer$RETURN0)

@256
D=A
@SP
M=D

(Main.fibonacci)

@0
D=A
@ARG
A=D+M
D=M
@SP
A=M
M=D
@SP
M=M+1

@2
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
M=M-1

A=M
D=M
@SP
M=M-1

A=M
M=M-D
D=M
@TRUE3
D;JLT
@FALSE3
0;JMP
(TRUE3)
@SP
A=M
M=-1
@NEXT3
0;JMP
(FALSE3)
@SP
A=M
M=0
(NEXT3)
@SP
M=M+1

@SP
AM=M-1
D=M
@Main.fibonacci$IF_TRUE
D;JNE
@Main.fibonacci$IF_FALSE
0;JMP
(Main.fibonacci$IF_TRUE)
@0
D=A
@ARG
A=D+M
D=M
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@R5
M=D
@5
D=A
@R5
A=M
A=A-D
D=M
@R6
M=D

@SP
M=M-1

@SP
A=M
D=M
@ARG
A=M
M=D
@ARG
D=M+1
@SP
M=D
@1
D=A
@R5
A=M
A=A-D
D=M
@THAT
M=D

@2
D=A
@R5
A=M
A=A-D
D=M
@THIS
M=D

@3
D=A
@R5
A=M
A=A-D
D=M
@ARG
M=D

@4
D=A
@R5
A=M
A=A-D
D=M
@LCL
M=D

@R6
A=M
0;JMP
(VM$outer$IF_FALSE)
@0
D=A
@ARG
A=D+M
D=M
@SP
A=M
M=D
@SP
M=M+1

@2
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
AM=M-1
D=M
@SP
AM=M-1
M=M-D
@SP
M=M+1

@VM$outer$RETURN13
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1


@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1


@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1


@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1


@SP
D=M
@1
D=D-A
@5
D=D-A
@ARG
M=D
@SP
D=M
@LCL
M=D
@Main.fibonacci
0;JMP
(VM$outer$RETURN13)
@0
D=A
@ARG
A=D+M
D=M
@SP
A=M
M=D
@SP
M=M+1

@1
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
AM=M-1
D=M
@SP
AM=M-1
M=M-D
@SP
M=M+1

@VM$outer$RETURN17
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1


@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1


@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1


@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1


@SP
D=M
@1
D=D-A
@5
D=D-A
@ARG
M=D
@SP
D=M
@LCL
M=D
@Main.fibonacci
0;JMP
(VM$outer$RETURN17)
@SP
AM=M-1
D=M
@SP
AM=M-1
M=D+M
@SP
M=M+1

@LCL
D=M
@R5
M=D
@5
D=A
@R5
A=M
A=A-D
D=M
@R6
M=D

@SP
M=M-1

@SP
A=M
D=M
@ARG
A=M
M=D
@ARG
D=M+1
@SP
M=D
@1
D=A
@R5
A=M
A=A-D
D=M
@THAT
M=D

@2
D=A
@R5
A=M
A=A-D
D=M
@THIS
M=D

@3
D=A
@R5
A=M
A=A-D
D=M
@ARG
M=D

@4
D=A
@R5
A=M
A=A-D
D=M
@LCL
M=D

@R6
A=M
0;JMP

(END)
@END
0;JMP
