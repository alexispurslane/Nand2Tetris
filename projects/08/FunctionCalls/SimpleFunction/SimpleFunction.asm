(SimpleFunction.SimpleFunction.test)
@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1


@0
D=A
@LCL
A=D+M
D=M
@SP
A=M
M=D
@SP
M=M+1

@1
D=A
@LCL
A=D+M
D=M
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
M=D+M
@SP
M=M+1

@SP
M=M-1

A=M
M=!M
@SP
M=M+1

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

@SP
AM=M-1
D=M
@SP
AM=M-1
M=D+M
@SP
M=M+1

@1
D=A
@ARG
A=D+M
D=M
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

@LCL
D=M
@R11
M=D
@5
D=A
@R11
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
@R11
A=M
A=A-D
D=M
@THAT
M=D

@2
D=A
@R11
A=M
A=A-D
D=M
@THIS
M=D

@3
D=A
@R11
A=M
A=A-D
D=M
@ARG
M=D

@4
D=A
@R11
A=M
A=A-D
D=M
@LCL
M=D

@R11
A=M
0;JMP

(END)
@END
0;JMP
