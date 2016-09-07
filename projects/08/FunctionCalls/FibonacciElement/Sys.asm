@256
D=A
@SP
M=D

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

(Sys.init)

@4
D=A
@SP
A=M
M=D
@SP
M=M+1

@Sys.init$RETURN2
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
(Sys.init$RETURN2)
(Sys.init$WHILE)
@Sys.init$WHILE
0;JMP

(END)
@END
0;JMP
