@256
D=A
@SP
M=D
@3000
D=A
@THIS
M=D
@4000
D=A
@THAT
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

@Sys.init$RETURN1
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
@Sys.main
0;JMP
(Sys.init$RETURN1)
@6
D=A
@0
AD=D+A
@R13
M=D
@SP
AM=M-1
D=M
@R13
A=M
M=D
(Sys.init$LOOP)
@Sys.init$LOOP
0;JMP
(Sys.main)

@123
D=A
@SP
A=M
M=D
@SP
M=M+1

@Sys.main$RETURN7
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
@Sys.add12
0;JMP
(Sys.main$RETURN7)
@5
D=A
@0
AD=D+A
@R13
M=D
@SP
AM=M-1
D=M
@R13
A=M
M=D
@246
D=A
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
(Sys.add12)
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
@SP
A=M
M=D
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

@12
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
