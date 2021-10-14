.model small
.386

data  segment
	IO_ADDRESS EQU 200H

	map0    dw  256 dup(0)
	map1    dw  256 dup(0)

	flappy  db  28			;鸟初始高度 0-60
	flag	db  0
	fengxi  db  16			;缝隙宽度
	
	score	db  -5
	rad     db  ?
	temp    db  ?
	hz_adr  db  ?
	dizhix  db  ?
	dizhiy  db  ?

	ledcode	db 3fh, 06h, 5bh, 4fh, 66h, 6dh, 7dh, 07h, 7fh, 67h
data  ends

code  segment
	assume cs:code, ds:data
start:
	mov ax, data
	mov ds, ax

	mov dx, 203h 			;8255命令口地址
	mov al, 10000010b		;控制字(0方式 A口C口输出 B口输入)
	out dx, al

	call drawoff
	call high_disp
	call low_disp
	call drawon	
	
mainlp:	call random
	mov  rad, bl
	inc  score
	cmp  score, 0
	jna  qwe
	call disp
qwe:	call zhuzi
	call jump	
	call drawbird
	call high_disp
	call low_disp
	call drawon
	call deletebird
	
	mov  cx, 5
lpp:	call repeat1
	call repeat2
	call jump
	call drawbird
	call high_disp
	call low_disp
	call drawon
	call deletebird
	loop lpp
	
	mov  cx, 10
lpp2:	call zuoyi1
	call zuoyi2
	call jump
	call drawbird
	call high_disp
	call low_disp
	call drawon
	call deletebird
	loop lpp2

	jmp mainlp
		
lp:	jmp lp

disp  proc
	
disp  endp

jump  proc
	mov  dx, 201h
	in   al, dx              ;输入列线状态
	or   al, 11110000b            ;高四位置1，低四位为列线状态
	cmp  al, 11110111B           
	je   goup
	ret
goup:	mov  flag, 3
	ret       
jump  endp

gameover proc
sss:	call  clear

	MOV   CX, 0FFFH
XX:	LOOP  XX
	jmp   sss

gameover endp

deletebird  proc
	push cx
	mov al, flappy

	cmp al, 31
	jna abb2
	
	cmp al, 35
	jnb bee2
	
	cmp al, 32
	jmp b32
	
	cmp al, 33
	jmp b33

	cmp al, 34
	jmp b34
	
	jmp enddraw
	
b32:	lea si, map1
	add si, 129
	mov al, [si]
	sub [si], 11110000B
	
	lea si, map0
	add si, 187
	mov al, [si]
	sub [si], 11110000B
	
	add si, 2
	sub [si], 10010000B
	add si, 2
	sub [si], 10010000B
	jmp enddelete

b33:	lea si, map0
	add si, 189
	mov al, [si]
	sub [si], 11110000B

	add si, 2
	sub [si], 10010000B
	
	lea si, map1
	add si, 131
	mov al, [si]
	sub [si], 11110000B
	
	sub si, 2
	sub [si], 10010000B
	jmp enddelete

b34:	lea si, map0
	add si, 191
	mov al, [si]
	sub [si], 11110000B
	
	lea si, map1
	add si, 133
	mov al, [si]
	sub [si], 11110000B
	
	sub si, 2
	sub [si], 10010000B
	sub si, 2
	sub [si], 10010000B
	jmp enddelete

abb2:	lea si, map0
	sal al, 1
	add al, 129
	mov temp, al
	add si, word ptr temp
	mov al, [si]
	sub [si], 11110000B
	
	sub si, 6
	mov al, [si]
	sub [si], 11110000B
	
	add si, 2
	sub [si], 10010000B
	add si, 2
	sub [si], 10010000B
	jmp enddelete 

bee2:	lea si, map1
	sub al, 32
	sal al, 1
	add al, 129
	mov temp, al
	add si, word ptr temp
	mov al, [si]
	sub [si], 11110000B
	
	sub si, 6
	mov al, [si]
	sub [si], 11110000B
	
	add si, 2
	sub [si], 10010000B
	add si, 2
	sub [si], 10010000B
	jmp enddraw

enddelete:pop cx
	ret
deletebird  endp

drawbird  proc
	push cx
	cmp  flag, 0
	jne  uppp
	add  flappy, 2
	jmp  norm

uppp:	sub  flag, 1
	sub  flappy, 2
	cmp  flappy, 3
	jna  go
	
norm:	mov al, flappy
	
	cmp al, 62
	ja  go

	cmp al, 31
	jna abb
	
	cmp al, 35
	jnb bee
	
	cmp al, 32
	jmp a32
	
	cmp al, 33
	jmp a33

	cmp al, 34
	jmp a34
	
	jmp enddraw
	
a32:	lea si, map1
	add si, 129
	mov al, [si]
	and al, 11110000B
	cmp al, 0
	ja  go
	add [si], 11110000B
	
	lea si, map0
	add si, 187
	mov al, [si]
	and al, 11110000B
	cmp al, 0
	ja  go
	add [si], 11110000B
	
	add si, 2
	add [si], 10010000B
	add si, 2
	add [si], 10010000B
	jmp enddraw

a33:	lea si, map0
	add si, 189
	mov al, [si]
	and al, 11110000B
	cmp al, 0
	ja  go
	add [si], 11110000B

	add si, 2
	add [si], 10010000B
	
	lea si, map1
	add si, 131
	mov al, [si]
	and al, 11110000B
	cmp al, 0
	ja  go
	add [si], 11110000B
	
	sub si, 2
	add [si], 10010000B
	jmp enddraw

a34:	lea si, map0
	add si, 191
	mov al, [si]
	and al, 11110000B
	cmp al, 0
	ja  go
	add [si], 11110000B
	
	lea si, map1
	add si, 133
	mov al, [si]
	and al, 11110000B
	cmp al, 0
	ja  go
	add [si], 11110000B
	
	sub si, 2
	add [si], 10010000B
	sub si, 2
	add [si], 10010000B
	jmp enddraw

abb:	lea si, map0
	sal al, 1
	add al, 129
	mov temp, al
	add si, word ptr temp
	mov al, [si]
	and al, 11110000B
	cmp al, 0
	ja  go
	add [si], 11110000B
	
	sub si, 6
	mov al, [si]
	and al, 11110000B
	cmp al, 0
	ja  go
	add [si], 11110000B
	
	add si, 2
	add [si], 10010000B
	add si, 2
	add [si], 10010000B
	jmp enddraw

bee:	lea si, map1
	sub al, 32
	sal al, 1
	add al, 129
	mov temp, al
	add si, word ptr temp
	mov al, [si]
	and al, 11110000B
	cmp al, 0
	ja  go
	add [si], 11110000B
	
	sub si, 6
	mov al, [si]
	and al, 11110000B
	cmp al, 0
	ja  go
	add [si], 11110000B
	
	add si, 2
	add [si], 10010000B
	add si, 2
	add [si], 10010000B
	jmp enddraw

go:	call gameover

enddraw:pop cx
	ret
drawbird  endp

zuoyi1  proc
	lea  si, map0
	mov  bx, si
	add  bx, 511
lll1:	cmp  si, bx
	ja   eed1
	sal  [si], 1
	add  si, 2
	jmp  lll1

eed1:   ret
zuoyi1  endp

zuoyi2  proc
	lea  si, map1
	mov  bx, si
	add  bx, 511
lll2:	cmp  si, bx
	ja   eed2
	sal  [si], 1
	add  si, 2
	jmp  lll2

eed2:   ret
zuoyi2  endp

repeat1 proc
	lea  si, map0
	mov  bx, si
	add  bx, 511
re1:	cmp  si, bx
	ja   edd1
	
	mov  ax, [si]
	and  ax, 1
	cmp  ax, 0 
	ja   op1
	sal  [si], 1
	add  si, 2
	jmp  re1

op1:	sal  [si], 1
	add  [si], 1
	add  si, 2
	jmp  re1

edd1:	ret
repeat1 endp

repeat2 proc
	lea  si, map1
	mov  bx, si
	add  bx, 511
re2:	cmp  si, bx
	ja   edd2
	
	mov  ax, [si]
	and  ax, 1
	cmp  ax, 0 
	ja   op2
	sal  [si], 1
	add  si, 2
	jmp  re2

op2:	sal  [si], 1
	add  [si], 1
	add  si, 2
	jmp  re2

edd2:	ret
repeat2 endp

zhuzi  proc
	push cx

	call zuoyi1
	call zuoyi2

	mov  cx, 256
	lea  si, map0
ad1:	add  [si], 1
	add  si, 2
	loop ad1

	mov  cx, 256
	lea  si, map1
ad2:	add  [si], 1
	add  si, 2
	loop ad2

	mov  cx, 224
	lea  si, map0
	add  si, 64
l1:	cmp  [si], 1
	jz   n1
bk1:	add  si, 2
	loop l1

	mov  cx, 224
	lea  si, map1
	add  si, 64
l2:	cmp  [si], 1
	jz   n2
bk2:	add  si, 2
	loop l2
	
	mov  al, rad
	add  al, 12
	mov  cl, fengxi
	
	cmp  al, 32
	ja   m1
	lea  si, map0		
	add  si, 446
	sal  al, 1
	mov  temp, al
	add  si, word ptr temp
	
m2:	sub  [si], 1
	add  si, 2
	add  al, 2
	dec  cl
	jz   ed
	cmp  al, 64
	ja   m1
	jmp  m2
	
m1:	lea  si, map1
	add  si, 448
	;mov  al, 0
	;mov  temp, al
	;add  si, word ptr temp
	
m3:	sub  [si], 1
	add  si, 2
	add  al, 2
	dec  cl
	jz   ed
	jmp  m3
	
n1:	sub [si - 64], 1
	jmp bk1

n2:	sub [si - 64], 1
	jmp bk2

ed:	pop cx
	ret
zhuzi  endp


high_disp  proc  
	mov byte ptr dizhix, 10000000B  
	mov byte ptr dizhiy, 10000000B

continue1:
	;设定GDRAM地址x命令
	MOV  AL, dizhix
	MOV  DX, IO_ADDRESS
	OUT  DX, AL
	CALL CMD_SETUP
	CALL DELAY			

	;设定GDRAM地址y命令
	MOV  AL, dizhiy
	MOV  DX, IO_ADDRESS
	OUT  DX, AL
	CALL CMD_SETUP
	CALL DELAY			

	;先送D15-D8
	lea  si, map0
	mov  ax, 0
	mov  al, dizhiy
	sub  al, 10000000B
	mov  bl, 32
	mul  bl
	mov  bl, dizhix
	sub  bl, 10000000B
	add  al, bl
	sal  ax, 1
	
	add  si, ax
	 
	MOV  ax, [si]
	push ax
	mov  al, ah
	MOV  DX,IO_ADDRESS
	OUT  DX,AL
	CALL DATA_SETUP
	CALL DELAY

	;再送D7-D0
	pop  ax
	MOV  DX,IO_ADDRESS
	OUT  DX,AL
	CALL DATA_SETUP
	CALL DELAY

	inc  byte ptr dizhix
	cmp  dizhix, 10100000B
	jnz  continue1

	inc byte ptr dizhiy
	mov byte ptr dizhix, 10000000B
	cmp dizhiy, 10001000B
	jnz continue1

	RET
high_disp  endp

low_disp  proc  
	mov byte ptr dizhix, 10000000B  
	mov byte ptr dizhiy, 10001000B

continue2:
	;设定GDRAM地址x命令
	MOV  AL, dizhix
	MOV  DX, IO_ADDRESS
	OUT  DX, AL
	CALL CMD_SETUP
	CALL DELAY			

	;设定GDRAM地址y命令
	MOV  AL, dizhiy
	MOV  DX, IO_ADDRESS
	OUT  DX, AL
	CALL CMD_SETUP
	CALL DELAY			

	;先送D15-D8
	lea  si, map1
	mov  ax, 0
	mov  al, dizhiy
	sub  al, 10001000B
	mov  bl, 32
	mul  bl
	mov  bl, dizhix
	sub  bl, 10000000B
	add  al, bl
	sal  ax, 1
	
	add  si, ax
	 
	MOV  ax, [si]
	push ax
	mov  al, ah
	MOV  DX,IO_ADDRESS
	OUT  DX,AL
	CALL DATA_SETUP
	CALL DELAY

	;再送D7-D0
	pop  ax
	MOV  DX,IO_ADDRESS
	OUT  DX,AL
	CALL DATA_SETUP
	CALL DELAY

	inc  byte ptr dizhix
	cmp  dizhix, 10100000B
	jnz  continue2

	inc byte ptr dizhiy
	mov byte ptr dizhix, 10000000B
	cmp dizhiy, 10010000B
	jnz continue2

	RET
low_disp  endp

clear  proc
	mov byte ptr dizhix, 10000000B
	mov byte ptr dizhiy, 10000000B   

ccont:
	;设定GDRAM地址x命令
	MOV  AL, dizhix
	MOV  DX, IO_ADDRESS
	OUT  DX, AL
	CALL CMD_SETUP
	CALL DELAY			

	;设定GDRAM地址y命令
	MOV  AL, dizhiy
	MOV  DX, IO_ADDRESS
	OUT  DX, AL
	CALL CMD_SETUP
	CALL DELAY			

	;先送D15-D8
	MOV  AL, 00000000B
	MOV  DX,IO_ADDRESS
	OUT  DX,AL
	CALL DATA_SETUP
	CALL DELAY

	;再送D7-D0
	MOV  AL, 00000000B
	MOV  DX,IO_ADDRESS
	OUT  DX,AL
	CALL DATA_SETUP
	CALL DELAY

	inc  byte ptr dizhix
	cmp  dizhix, 10100000B
	jnz  ccont

	inc byte ptr dizhiy
	mov byte ptr dizhix, 10000000B
	cmp dizhiy, 10010000B
	jnz ccont

	RET
clear  endp

drawoff  proc
	mov  al, 00110100B
	mov  dx, IO_ADDRESS
	out  dx, al
	call cmd_setup			;启动LCD执行命令
	ret
drawoff  endp

drawon  proc
	mov  al, 00110110B
	mov  dx, IO_ADDRESS
	out  dx, al
	call cmd_setup			;启动LCD执行命令
	ret
drawon  endp

CMD_SETUP  PROC
	CALL   DELAY 
	MOV    DX, IO_ADDRESS		;指向8255端口控制端口
	ADD    DX, 2
           
	MOV    AL, 00000000B		;PC1置0,pc0置0 （LCD I端=0，W端＝0）
	OUT    DX, AL
	CALL   DELAY     

	MOV    AL, 00000100B		;PC2置1 （LCD E端＝1）
	OUT    DX, AL
	call   DELAY
	
	MOV    AL, 00000000B		;PC2置0,（LCD E端置0）
	OUT    DX, AL
	CALL   DELAY
	RET
CMD_SETUP  ENDP

DATA_SETUP PROC
	MOV    DX,IO_ADDRESS		;指向8255控制端口
	ADD    DX,2
	MOV    AL, 00000001B		;PC1置0，PC0=1 （LCD I端=1）
	OUT    DX, AL
	CALL   DELAY

	MOV    AL, 00000101B		;PC2置1 （LCD E端＝1）
	OUT    DX, AL
	CALL   DELAY
           
	MOV    AL, 00000001B		;PC2置0,（LCD E端＝0）
	OUT    DX, AL
	CALL   DELAY
	RET
DATA_SETUP ENDP

DELAY  PROC
	PUSH   CX    
	PUSH   DX    
	MOV    CX, 0AH
X1:	LOOP   X1   
	POP    DX    
	POP    CX    
	RET
DELAY  ENDP

random proc
	push cx
	push dx
	push ax
	
	mov ax, 25
	mov dx, 41h
	out dx, ax
	in  al, dx
	mov bl, al

	pop ax
	pop dx
	pop cx
	ret
random endp

code  ends
      end start
