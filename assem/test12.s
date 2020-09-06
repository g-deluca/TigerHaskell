.data
L10:
.long 14
.string "llegamos a 10
"
L5:
.long 12
.string "inside loop
"
L2:
.long 15
.string "got out of loop"

.text
.globl _tigermain

.type _tigermain, @function

_tigermain:

push %ebp

mov %esp, %ebp

push %edi

push %esi

push %esp

push %ebx

push %ebp

sub $0, %esp

L16:

mov $0, %eax

mov $1, %eax

mov %eax, %edi

L13:

mov $10, %eax

cmp %eax, %edi

jle L14

jmp L3

L3:

push %ebx

push %ecx

push %eax

push %ebp

call g

add $8, %esp

pop %ebx

pop %ecx

jmp L15

L14:

push %ebx

push %ecx

mov $L5, %eax

push %eax

call print

add $4, %esp

pop %ebx

pop %ecx

mov $10, %eax

cmp %eax, %edi

je L6

jmp L7

L7:

mov $0, %eax

L8:

mov $0, %edx

cmp %edx, %eax

jne L12

jmp L11

L11:

mov $1, %eax

add %edi, %eax

mov %eax, %edi

jmp L13

L6:

mov $1, %eax

jmp L8

L12:

push %ebx

push %ecx

mov $L10, %eax

push %eax

call print

add $4, %esp

pop %ebx

pop %ecx

jmp L11

L15:

 
add $0, %esp

pop %ebp

pop %ebx

pop %esp

pop %esi

pop %edi

pop %ebp

ret

.globl g

.type g, @function

g:

push %ebp

mov %esp, %ebp

push %edi

push %esi

push %esp

push %ebx

push %ebp

sub $0, %esp

L18:

push %ebx

push %ecx

mov $L2, %eax

push %eax

call print

add $4, %esp

pop %ebx

pop %ecx

mov 12(%ebp), %eax

jmp L17

L17:

 
add $0, %esp

pop %ebp

pop %ebx

pop %esp

pop %esi

pop %edi

pop %ebp

ret


