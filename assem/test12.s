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

L17:

mov $0, %eax

mov $1, %eax

mov %eax, %ebx

L13:

mov $10, %eax

cmp %eax, %ebx

jle L14

jmp L3

L3:

push %eax

push %ebp

call g

add $8, %esp

jmp L16

L14:

mov $L5, %eax

push %eax

call print

add $4, %esp

mov $10, %eax

cmp %eax, %ebx

je L6

jmp L7

L7:

mov $0, %eax

L8:

mov $0, %ecx

cmp %ecx, %eax

jne L12

jmp L11

L11:

mov $1, %eax

add %ebx, %eax

mov %eax, %ebx

jmp L13

L6:

mov $1, %eax

jmp L8

L12:

mov $L10, %eax

push %eax

call print

add $4, %esp

jmp L11

L16:

 
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

L19:

mov $L2, %eax

push %eax

call print

add $4, %esp

mov 12(%ebp), %eax

jmp L18

L18:

 
add $0, %esp

pop %ebp

pop %ebx

pop %esp

pop %esi

pop %edi

pop %ebp

ret


