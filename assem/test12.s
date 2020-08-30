.data
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

L11:

mov $0, %eax

mov $0, %ebx

mov $0, %ebx

mov %ebx, %ecx

mov $100, %ebx

L7:

cmp %ebx, %ecx

jle L8

jmp L3

L3:

push %eax

push %ebp

call g

add $8, %esp

jmp L10

L8:

mov $1, %edi

add %edi, %eax

mov $1, %edi

add %edi, %ecx

jmp L7

L10:

 
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

sub $4, %esp

L13:

mov $L2, %eax

push %eax

call print

add $4, %esp

mov $12, %ecx

mov %ebp, %eax

add %ecx, %eax

mov (%eax), %eax

jmp L12

L12:

 
add $4, %esp

pop %ebp

pop %ebx

pop %esp

pop %esi

pop %edi

pop %ebp

ret


